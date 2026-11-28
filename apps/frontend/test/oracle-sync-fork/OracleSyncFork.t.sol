// SPDX-License-Identifier: AGPL-3.0
pragma solidity 0.8.39;

interface Vm {
    function envString(string calldata) external returns (string memory);
    function createSelectFork(string calldata) external returns (uint256);
    function warp(uint256) external;
    function etch(address, bytes calldata) external;
    function prank(address) external;
    function deal(address, uint256) external;
}
struct Price { int64 price; uint64 conf; int32 expo; uint256 publishTime; }
struct Feed { bytes32 id; Price price; Price emaPrice; }
struct Snapshot { uint256 price; uint256 markPrice; uint64 publishTime; uint256 updateFee; uint256 maxStaleness; bool closeOnly; bool oracleFrozen; bool isFadWindow; }
interface Pyth { function getPriceUnsafe(bytes32) external view returns (Price memory); }
interface Oracle {
    function pyth() external view returns (address);
    function pythFeedIds(uint256) external view returns (bytes32);
    function getLatestPrice(uint8) external view returns (Snapshot memory);
    function getAcceptedBasketMark() external view returns (bool, uint256, uint64);
    function getAcceptedFeedPublishTimes() external view returns (uint256[] memory);
    function isOracleFrozen() external view returns (bool);
}
interface Router { function pletherOracle() external view returns (address); function engine() external view returns (address); function updateMarkPrice(bytes[] calldata) external payable; }
interface Engine { function updateMarkPrice(uint256,uint64) external; function lastMarkTime() external view returns (uint64); function lastMarkPrice() external view returns (uint256); }

// Only Pyth verification is replaced. Current v2 Oracle/Router/Engine bytecode
// executes unchanged on a local fork; no transaction is broadcast.
contract SeparatePythState {
    mapping(bytes32 => Price) public prices;
    uint64 public parsedTime;
    uint64 public storedTime;
    function configure(bytes32 id, Price calldata price, uint64 parsed, uint64 stored) external {
        prices[id] = price; parsedTime = parsed; storedTime = stored;
    }
    function setParsedTime(uint64 timestamp) external { parsedTime = timestamp; }
    function getPriceUnsafe(bytes32 id) external view returns (Price memory p) { p = prices[id]; p.publishTime = storedTime; }
    function getUpdateFee(bytes[] calldata) external pure returns (uint256) { return 1; }
    function parsePriceFeedUpdates(bytes[] calldata, bytes32[] calldata ids, uint64 min, uint64 max)
        external payable returns (Feed[] memory result) {
        require(msg.value == 1 && min <= parsedTime && parsedTime <= max, 'invalid parse funding/window');
        result = new Feed[](ids.length);
        for (uint256 i; i < ids.length; ++i) {
            Price memory p = prices[ids[i]]; p.publishTime = parsedTime;
            result[i] = Feed(ids[i], p, p);
        }
        // Live parsing does not update stored feeds. Core v2 retains the parsed
        // basket internally, so a stored-feed health check cannot observe it.
    }
}

contract OracleSyncForkTest {
    Vm constant vm = Vm(address(uint160(uint256(keccak256('hevm cheat code')))));
    Router constant router = Router(address(bytes20(hex'B4c7548ba764e22107964637e31558af724c1c3E')));

    function test_V2RefreshValidatesAcceptedBasketWhilePythStorageStaysOld() public {
        vm.createSelectFork(vm.envString('ARB_SEPOLIA_RPC_URL'));
        Oracle oracle = Oracle(router.pletherOracle());
        Engine engine = Engine(router.engine());
        require(address(oracle) == address(bytes20(hex'3BAc571AF4f9F6535696F82460EFE56A348C91B2')), 'unexpected v2 oracle');
        require(address(engine) == address(bytes20(hex'a3C43242F4E0a7C9a21Bd7dE48376D2f0779CaA6')), 'unexpected v2 engine');
        require(address(oracle).codehash == hex'3d4209085377ed55a9abb72b9ea85b4a5236401877e422c8a949771a5b5b73d6', 'oracle runtime changed');
        require(address(router).codehash == hex'a46e0b1b93d3d2bf95330517b9504e447edc7d27d6d7cf15000b45dd67df33db', 'router runtime changed');
        address pyth = oracle.pyth();
        bytes32[] memory ids = new bytes32[](6);
        Price[] memory prices = new Price[](6);
        for (uint256 i; i < 6; ++i) {
            ids[i] = oracle.pythFeedIds(i);
            prices[i] = Pyth(pyth).getPriceUnsafe(ids[i]);
            prices[i].conf = 0; // Isolate basket state/ordering from confidence policy.
        }
        uint64 tick = uint64(block.timestamp + 60);
        vm.warp(tick);
        while (oracle.isOracleFrozen()) { tick += 86400; vm.warp(tick); }
        SeparatePythState implementation = new SeparatePythState();
        vm.etch(pyth, address(implementation).code);
        for (uint256 i; i < 6; ++i) SeparatePythState(pyth).configure(ids[i], prices[i], tick, tick - 3600);
        vm.deal(address(this), 1 ether);
        bytes[] memory payload = new bytes[](1); payload[0] = hex'abcd';
        router.updateMarkPrice{value: 1}(payload);
        (bool accepted, uint256 price, uint64 acceptedTime) = oracle.getAcceptedBasketMark();
        require(accepted && acceptedTime == tick && engine.lastMarkTime() == tick, 'accepted mark not advanced');
        require(oracle.getLatestPrice(1).markPrice == price, 'valid basket rejected');
        uint256[] memory times = oracle.getAcceptedFeedPublishTimes();
        require(times.length == 6, 'unexpected accepted feed count');
        for (uint256 i; i < 6; ++i) {
            require(times[i] == tick, 'accepted component not refreshed');
            require(Pyth(pyth).getPriceUnsafe(ids[i]).publishTime == tick - 3600, 'live parsing updated Pyth storage');
        }

        // Model the authorized engine mark advancement used by execution. A real
        // accepted-basket ordering failure must still trigger worker recovery.
        vm.warp(tick + 1);
        vm.prank(address(router)); engine.updateMarkPrice(price, tick + 1);
        (bool ok, bytes memory failure) = address(oracle).staticcall(abi.encodeWithSignature('getLatestPrice(uint8)', 1));
        require(!ok && bytes4(failure) == bytes4(keccak256('PletherOracle__PriceOutOfOrder(uint64,uint64)')), 'expected accepted-basket ordering failure');
        SeparatePythState(pyth).setParsedTime(tick + 1);
        router.updateMarkPrice{value: 1}(payload);
        Snapshot memory repaired = oracle.getLatestPrice(1);
        require(repaired.publishTime == tick + 1 && repaired.markPrice == price, 'accepted basket not repaired');
        require(engine.lastMarkTime() == tick + 1 && engine.lastMarkPrice() == price, 'repair changed engine terms');
        require(Pyth(pyth).getPriceUnsafe(ids[0]).publishTime == tick - 3600, 'repair depended on Pyth storage');
    }
}
