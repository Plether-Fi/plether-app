// SPDX-License-Identifier: AGPL-3.0
pragma solidity 0.8.35;

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
    Router constant router = Router(address(bytes20(hex'918663d6da8cb33b325a04c4a5e8c51d42f3b444')));

    function test_V2RefreshValidatesAcceptedBasketWhilePythStorageStaysOld() public {
        vm.createSelectFork(vm.envString('ARB_SEPOLIA_RPC_URL'));
        Oracle oracle = Oracle(router.pletherOracle());
        Engine engine = Engine(router.engine());
        require(address(oracle) == address(bytes20(hex'4f2c62a0e03657f52c633c19c11f88d8f0897bb2')), 'unexpected v2 oracle');
        require(address(engine) == address(bytes20(hex'ca18a6783d7542f9d2f28f4f259772b74c4d6baa')), 'unexpected v2 engine');
        require(address(oracle).codehash == hex'1ecb1ca760e7ba51d958697eeefe06ff7ba1a3ad162b3e7978af3df30393bc13', 'oracle runtime changed');
        require(address(router).codehash == hex'6c65de67ef2824572537e3641a71100100c49d909b196c43e5d271312534d3cf', 'router runtime changed');
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
