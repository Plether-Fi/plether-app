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
struct ExecutionRequest { uint64 commitTime; uint256 targetPrice; uint8 side; bool isClose; bool revertOnHistoricalUnavailable; }
interface Pyth { function getPriceUnsafe(bytes32) external view returns (Price memory); }
interface Oracle {
    function pyth() external view returns (address);
    function pythFeedIds(uint256) external view returns (bytes32);
    function updateOrderExecutionPrice(address, bytes[] calldata, ExecutionRequest calldata) external payable returns (bool, Snapshot memory);
    function getLatestPrice() external view returns (uint256);
    function isOracleFrozen() external view returns (bool);
}
interface Router { function pletherOracle() external view returns (address); function engine() external view returns (address); function updateMarkPrice(bytes[] calldata) external payable; }
interface Engine { function updateMarkPrice(uint256,uint64) external; function lastMarkTime() external view returns (uint64); function lastMarkPrice() external view returns (uint256); }

// Only Pyth's signature verification/storage is replaced. Deployed oracle,
// router, engine and carry accounting bytecode execute unchanged on a local fork.
contract SeparatePythState {
    mapping(bytes32 => Price) public prices;
    uint64 public historicalTime;
    uint64 public storedTime;
    function configure(bytes32 id, Price calldata price, uint64 historical, uint64 stored) external {
        prices[id] = price; historicalTime = historical; storedTime = stored;
    }
    function getPriceUnsafe(bytes32 id) external view returns (Price memory p) { p = prices[id]; p.publishTime = storedTime; }
    function getUpdateFee(bytes[] calldata) external pure returns (uint256) { return 1; }
    function updatePriceFeeds(bytes[] calldata) external payable { require(msg.value == 1); storedTime = historicalTime; }
    function parsePriceFeedUpdatesUnique(bytes[] calldata, bytes32[] calldata ids, uint64 min, uint64 max)
        external payable returns (Feed[] memory result) {
        require(msg.value == 1 && min <= historicalTime && historicalTime <= max);
        result = new Feed[](ids.length);
        for (uint256 i; i < ids.length; ++i) {
            Price memory p = prices[ids[i]]; p.publishTime = historicalTime;
            result[i] = Feed(ids[i], p, p);
        }
        // Parsing deliberately leaves storedTime unchanged, as Pyth does.
    }
}

contract OracleSyncForkTest {
    Vm constant vm = Vm(address(uint160(uint256(keccak256('hevm cheat code')))));
    Router constant router = Router(address(bytes20(hex'6215d36fcbd610ca1525252eebcbfd8b223a6072')));
    function test_EqualTimestampRepairRestoresLiveReadAndPreservesHistoricalPrice() public {
        vm.createSelectFork(vm.envString('ARB_SEPOLIA_RPC_URL'));
        Oracle oracle = Oracle(router.pletherOracle());
        Engine engine = Engine(router.engine());
        require(address(oracle) == address(bytes20(hex'9f4d9ae736b94249b18a85a7e14092bfca0688eb')), 'unexpected deployed oracle');
        address pyth = oracle.pyth();
        bytes32[] memory ids = new bytes32[](6);
        Price[] memory prices = new Price[](6);
        for (uint256 i; i < 6; ++i) { ids[i] = oracle.pythFeedIds(i); prices[i] = Pyth(pyth).getPriceUnsafe(ids[i]); }
        // Advance only the fork to an open-calendar day, leaving live chain untouched.
        uint64 tick = uint64(block.timestamp + 1);
        vm.warp(tick);
        while (oracle.isOracleFrozen()) { tick += 86400; vm.warp(tick); }
        SeparatePythState implementation = new SeparatePythState();
        vm.etch(pyth, address(implementation).code);
        for (uint256 i; i < 6; ++i) SeparatePythState(pyth).configure(ids[i], prices[i], tick, tick - 5);
        vm.deal(address(this), 1 ether);
        bytes[] memory payload = new bytes[](1); payload[0] = hex'abcd';
        ExecutionRequest memory request = ExecutionRequest(tick - 1, 0, 0, false, true);
        (bool ok, Snapshot memory historical) = oracle.updateOrderExecutionPrice{value: 1}(address(this), payload, request);
        require(ok && historical.publishTime == tick, 'historical resolution failed');
        // The same authorized mark installation used after historical resolution.
        vm.prank(address(router)); engine.updateMarkPrice(historical.markPrice, historical.publishTime);
        require(Pyth(pyth).getPriceUnsafe(ids[0]).publishTime == tick - 5, 'parse unexpectedly updated stored feeds');
        (bool liveOk, bytes memory failure) = address(oracle).staticcall(abi.encodeWithSignature('getLatestPrice()'));
        require(!liveOk && bytes4(failure) == bytes4(keccak256('PletherOracle__PriceOutOfOrder(uint64,uint64)')), 'expected ordering failure');
        router.updateMarkPrice{value: 1}(payload);
        require(engine.lastMarkTime() == tick && engine.lastMarkPrice() == historical.markPrice, 'repair changed mark');
        require(oracle.getLatestPrice() == historical.markPrice, 'live read not recovered');
        for (uint256 i; i < 6; ++i) require(Pyth(pyth).getPriceUnsafe(ids[i]).publishTime == tick, 'feed not repaired');
        (, Snapshot memory afterRepair) = oracle.updateOrderExecutionPrice{value: 1}(address(this), payload, request);
        require(afterRepair.price == historical.price, 'historical execution price changed');
    }
}
