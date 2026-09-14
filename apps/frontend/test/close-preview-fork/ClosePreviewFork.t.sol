// SPDX-License-Identifier: AGPL-3.0
pragma solidity 0.8.35;

import {Test} from "forge-std/Test.sol";
import {CfdTypes} from "@plether/perps/CfdTypes.sol";
import {OrderV2Types} from "@plether/perps/OrderV2Types.sol";
import {IPletherOracle} from "@plether/perps/interfaces/IPletherOracle.sol";
import {IERC20} from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

interface Pool { function totalAssets() external view returns (uint256); }
interface Book { function currentExecutionConfigHash() external view returns (bytes32); }
interface CfdEngine {
    function clearinghouse() external view returns (address);
    function USDC() external view returns (address);
    function pool() external view returns (Pool);
    function traderClaimBalanceUsdc(address) external view returns (uint256);
    function positions(address) external view returns (uint256,uint256,uint256,uint256,uint8,uint64,int256);
}
interface CfdClosePreview {
    struct ClosePreview { uint256 commitmentCarryUsdc; uint256 executionBountyUsdc; OrderV2Types.ExecutionAssessment assessment; }
    function previewClose(address,CfdTypes.Order calldata,address,uint256,uint64,OrderV2Types.ExecutionBounds calldata) external view returns (ClosePreview memory);
}
interface CfdOrderPolicyEvaluator {
    function assessOrder(address,CfdTypes.Order calldata,address,uint256,uint256,uint64,OrderV2Types.ExecutionBounds calldata,uint256) external view returns (OrderV2Types.ExecutionAssessment memory);
}
interface OrderRouter {
    function lifecycleBook() external view returns (Book);
    function pletherOracle() external view returns (address);
    function policyEvaluator() external view returns (address);
    function closeOrderExecutionBountyUsdc() external view returns (uint256);
    function commitOrder(OrderV2Types.OrderRequest calldata) external returns (uint64);
    function executeOrder(uint64,bytes[] calldata) external payable returns (OrderV2Types.ExecutionResult memory);
}
interface MarginClearinghouse {
    struct Buckets { uint256 settlementBalanceUsdc; uint256 totalLockedMarginUsdc; uint256 activePositionMarginUsdc; uint256 otherLockedMarginUsdc; uint256 freeSettlementUsdc; }
    function getAccountUsdcBuckets(address) external view returns (Buckets memory);
    function balanceUsdc(address) external view returns (uint256);
    function deposit(address,uint256) external;
    function withdraw(address,uint256) external;
    function totalBountyReservationsUsdc(address) external view returns (uint256);
}

// Only the oracle response and test-wallet funding are controlled. Engine,
// planner, clearinghouse, router and preview execute their deployed bytecode.
contract ClosePreviewForkTest is Test {
    CfdEngine engine = CfdEngine(address(bytes20(hex"afece93321be41aa73474457e2f47cf7b2fb738f")));
    OrderRouter router = OrderRouter(payable(address(bytes20(hex"6215d36fcbd610ca1525252eebcbfd8b223a6072"))));
    CfdClosePreview lens = CfdClosePreview(address(bytes20(hex"202a2c5156563ec4fef7d3997771bbca90e98117")));
    MarginClearinghouse ch;
    address trader = address(0xCAFE1234);
    address executor = address(0xBEEF1234);
    CfdTypes.Side side;
    uint256 constant SIZE = 10_000e18;
    uint256 constant PRICE = 1e8;

    function setUp() public {
        vm.createSelectFork(vm.envString("ARB_SEPOLIA_RPC_URL"), vm.envOr("CLOSE_PREVIEW_FORK_BLOCK", uint256(308941947)));
        assertEq(address(lens).codehash, hex"2f8f5cf607ddcd71f3bafd166e3fa3d20980a4077eaa28b8508b2a0ac1c29b16");
        assertEq(address(engine).codehash, hex"a4ef32f28b3910d745635f4d505e598f800ce88458b5cf782a71fb2d1e196531");
        ch = MarginClearinghouse(address(engine.clearinghouse()));
        address token = address(engine.USDC());
        deal(token, trader, 1000e6);
        vm.startPrank(trader);
        IERC20(token).approve(address(ch), type(uint256).max);
        ch.deposit(trader, 1000e6);
        vm.stopPrank();
    }

    function bounds() internal view returns (OrderV2Types.ExecutionBounds memory b) {
        b.validUntil = uint64(block.timestamp + 60);
        b.allowedExecutionModes = 7;
        b.expectedConfigHash = router.lifecycleBook().currentExecutionConfigHash();
        b.maxExecutionBountyUsdc = type(uint256).max;
        b.maxExecutionNotionalUsdc = type(uint256).max;
        b.maxGrossAccountDebitUsdc = type(uint256).max;
        b.maxActionChargeUsdc = type(uint256).max;
        b.maxExplicitFeesUsdc = type(uint256).max;
        b.maxPostPositionSize = type(uint256).max;
        b.maxPostLeverageBps = type(uint32).max;
    }

    function execute(uint64 id, uint256 price) internal returns (OrderV2Types.ExecutionResult memory) {
        IPletherOracle oracle = IPletherOracle(address(router.pletherOracle()));
        IPletherOracle.PriceSnapshot memory p = IPletherOracle.PriceSnapshot(price, price, uint64(block.timestamp), 0, 60, false, false, false);
        vm.mockCall(address(oracle), abi.encodeWithSelector(IPletherOracle.getUpdateFee.selector), abi.encode(uint256(0)));
        vm.mockCall(address(oracle), abi.encodeWithSelector(IPletherOracle.updateOrderExecutionPrice.selector), abi.encode(true, p));
        vm.prank(executor);
        return router.executeOrder(id, new bytes[](0));
    }

    function openPosition(uint256 freeAfter) internal {
        OrderV2Types.OrderRequest memory request = OrderV2Types.OrderRequest(bytes32(uint256(1)), side, SIZE, 250e6, PRICE, false, bounds());
        vm.prank(trader);
        uint64 openId = router.commitOrder(request);
        vm.roll(block.number + 1);
        vm.warp(block.timestamp + 1);
        assertEq(uint8(execute(openId, PRICE).status), uint8(OrderV2Types.LifecycleStatus.Executed), "deployed opening succeeds");
        uint256 free = ch.getAccountUsdcBuckets(trader).freeSettlementUsdc;
        vm.prank(trader);
        ch.withdraw(trader, free - freeAfter);
    }

    function test_DeployedShortReservationRegression() public {
        side = CfdTypes.Side.SHORT;
        checkParity(97_500_000, true);
    }

    function test_DeployedLongExecutionParity() public {
        side = CfdTypes.Side.LONG;
        checkParity(102_500_000, false);
    }

    function test_DeployedSelfCloseParity() public {
        side = CfdTypes.Side.SHORT;
        executor = trader;
        checkParity(97_500_000, true);
    }

    function test_DeployedPartialLongLoss() public { checkPartial(CfdTypes.Side.LONG, 101_000_000); }
    function test_DeployedPartialLongGain() public { checkPartial(CfdTypes.Side.LONG, 99_000_000); }
    function test_DeployedPartialShortLoss() public { checkPartial(CfdTypes.Side.SHORT, 99_000_000); }
    function test_DeployedPartialShortGain() public { checkPartial(CfdTypes.Side.SHORT, 101_000_000); }

    function checkPartial(CfdTypes.Side positionSide, uint256 price) internal {
        side = positionSide;
        openPosition(100e6);
        CfdTypes.Order memory order = CfdTypes.Order(trader, SIZE / 2, 0, price, uint64(block.timestamp), uint64(block.number), 0, side, true);
        CfdClosePreview.ClosePreview memory preview = lens.previewClose(address(engine), order, executor, price, uint64(block.timestamp), bounds());
        OrderV2Types.OrderRequest memory request = OrderV2Types.OrderRequest(bytes32(uint256(2)), side, SIZE / 2, 0, price, true, bounds());
        vm.prank(trader);
        uint64 id = router.commitOrder(request);
        CfdOrderPolicyEvaluator evaluator = CfdOrderPolicyEvaluator(router.policyEvaluator());
        OrderV2Types.ExecutionAssessment memory committed = evaluator.assessOrder(address(engine), order, executor, price, engine.pool().totalAssets(), uint64(block.timestamp), bounds(), preview.executionBountyUsdc);
        assertEq(keccak256(abi.encode(committed)), keccak256(abi.encode(preview.assessment)), "partial commitment parity");
        vm.roll(block.number + 1);
        vm.warp(block.timestamp + 1);
        // Future carry is deliberately excluded by the lens. Assess the committed
        // order again at actual execution time instead of inventing a tolerance.
        committed = evaluator.assessOrder(address(engine), order, executor, price, engine.pool().totalAssets(), uint64(block.timestamp), bounds(), preview.executionBountyUsdc);
        assertEq(uint8(execute(id, price).status), uint8(OrderV2Types.LifecycleStatus.Executed));
        assertEq(ch.balanceUsdc(trader), committed.postSettlementBalanceUsdc);
        assertEq(engine.traderClaimBalanceUsdc(trader), committed.postTraderClaimUsdc);
        (uint256 sizeAfter, uint256 marginAfter,,,,,) = engine.positions(trader);
        assertEq(sizeAfter, committed.postPositionSize);
        assertEq(marginAfter, committed.postPositionMarginUsdc);
        assertEq(ch.totalBountyReservationsUsdc(trader), 0);
    }

    function checkParity(uint256 adverse, bool regression) internal {
        openPosition(router.closeOrderExecutionBountyUsdc());
        uint256 bounty = router.closeOrderExecutionBountyUsdc();
        CfdTypes.Order memory order = CfdTypes.Order(trader, SIZE, 0, adverse, uint64(block.timestamp), uint64(block.number), 0, side, true);
        CfdClosePreview.ClosePreview memory preview = lens.previewClose(address(engine), order, executor, adverse, uint64(block.timestamp), bounds());
        OrderV2Types.ExecutionAssessment memory old = CfdOrderPolicyEvaluator(router.policyEvaluator()).assessOrder(address(engine), order, executor, adverse, engine.pool().totalAssets(), uint64(block.timestamp), bounds(), bounty);
        if (regression) {
            assertEq(old.actionChargeCollectedUsdc - preview.assessment.actionChargeCollectedUsdc, bounty, "old preview spends new bounty");
            assertEq(preview.assessment.postSettlementBalanceUsdc - old.postSettlementBalanceUsdc, bounty);
        }
        uint256 beforeBalance = ch.balanceUsdc(trader);
        OrderV2Types.OrderRequest memory request = OrderV2Types.OrderRequest(bytes32(uint256(2)), side, SIZE, 0, adverse, true, bounds());
        vm.prank(trader);
        uint64 closeId = router.commitOrder(request);
        assertEq(beforeBalance - ch.balanceUsdc(trader), preview.commitmentCarryUsdc);
        OrderV2Types.ExecutionAssessment memory committed = CfdOrderPolicyEvaluator(router.policyEvaluator()).assessOrder(address(engine), order, executor, adverse, engine.pool().totalAssets(), uint64(block.timestamp), bounds(), bounty);
        assertEq(keccak256(abi.encode(committed)), keccak256(abi.encode(preview.assessment)), "exact committed assessment parity");
        assertExecution(closeId, adverse, preview);
    }

    function assertExecution(uint64 closeId, uint256 adverse, CfdClosePreview.ClosePreview memory preview) internal {
        // Execution requires a publish time strictly after commitment. The
        // full-consumption cases still compare exact post-state, without tolerance.
        vm.roll(block.number + 1);
        vm.warp(block.timestamp + 1);
        assertEq(uint8(execute(closeId, adverse).status), uint8(OrderV2Types.LifecycleStatus.Executed), "deployed close succeeds");
        assertEq(ch.balanceUsdc(trader), preview.assessment.postSettlementBalanceUsdc);
        assertEq(engine.traderClaimBalanceUsdc(trader), preview.assessment.postTraderClaimUsdc);
        (uint256 sizeAfter, uint256 marginAfter,,,,,) = engine.positions(trader);
        assertEq(sizeAfter, preview.assessment.postPositionSize);
        assertEq(marginAfter, preview.assessment.postPositionMarginUsdc);
        assertEq(ch.totalBountyReservationsUsdc(trader), 0);
    }
}
