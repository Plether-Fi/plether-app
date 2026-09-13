// SPDX-License-Identifier: AGPL-3.0
pragma solidity 0.8.35;
import {BasePerpTest} from "./BasePerpTest.sol";
import {PositionProtectionTypes} from "@plether/perps/interfaces/PositionProtectionTypes.sol";
import {IPositionProtectionBook} from "@plether/perps/interfaces/IPositionProtectionBook.sol";
import {IPositionProtectionActions} from "@plether/perps/interfaces/IPositionProtectionActions.sol";
import {IPositionProtectionViews} from "@plether/perps/interfaces/IPositionProtectionViews.sol";
import {CfdTypes} from "@plether/perps/CfdTypes.sol";
import {OrderV2Types} from "@plether/perps/OrderV2Types.sol";
import {Vm} from "forge-std/Vm.sol";
import {StdStorage, stdStorage} from "forge-std/StdStorage.sol";

struct PackedOp {
    address sender;
    uint256 nonce;
    bytes initCode;
    bytes callData;
    bytes32 accountGasLimits;
    uint256 preVerificationGas;
    bytes32 gasFees;
    bytes paymasterAndData;
    bytes signature;
}

interface EP {
    function getNonce(address, uint192) external view returns (uint256);
    function getUserOpHash(PackedOp calldata) external view returns (bytes32);
    function handleOps(PackedOp[] calldata, address payable) external;
    function depositTo(address) external payable;
    function balanceOf(address) external view returns (uint256);
}

interface PM {
    function getSponsorshipHash(PackedOp calldata) external view returns (bytes32);
    function policyId() external view returns (bytes32);
}

// Exact release source + real captured AA runtime. Only Pyth input and initial
// claims are synthetic; no RPC, production key, Core deployment or authorization.
contract NativeAALifecycleTest is BasePerpTest {
    using stdStorage for StdStorage;
    address constant ALICE = address(0xA11CE);
    address constant EXECUTION_KEEPER = address(0xE0EC);
    uint256 constant POSITION_SIZE = 10_000e18;
    uint256 constant POSITION_MARGIN_USDC = 2000e6;
    IPositionProtectionBook protectionBook;
    IPositionProtectionActions protectionActions;
    IPositionProtectionViews protectionViews;
    address constant ENTRY = 0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108;
    address constant PAYMASTER = 0x9761091045616A388f5fE1433721B272c78fe31b;
    uint256 constant CAP = 3_000_000;
    bytes32 constant OP_EVENT = keccak256("UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)");
    bytes32 constant REVERT_EVENT = keccak256("UserOperationRevertReason(bytes32,address,uint256,bytes)");

    function _setupAA() internal {
        protectionBook = router.positionProtectionBook();
        protectionActions = IPositionProtectionActions(address(protectionBook));
        protectionViews = IPositionProtectionViews(address(protectionBook));
        _fundTrader(ALICE, 20_000e6);
        vm.chainId(421614);
        string memory f = vm.readFile("aa-fixture.json");
        vm.etch(ENTRY, vm.parseJsonBytes(f, ".accounts.0x4337084d9e255ff0702461cf8895ce9e3b5ff108.code"));
        vm.etch(
            0x13E9ed32155810FDbd067D4522C492D6f68E5944,
            vm.parseJsonBytes(f, ".accounts.0x13e9ed32155810fdbd067d4522c492d6f68e5944.code")
        );
        vm.etch(PAYMASTER, vm.parseJsonBytes(f, ".accounts.0x9761091045616a388f5fe1433721b272c78fe31b.code"));
        vm.etch(ALICE, vm.parseJsonBytes(f, ".accounts.0x9314586d4068c73b23a64d7406ca8ffeecc2cbfc.code"));
        bytes32 implementationSlot = 0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc;
        bytes32 implementationWord = vm.parseJsonBytes32(
            f,
            ".accounts.0x9314586d4068c73b23a64d7406ca8ffeecc2cbfc.storage.0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc"
        );
        vm.store(ALICE, implementationSlot, implementationWord);
        address implementation = address(uint160(uint256(implementationWord)));
        vm.etch(
            implementation,
            vm.parseJsonBytes(f, string.concat(".accounts.", vm.toLowercase(vm.toString(implementation)), ".code"))
        );
        assertEq(ENTRY.codehash, 0xe3f30f78ae55058acdefea00952c8e44f2263215cf720fe1b27b6f148add0278);
        assertEq(PAYMASTER.codehash, 0xb8ae276b01850fdbb8d9d7fd32ec7b9b1c7ab7af20f5d62179a76f6b4912c528);
        string memory path = ".accounts.0x9761091045616a388f5fe1433721b272c78fe31b.storage";
        string[] memory slots = vm.parseJsonKeys(f, path);
        for (uint256 i; i < slots.length; i++) {
            vm.store(PAYMASTER, vm.parseBytes32(slots[i]), vm.parseJsonBytes32(f, string.concat(path, ".", slots[i])));
        }
        vm.store(ALICE, bytes32(0), bytes32(uint256(uint160(vm.addr(1)))));
        vm.store(PAYMASTER, bytes32(uint256(4)), bytes32(uint256(uint160(vm.addr(2)))));
        vm.deal(address(this), 100 ether);
        EP(ENTRY).depositTo{value: 10 ether}(PAYMASTER);
        bytes[] memory tick = _mockPythUpdateData(120_000_000);
        router.updateMarkPrice(tick);
    }

    function _params(uint256 tp, uint256 sl)
        internal
        pure
        returns (PositionProtectionTypes.PositionProtectionParams memory)
    {
        return PositionProtectionTypes.PositionProtectionParams(tp, sl);
    }

    function _totalProtectionBountyUsdc() internal view returns (uint256) {
        return router.positionProtectionTriggerBountyUsdc() + router.closeOrderExecutionBountyUsdc();
    }

    function _seedAuthenticatedTraderClaim(address account, uint256 amount) internal {
        bytes32 oldHash = terminalNavBook.curveHashOf(account);
        uint256 total = engine.totalTraderClaimBalanceUsdc() - engine.traderClaimBalanceUsdc(account) + amount;
        stdstore.target(address(engine)).sig("traderClaimBalanceUsdc(address)").with_key(account).checked_write(amount);
        stdstore.target(address(engine)).sig("totalTraderClaimBalanceUsdc()").checked_write(total);
        vm.startPrank(address(engine));
        terminalNavBook.syncFromEngine(account, oldHash);
        terminalNavBook.authenticateEngineState(account);
        vm.stopPrank();
    }

    function _sig(uint256 key, bytes32 hash) private returns (bytes memory) {
        (uint8 v, bytes32 r, bytes32 s) = vm.sign(key, hash);
        return abi.encodePacked(r, s, v);
    }

    function _sponsor(string memory label, address target, bytes memory data, bool expected) internal {
        _sponsorAtGas(label, target, data, expected, CAP);
    }

    function _sponsorAtGas(string memory label, address target, bytes memory data, bool expected, uint256 gasLimit)
        internal
    {
        PackedOp memory op;
        op.sender = ALICE;
        op.nonce = EP(ENTRY).getNonce(ALICE, 0);
        op.callData = abi.encodeWithSignature("execute(address,uint256,bytes)", target, 0, data);
        op.accountGasLimits = bytes32((uint256(100_000) << 128) | gasLimit);
        op.preVerificationGas = 59_000;
        op.gasFees = bytes32((uint256(1) << 128) | 1 gwei);
        bytes memory prefix = abi.encodePacked(
            PAYMASTER,
            uint128(100_000),
            uint128(0),
            uint48(block.timestamp + 120),
            uint48(block.timestamp - 30),
            uint128(0.01 ether),
            PM(PAYMASTER).policyId(),
            ALICE.codehash
        );
        op.paymasterAndData = bytes.concat(prefix, new bytes(65));
        op.paymasterAndData = bytes.concat(prefix, _sig(2, PM(PAYMASTER).getSponsorshipHash(op)));
        bytes32 hash = EP(ENTRY).getUserOpHash(op);
        op.signature = _sig(1, hash);
        PackedOp[] memory ops = new PackedOp[](1);
        ops[0] = op;
        vm.cool(ALICE);
        vm.cool(address(router));
        vm.cool(address(engine));
        vm.cool(address(clearinghouse));
        vm.cool(address(terminalNavBook));
        vm.cool(address(protectionBook));
        vm.cool(address(pool));
        uint256 beforeDeposit = EP(ENTRY).balanceOf(PAYMASTER);
        vm.recordLogs();
        EP(ENTRY).handleOps(ops, payable(address(this)));
        Vm.Log[] memory logs = vm.getRecordedLogs();
        bool found;
        for (uint256 i; i < logs.length; i++) {
            if (logs[i].emitter != ENTRY || logs[i].topics.length < 2 || logs[i].topics[1] != hash) continue;
            if (logs[i].topics[0] == REVERT_EVENT) {
                (, bytes memory reason) = abi.decode(logs[i].data, (uint256, bytes));
                emit log_named_bytes(label, reason);
            }
            if (logs[i].topics[0] == OP_EVENT) {
                (, bool success, uint256 cost, uint256 used) =
                    abi.decode(logs[i].data, (uint256, bool, uint256, uint256));
                assertEq(success, expected, label);
                assertEq(beforeDeposit - EP(ENTRY).balanceOf(PAYMASTER), cost);
                emit log_named_uint(label, used);
                found = true;
            }
        }
        assertTrue(found, "must observe exact UserOperation outcome");
    }

    function _protectedOpenRequest() internal view returns (OrderV2Types.OrderRequest memory request) {
        request.side = CfdTypes.Side.LONG;
        request.sizeDelta = POSITION_SIZE;
        request.marginDelta = POSITION_MARGIN_USDC;
        request.targetPrice = 1;
        request.clientOrderId = keccak256("offline-native-protected-open");
        request.bounds.allowedExecutionModes = 1;
        request.bounds.validUntil = uint64(block.timestamp + 60);
        request.bounds.expectedConfigHash = router.lifecycleBook().currentExecutionConfigHash();
        request.bounds.maxExecutionBountyUsdc = 10e6;
        request.bounds.maxExecutionNotionalUsdc = 20_000e6;
        request.bounds.maxGrossAccountDebitUsdc = 10_000e6;
        request.bounds.maxActionChargeUsdc = 1000e6;
        request.bounds.maxExplicitFeesUsdc = 1000e6;
        request.bounds.maxPostPositionSize = POSITION_SIZE;
        request.bounds.maxPostLeverageBps = 100_000;
    }

    // Regression reproducing the previous preparation ceiling, not the new policy:
    // an accurate estimate must exceed 1.4M, whose 50% padding already hits 2.1M.
    function test_NativeAA_ProtectedOpenExceedsPreviousRawEstimate() public {
        _setupAA();
        _sponsorAtGas(
            "protected_open_raw_ceiling_rejected",
            address(protectionBook),
            abi.encodeCall(
                protectionActions.commitOpenOrderWithProtection,
                (_protectedOpenRequest(), _params(100_000_000, 140_000_000))
            ),
            false,
            1_400_000
        );
        assertEq(protectionViews.activePositionProtectionId(ALICE), 0);
        assertEq(router.pendingOrderCounts(ALICE), 0);
        assertEq(router.getAccountReservations(ALICE).executionBountyUsdc, 0);
    }

    function test_NativeAA_ProtectedOpenReplaceTriggerExecute() public {
        _setupAA();
        uint64 parent = router.nextCommitId();
        OrderV2Types.OrderRequest memory request = _protectedOpenRequest();
        // The recorded cold execution was 1,545,240 gas. Exercise the exact
        // rounded policy result (2,317,860), not an arbitrary assigned 3M.
        uint256 raw = 1_545_240;
        uint256 padded = (raw * 3 + 1) / 2;
        assertLe(raw, 2_000_000);
        assertLe(padded, CAP);
        _sponsorAtGas(
            "protected_open",
            address(protectionBook),
            abi.encodeCall(
                protectionActions.commitOpenOrderWithProtection, (request, _params(100_000_000, 140_000_000))
            ),
            true,
            padded
        );
        uint64 id = protectionViews.activePositionProtectionId(ALICE);
        assertGt(id, 0);
        assertEq(
            uint8(protectionViews.getPositionProtection(id).status),
            uint8(PositionProtectionTypes.PositionProtectionStatus.PendingOpen)
        );
        _sponsor(
            "replace_pending",
            address(protectionBook),
            abi.encodeCall(protectionActions.replacePositionProtection, (id, _params(95_000_000, 145_000_000))),
            true
        );
        bytes[] memory tick = _mockPythUpdateData(120_000_000);
        vm.prank(EXECUTION_KEEPER);
        router.executeOrder(parent, tick);
        assertEq(
            uint8(protectionViews.getPositionProtection(id).status),
            uint8(PositionProtectionTypes.PositionProtectionStatus.Armed)
        );
        (uint256 size,,,,,,) = engine.positions(ALICE);
        assertEq(size, POSITION_SIZE);
        tick = _mockPythUpdateData(95_000_000);
        uint64 closeId = protectionActions.triggerPositionProtection(id, tick);
        tick = _mockPythUpdateData(95_000_000);
        vm.prank(EXECUTION_KEEPER);
        router.executeOrder(closeId, tick);
        assertEq(
            uint8(protectionViews.getPositionProtection(id).status),
            uint8(PositionProtectionTypes.PositionProtectionStatus.Executed)
        );
        (size,,,,,,) = engine.positions(ALICE);
        assertEq(size, 0);
        assertEq(protectionViews.activePositionProtectionId(ALICE), 0);
        assertEq(router.getAccountReservations(ALICE).executionBountyUsdc, 0);
    }

    function test_NativeAA_CreateReplaceCancel() public {
        _setupAA();
        _open(ALICE, CfdTypes.Side.SHORT, POSITION_SIZE, POSITION_MARGIN_USDC, 120_000_000);
        uint256 free = _freeSettlementUsdc(ALICE);
        _sponsor(
            "protection_create",
            address(protectionBook),
            abi.encodeCall(protectionActions.createPositionProtection, (_params(140_000_000, 100_000_000))),
            true
        );
        uint64 id = protectionViews.activePositionProtectionId(ALICE);
        assertGt(id, 0);
        assertEq(_freeSettlementUsdc(ALICE), free - _totalProtectionBountyUsdc());
        _sponsor(
            "protection_replace",
            address(protectionBook),
            abi.encodeCall(protectionActions.replacePositionProtection, (id, _params(150_000_000, 90_000_000))),
            true
        );
        assertEq(protectionViews.getPositionProtection(id).takeProfitTriggerPrice, 150_000_000);
        _sponsor(
            "protection_cancel",
            address(protectionBook),
            abi.encodeCall(protectionActions.cancelPositionProtection, (id)),
            true
        );
        assertEq(protectionViews.activePositionProtectionId(ALICE), 0);
        assertEq(_freeSettlementUsdc(ALICE), free);
        assertEq(router.getAccountReservations(ALICE).executionBountyUsdc, 0);
    }

    function test_NativeAA_ClaimWithoutPositionAndDuplicateRejection() public {
        _setupAA();
        _seedAuthenticatedTraderClaim(ALICE, 50e6);
        uint256 settled = _settlementBalance(ALICE);
        uint256 cash = usdc.balanceOf(address(pool));
        _sponsor("claim_flat", address(engine), abi.encodeCall(engine.settleTraderClaim, (ALICE)), true);
        assertEq(engine.traderClaimBalanceUsdc(ALICE), 0);
        assertEq(engine.totalTraderClaimBalanceUsdc(), 0);
        assertEq(_settlementBalance(ALICE), settled + 50e6);
        assertEq(usdc.balanceOf(address(pool)), cash - 50e6);
        _sponsor("claim_duplicate_rejected", address(engine), abi.encodeCall(engine.settleTraderClaim, (ALICE)), false);
        assertEq(_settlementBalance(ALICE), settled + 50e6);
    }

    function test_NativeAA_ClaimWithPosition() public {
        _setupAA();
        _open(ALICE, CfdTypes.Side.SHORT, POSITION_SIZE, POSITION_MARGIN_USDC, 120_000_000);
        _seedAuthenticatedTraderClaim(ALICE, 50e6);
        uint256 settled = _settlementBalance(ALICE);
        uint256 cash = usdc.balanceOf(address(pool));
        _sponsor("claim_position", address(engine), abi.encodeCall(engine.settleTraderClaim, (ALICE)), true);
        assertEq(engine.traderClaimBalanceUsdc(ALICE), 0);
        assertEq(engine.totalTraderClaimBalanceUsdc(), 0);
        assertEq(_settlementBalance(ALICE), settled + 50e6);
        assertEq(usdc.balanceOf(address(pool)), cash - 50e6);
    }

    function test_NativeAA_ClaimLiquidityShortfallThenRecovery() public {
        _setupAA();
        _seedAuthenticatedTraderClaim(ALICE, 50e6);
        usdc.burn(address(pool), usdc.balanceOf(address(pool)) - 20e6);
        uint256 settled = _settlementBalance(ALICE);
        _sponsor("claim_shortfall_rejected", address(engine), abi.encodeCall(engine.settleTraderClaim, (ALICE)), false);
        assertEq(engine.traderClaimBalanceUsdc(ALICE), 50e6);
        assertEq(engine.totalTraderClaimBalanceUsdc(), 50e6);
        assertEq(_settlementBalance(ALICE), settled);
        usdc.mint(address(pool), 30e6);
        _sponsor(
            "claim_after_liquidity_recovery", address(engine), abi.encodeCall(engine.settleTraderClaim, (ALICE)), true
        );
        assertEq(engine.traderClaimBalanceUsdc(ALICE), 0);
        assertEq(engine.totalTraderClaimBalanceUsdc(), 0);
        assertEq(_settlementBalance(ALICE), settled + 50e6);
        assertEq(usdc.balanceOf(address(pool)), 0);
    }
}
