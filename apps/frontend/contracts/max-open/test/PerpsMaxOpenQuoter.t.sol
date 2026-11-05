// SPDX-License-Identifier: AGPL-3.0-only
pragma solidity 0.8.35;

import {IPerpsMaxOpenLens, PerpsMaxOpenQuoter} from "../PerpsMaxOpenQuoter.sol";

interface Vm {
    function expectRevert(bytes calldata data) external;
    function expectRevert(bytes4 selector) external;
}

contract MockMaxOpenConfiguration {
    uint256 public CAP_PRICE = 200_000_000;
    uint256 public maxOpenOrderExecutionBountyAssets = 200_000;

    function setBounty(uint256 bounty) external {
        maxOpenOrderExecutionBountyAssets = bounty;
    }
}

contract MockMaxOpenLens is IPerpsMaxOpenLens {
    error PlannerDependencyFailure();

    uint256 internal constant QUANTUM = 100e18;
    uint256 public maximum = 100 * QUANTUM;
    uint256 public existingSize;
    int256 public equity = 100e6;
    uint256 public feePerLot;
    uint256 public invalidSize;
    uint256 public expectedPrice;
    bool public protocolInvalid;
    bool public dependencyFailure;
    bool public previewDependencyFailure;
    bool public fixedPostSize;
    uint256 public postSizeOverride;

    function configure(uint256 maxSize, uint256 existing, int256 baseEquity, uint256 fee) external {
        maximum = maxSize;
        existingSize = existing;
        equity = baseEquity;
        feePerLot = fee;
    }

    function setInvalidSize(uint256 size) external {
        invalidSize = size;
    }

    function setProtocolInvalid() external {
        protocolInvalid = true;
    }

    function setDependencyFailure(bool previewOnly) external {
        if (previewOnly) previewDependencyFailure = true;
        else dependencyFailure = true;
    }

    function setExpectedPrice(uint256 price) external {
        expectedPrice = price;
    }

    function setPostSizeOverride(uint256 size) external {
        fixedPostSize = true;
        postSizeOverride = size;
    }

    function quoteMaxOpen(address, uint8, uint256 marginDelta, uint256 oraclePrice)
        external
        view
        returns (MaxOpenQuote memory quote)
    {
        if (dependencyFailure) revert PlannerDependencyFailure();
        quote.maxSizeDelta = maximum;
        quote.preview = _preview(maximum == 0 ? QUANTUM : maximum, marginDelta, oraclePrice);
        quote.limitingReason = 9;
        if (maximum == 0 || protocolInvalid) {
            quote.preview.valid = false;
            quote.preview.invalidReason = 9;
        }
    }

    function previewOpen(address, uint8, uint256 sizeDelta, uint256 marginDelta, uint256 oraclePrice)
        external
        view
        returns (OpenPreview memory preview)
    {
        if (previewDependencyFailure) revert PlannerDependencyFailure();
        return _preview(sizeDelta, marginDelta, oraclePrice);
    }

    function _preview(uint256 size, uint256 margin, uint256 price) private view returns (OpenPreview memory preview) {
        if (expectedPrice != 0) require(price == expectedPrice, "Original oracle input must be preserved");
        preview.valid = size != invalidSize;
        preview.invalidReason = preview.valid ? 0 : 6;
        preview.sizeDelta = size;
        preview.postSize = fixedPostSize ? postSizeOverride : size + existingSize;
        preview.marginDeltaAssets = margin;
        preview.postEquityAssets = equity - int256(size / QUANTUM * feePerLot);
        preview.executionPrice = price;
        preview.postHealthBps = 12345;
        preview.hasLiquidationPrice = true;
        preview.liquidationPrice = 180_000_000;
    }
}

contract PerpsMaxOpenQuoterTest {
    Vm private constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));
    uint256 private constant Q = 100e18;
    PerpsMaxOpenQuoter private helper;
    MockMaxOpenLens private lens;
    MockMaxOpenConfiguration private config;

    function setUp() public {
        helper = new PerpsMaxOpenQuoter();
        lens = new MockMaxOpenLens();
        config = new MockMaxOpenConfiguration();
    }

    function quote(uint256 leverage) private view returns (IPerpsMaxOpenLens.MaxOpenQuote memory) {
        return helper.quoteWithMargin(address(lens), address(config), address(this), 0, 100e6, 80_000_000, leverage);
    }

    function test_ExactHighestLotAndCanonicalPreview() public view {
        IPerpsMaxOpenLens.MaxOpenQuote memory result = quote(100_000);
        require(result.maxSizeDelta == 12 * Q, "12 lots fit;13 exceed10x");
        require(result.preview.sizeDelta == 12 * Q && result.preview.postSize == 12 * Q, "Chosen preview must match");
        require(
            result.preview.postHealthBps == 12345 && result.preview.liquidationPrice == 180_000_000, "Keep full preview"
        );
        require(result.limitingReason == 9, "Preserve canonical limiting reason");
    }

    function test_ExistingExposureCountsAgainstLimit() public {
        lens.configure(100 * Q, 10 * Q, 100e6, 0);
        IPerpsMaxOpenLens.MaxOpenQuote memory result = quote(100_000);
        require(result.maxSizeDelta == 2 * Q && result.preview.postSize == 12 * Q, "Count existing ten lots");
    }

    function test_UsesPostTradeEquityAfterFees() public {
        lens.configure(50 * Q, 0, 100e6, 1e6);
        IPerpsMaxOpenLens.MaxOpenQuote memory result = quote(100_000);
        require(result.maxSizeDelta == 11 * Q, "Fees reduce maximum from12to11lots");
        require(result.preview.postEquityAssets == 89e6, "Use fee-adjusted equity");
    }

    function test_FractionalLeverageRoundsUp() public {
        lens.configure(2 * Q, 0, 80e6 - 1, 0);
        require(quote(10_000).maxSizeDelta == 0, "One lot is slightly above1x");
        lens.configure(2 * Q, 0, 80e6, 0);
        require(quote(10_000).maxSizeDelta == Q, "Exact1x lot is accepted");
    }

    function test_ProtocolMaximumAlreadyFitsDoesNotPreview() public {
        lens.configure(5 * Q, 0, 100e6, 0);
        lens.setDependencyFailure(true);
        require(quote(100_000).maxSizeDelta == 5 * Q, "Reuse protocol preview");
    }

    function test_ZeroCapacityPreservesDiagnostics() public {
        lens.configure(0, 0, 0, 0);
        IPerpsMaxOpenLens.MaxOpenQuote memory result = quote(100_000);
        require(result.maxSizeDelta == 0 && !result.preview.valid, "Preserve zero capacity");
        require(result.preview.invalidReason == 9 && result.preview.sizeDelta == Q, "Keep minimum-size diagnostic");
    }

    function test_InvalidProtocolPreviewIsPreserved() public {
        lens.setProtocolInvalid();
        IPerpsMaxOpenLens.MaxOpenQuote memory result = quote(100_000);
        require(
            result.maxSizeDelta == 100 * Q && !result.preview.valid && result.preview.invalidReason == 9, "Keep failure"
        );
    }

    function test_NoLeverageLotRetainsOriginalValidPreview() public view {
        IPerpsMaxOpenLens.MaxOpenQuote memory result = quote(1);
        require(result.maxSizeDelta == 0 && result.preview.valid, "Zero leverage capacity");
        require(result.preview.sizeDelta == 100 * Q, "Original protocol diagnostic is retained");
    }

    function test_ReserveOpenAndProtectionRewards() public view {
        (uint256 margin, IPerpsMaxOpenLens.MaxOpenQuote memory result) = helper.quoteAvailable(
            address(lens), address(config), address(config), address(this), 0, 100_600_000, 400_000, 80_000_000, 100_000
        );
        require(margin == 100e6 && result.preview.marginDeltaAssets == 100e6, "Both rewards excluded from margin");
    }

    function test_ExhaustedRewardsReturnZeroWithoutLensRead() public view {
        (uint256 margin, IPerpsMaxOpenLens.MaxOpenQuote memory result) = helper.quoteAvailable(
            address(0), address(0), address(config), address(this), 0, 600_000, 400_000, 80_000_000, 100_000
        );
        require(margin == 0 && result.maxSizeDelta == 0, "No margin after exact reward reservation");
    }

    function test_RewardSumOverflowSaturatesToZero() public {
        config.setBounty(type(uint256).max - 1);
        (uint256 margin,) = helper.quoteAvailable(
            address(0), address(0), address(config), address(this), 0, type(uint256).max, 10, 80_000_000, 100_000
        );
        require(margin == 0, "Bigint reward sum saturates available balance");
    }

    function test_CapForLeverageKeepsOriginalOracleForPlanner() public {
        lens.setExpectedPrice(300_000_000);
        IPerpsMaxOpenLens.MaxOpenQuote memory result =
            helper.quoteWithMargin(address(lens), address(config), address(this), 0, 100e6, 300_000_000, 100_000);
        require(result.maxSizeDelta == 5 * Q, "Leverage uses capped200m price");
    }

    function test_InvalidIntermediatePlannerRangeFailsClosed() public {
        lens.setInvalidSize(50 * Q);
        vm.expectRevert(
            abi.encodeWithSelector(PerpsMaxOpenQuoter.PerpsMaxOpenQuoter__InvalidPreview.selector, uint8(6))
        );
        quote(100_000);
    }

    function test_QuoteDependencyFailurePropagates() public {
        lens.setDependencyFailure(false);
        vm.expectRevert(MockMaxOpenLens.PlannerDependencyFailure.selector);
        quote(100_000);
    }

    function test_PreviewDependencyFailurePropagates() public {
        lens.setDependencyFailure(true);
        vm.expectRevert(MockMaxOpenLens.PlannerDependencyFailure.selector);
        quote(100_000);
    }

    function test_NonpositiveEquityFailsClosed() public {
        lens.configure(100 * Q, 0, 0, 0);
        vm.expectRevert(PerpsMaxOpenQuoter.PerpsMaxOpenQuoter__NoPositiveEquity.selector);
        quote(100_000);
    }

    function test_ZeroLeverageFailsClosed() public {
        vm.expectRevert(PerpsMaxOpenQuoter.PerpsMaxOpenQuoter__InvalidMaxLeverage.selector);
        quote(0);
    }

    function test_SearchLimitRemainsExplicit() public {
        lens.configure(type(uint256).max, 0, 1, 0);
        vm.expectRevert(PerpsMaxOpenQuoter.PerpsMaxOpenQuoter__SearchLimitExceeded.selector);
        quote(1);
    }

    function test_ValidPreviewWithWideNotionalProduct() public {
        lens.configure(2 * Q, 0, type(int256).max, 0);
        lens.setPostSizeOverride(type(uint256).max);
        require(quote(1).maxSizeDelta == 2 * Q, "512-bit notional intermediate fits after scaling");
    }

    function test_LeverageProductsCanExceedUint256() public {
        lens.configure(2 * Q, 0, type(int256).max, 0);
        require(quote(type(uint256).max).maxSizeDelta == 2 * Q, "Full-width equity times limit comparison");
    }

    function testFuzz_BoundaryIsExecutableAndNextLotExceedsLeverage(uint8 existingLots, uint16 requestedBps) public {
        uint256 existing = uint256(existingLots) % 10;
        uint256 limit = 10_000 + uint256(requestedBps) % 90_000;
        lens.configure(100 * Q, existing * Q, 100e6, 100_000);
        IPerpsMaxOpenLens.MaxOpenQuote memory result = quote(limit);
        if (result.maxSizeDelta != 0) {
            uint256 notional = (result.maxSizeDelta / Q + existing) * 80e6;
            require(
                notional * 10_000 <= uint256(result.preview.postEquityAssets) * limit, "Chosen lot respects leverage"
            );
        }
        uint256 next = result.maxSizeDelta / Q + 1;
        require((next + existing) * 80e6 * 10_000 > (100e6 - next * 100_000) * limit, "Next lot violates limit");
    }
}
