// SPDX-License-Identifier: AGPL-3.0-only
pragma solidity 0.8.35;

import {FullPrecisionMath} from "./FullPrecisionMath.sol";

/// @dev Mirrors the canonical v2.0.0 lens ABI. The artifact generator checks
///      both tuple layouts against the generated consumer ABI before compiling.
interface IPerpsMaxOpenLens {
    struct OpenPreview {
        bool valid;
        uint8 invalidReason;
        uint8 failureCategory;
        uint256 executionPrice;
        uint256 sizeDelta;
        uint256 notionalAssets;
        uint256 marginDeltaAssets;
        int256 vpiAssets;
        uint256 executionFeeAssets;
        int256 tradeCostAssets;
        uint256 poolRebatePayoutAssets;
        uint256 pendingCarryAssets;
        uint256 initialMarginRequirementAssets;
        uint256 maintenanceMarginAssets;
        uint256 postSize;
        uint256 postMarginAssets;
        uint256 postEntryPrice;
        int256 postVpiAccrued;
        int256 postUnrealizedPnlAssets;
        int256 postEquityAssets;
        uint256 postHealthBps;
        bool postLiquidatable;
        bool hasLiquidationPrice;
        uint256 liquidationPrice;
    }

    struct MaxOpenQuote {
        uint256 maxSizeDelta;
        OpenPreview preview;
        uint8 limitingReason;
    }

    function quoteMaxOpen(address account, uint8 side, uint256 marginDelta, uint256 oraclePrice)
        external
        view
        returns (MaxOpenQuote memory quote);

    function previewOpen(address account, uint8 side, uint256 sizeDelta, uint256 marginDelta, uint256 oraclePrice)
        external
        view
        returns (OpenPreview memory preview);
}

interface IPerpsMaxOpenEngine {
    function CAP_PRICE() external view returns (uint256);
}

interface IPerpsMaxOpenRouter {
    function maxOpenOrderExecutionBountyAssets() external view returns (uint256);
}

/// @notice A deployless read helper that applies selected leverage using the
///         deployed canonical planner. It never signs or submits a transaction.
/// @dev All constituent reads share the same eth_call state. Planner validity
///      may have disconnected ranges: an invalid intermediate preview reverts,
///      rather than being treated as an upper bound. Router admission, oracle
///      freshness, adverse-price review and submission checks remain with the
///      existing frontend preparation flow.
contract PerpsMaxOpenQuoter {
    uint256 internal constant SIZE_QUANTUM = 100e18;
    uint256 internal constant SIZE_TO_ASSETS_SCALE = 1e20;

    error PerpsMaxOpenQuoter__InvalidMaxLeverage();
    error PerpsMaxOpenQuoter__InvalidPreview(uint8 reason);
    error PerpsMaxOpenQuoter__NoPositiveEquity();
    error PerpsMaxOpenQuoter__SearchLimitExceeded();

    /// @notice Reserves open and protection rewards, then quotes an additional
    ///         position size using all remaining available margin.
    /// @dev Bounty and immutable cap are read inside this call, so even an
    ///      uncached frontend quote uses a single RPC with consistent state.
    function quoteAvailable(
        address lens,
        address engine,
        address router,
        address account,
        uint8 side,
        uint256 availableAssets,
        uint256 protectionRewardsAssets,
        uint256 oraclePrice,
        uint256 maxLeverageBps
    ) external view returns (uint256 marginDelta, IPerpsMaxOpenLens.MaxOpenQuote memory quote) {
        if (maxLeverageBps == 0) revert PerpsMaxOpenQuoter__InvalidMaxLeverage();
        uint256 bounty = IPerpsMaxOpenRouter(router).maxOpenOrderExecutionBountyAssets();
        // Saturating subtraction also preserves the bigint behavior when the
        // sum of both reward reservations would overflow a uint256.
        if (bounty >= availableAssets || protectionRewardsAssets >= availableAssets - bounty) return (0, quote);
        marginDelta = availableAssets - bounty - protectionRewardsAssets;
        quote = _quote(lens, engine, account, side, marginDelta, oraclePrice, maxLeverageBps);
    }

    /// @notice Quotes with a margin budget already determined by order review.
    /// @dev Keeps review-specific reward reservations and funding checks intact.
    function quoteWithMargin(
        address lens,
        address engine,
        address account,
        uint8 side,
        uint256 marginDelta,
        uint256 oraclePrice,
        uint256 maxLeverageBps
    ) external view returns (IPerpsMaxOpenLens.MaxOpenQuote memory quote) {
        if (maxLeverageBps == 0) revert PerpsMaxOpenQuoter__InvalidMaxLeverage();
        return _quote(lens, engine, account, side, marginDelta, oraclePrice, maxLeverageBps);
    }

    function _quote(
        address lensAddress,
        address engine,
        address account,
        uint8 side,
        uint256 marginDelta,
        uint256 oraclePrice,
        uint256 maxLeverageBps
    ) private view returns (IPerpsMaxOpenLens.MaxOpenQuote memory quote) {
        IPerpsMaxOpenLens lens = IPerpsMaxOpenLens(lensAddress);
        quote = lens.quoteMaxOpen(account, side, marginDelta, oraclePrice);
        if (quote.maxSizeDelta == 0 || !quote.preview.valid) return quote;
        uint256 capPrice = IPerpsMaxOpenEngine(engine).CAP_PRICE();
        uint256 price = oraclePrice < capPrice ? oraclePrice : capPrice;
        if (_withinLeverage(quote.preview, price, maxLeverageBps)) return quote;

        uint256 lower;
        uint256 upper = quote.maxSizeDelta / SIZE_QUANTUM;
        if (quote.maxSizeDelta % SIZE_QUANTUM != 0) upper += 1;
        uint256 bestSize;
        for (uint256 attempt; lower + 1 < upper; attempt += 1) {
            if (attempt >= 64) revert PerpsMaxOpenQuoter__SearchLimitExceeded();
            uint256 midpoint = (lower + upper) / 2;
            uint256 sizeDelta = midpoint * SIZE_QUANTUM;
            IPerpsMaxOpenLens.OpenPreview memory preview =
                lens.previewOpen(account, side, sizeDelta, marginDelta, oraclePrice);
            if (_withinLeverage(preview, price, maxLeverageBps)) {
                lower = midpoint;
                bestSize = sizeDelta;
                quote.preview = preview;
            } else {
                upper = midpoint;
            }
        }
        // If no lot fits, retain the original valid protocol preview as the
        // existing frontend did; maxSizeDelta=0 identifies leverage capacity.
        quote.maxSizeDelta = bestSize;
    }

    function _withinLeverage(IPerpsMaxOpenLens.OpenPreview memory preview, uint256 price, uint256 maxLeverageBps)
        private
        pure
        returns (bool)
    {
        if (!preview.valid) revert PerpsMaxOpenQuoter__InvalidPreview(preview.invalidReason);
        if (preview.postEquityAssets <= 0) revert PerpsMaxOpenQuoter__NoPositiveEquity();
        uint256 notional = FullPrecisionMath.mulDiv(preview.postSize, price, SIZE_TO_ASSETS_SCALE);
        // ceil(notional*10000/equity) <= limit is equivalent to this product
        // comparison. Full-width products match JavaScript bigint rounding
        // without introducing uint256 intermediate overflows.
        return
            FullPrecisionMath.productLessThanOrEqual(
                notional, 10_000, uint256(preview.postEquityAssets), maxLeverageBps
            );
    }
}
