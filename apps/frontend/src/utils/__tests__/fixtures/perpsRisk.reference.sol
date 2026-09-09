// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.35;
// Reference vectors for plether-core d704122c779d4d681d0fa2be517707b7f7df3902.
// Run with the pinned CfdMath, CfdTypes, and PositionRiskAccountingLib sources.
// @plether/perps/ must resolve to those unmodified sources, not the frontend solver.
import {CfdTypes} from "@plether/perps/CfdTypes.sol";
import {PositionRiskAccountingLib as Risk} from "@plether/perps/libraries/PositionRiskAccountingLib.sol";
contract PerpsRiskReferenceTest {
    event log_named_uint(string key, uint256 value);
    function record(string memory name, uint8 side, uint256 size, uint256 entryCost, uint256 margin, uint256 claim, uint256 bps) internal {
        CfdTypes.Position memory pos;
        pos.side = CfdTypes.Side(side); pos.size = size; pos.margin = margin;
        bool loLiquid = Risk.buildExactPriceRiskState(pos, entryCost, 0, 2e8, margin + claim, bps).liquidatable;
        bool hiLiquid = Risk.buildExactPriceRiskState(pos, entryCost, 2e8, 2e8, margin + claim, bps).liquidatable;
        if (side == 0 ? !hiLiquid : !loLiquid) { emit log_named_uint(name, type(uint256).max); return; }
        if (side == 0 ? loLiquid : hiLiquid) { emit log_named_uint(name, side == 0 ? 0 : 2e8); return; }
        uint256 low; uint256 high = 2e8;
        while (low < high) {
            uint256 mid = (low + high + (side == 0 ? 0 : 1)) / 2;
            bool liquid = Risk.buildExactPriceRiskState(pos, entryCost, mid, 2e8, margin + claim, bps).liquidatable;
            if (side == 0) { if (liquid) high = mid; else low = mid + 1; }
            else { if (liquid) low = mid; else high = mid - 1; }
        }
        require(Risk.buildExactPriceRiskState(pos, entryCost, low, 2e8, margin + claim, bps).liquidatable);
        require(!Risk.buildExactPriceRiskState(pos, entryCost, side == 0 ? low - 1 : low + 1, 2e8, margin + claim, bps).liquidatable);
        emit log_named_uint(name, low);
    }
    function testReferenceVectors() external {
        record("long", 0, 10000000000000000000000, 10000000000, 250000000, 0, 10);
        record("short", 1, 10000000000000000000000, 10000000000, 250000000, 0, 10);
        record("claim", 0, 10000000000000000000000, 10000000000, 250000000, 750000000, 10);
        record("added-margin", 0, 10000000000000000000000, 10000000000, 1000000000, 0, 10);
        record("fad", 0, 10000000000000000000000, 10000000000, 250000000, 0, 300);
        record("zero-maintenance-long", 0, 10000000000000000000000, 10000000000, 250000000, 0, 0);
        record("zero-maintenance-short", 1, 10000000000000000000000, 10000000000, 250000000, 0, 0);
        record("no-boundary-long", 0, 10000000000000000000000, 10000000000, 20000000000, 0, 10);
        record("no-boundary-short", 1, 10000000000000000000000, 10000000000, 20000000000, 0, 10);
        record("long-zero-endpoint", 0, 10000000000000000000000, 0, 0, 0, 10);
        record("short-cap-endpoint", 1, 10000000000000000000000, 20000000000, 0, 0, 10);
        record("increased-basis-dust", 0, 10300000000000000000000, 10299999977, 250000000, 0, 10);
        record("reduced-basis-dust", 1, 7300000000000000000000, 7299999991, 250000000, 0, 10);
    }
}
