// SPDX-License-Identifier: MIT
pragma solidity 0.8.35;

// Adapted from OpenZeppelin Contracts v5.5.0 Math.mulDiv (MIT), originally
// Remco Bloemen https://xn--2-umb.com/21/muldiv and Uniswap Labs (MIT).
// Copyright (c) 2016-2025 Zeppelin Group Ltd.
// The full MIT license is in LICENSE-MIT alongside this source.
library FullPrecisionMath {
    error FullPrecisionMath__Overflow();

    function product(uint256 x, uint256 y) internal pure returns (uint256 high, uint256 low) {
        assembly ("memory-safe") {
            let mm := mulmod(x, y, not(0))
            low := mul(x, y)
            high := sub(sub(mm, low), lt(mm, low))
        }
    }

    /// @dev floor(x * y / denominator), allowing a 512-bit intermediate product.
    function mulDiv(uint256 x, uint256 y, uint256 denominator) internal pure returns (uint256 result) {
        unchecked {
            (uint256 high, uint256 low) = product(x, y);
            if (high == 0) return low / denominator;
            if (denominator <= high) revert FullPrecisionMath__Overflow();

            uint256 remainder;
            assembly ("memory-safe") {
                remainder := mulmod(x, y, denominator)
                high := sub(high, gt(remainder, low))
                low := sub(low, remainder)
            }
            uint256 twos = denominator & (0 - denominator);
            assembly ("memory-safe") {
                denominator := div(denominator, twos)
                low := div(low, twos)
                twos := add(div(sub(0, twos), twos), 1)
            }
            low |= high * twos;
            uint256 inverse = (3 * denominator) ^ 2;
            inverse *= 2 - denominator * inverse;
            inverse *= 2 - denominator * inverse;
            inverse *= 2 - denominator * inverse;
            inverse *= 2 - denominator * inverse;
            inverse *= 2 - denominator * inverse;
            inverse *= 2 - denominator * inverse;
            return low * inverse;
        }
    }

    /// @dev Compare products without overflowing either uint256 intermediate.
    function productLessThanOrEqual(uint256 a, uint256 b, uint256 c, uint256 d) internal pure returns (bool) {
        (uint256 leftHigh, uint256 leftLow) = product(a, b);
        (uint256 rightHigh, uint256 rightLow) = product(c, d);
        return leftHigh < rightHigh || (leftHigh == rightHigh && leftLow <= rightLow);
    }
}
