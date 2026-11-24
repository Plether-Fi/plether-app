// SPDX-License-Identifier: AGPL-3.0
pragma solidity 0.8.30;

/// @dev A small target to prove execution from the Kernel address in both gas modes.
contract QualificationTarget {
    mapping(address account => uint256 count) public calls;

    function record() external {
        ++calls[msg.sender];
    }
}
