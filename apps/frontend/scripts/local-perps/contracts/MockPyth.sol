// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

/// Local-fork fixture only. Installed at Pyth's address using anvil_setCode.
/// The production oracle and trading contracts remain unchanged.
contract LocalPyth {
    struct Price { int64 price; uint64 conf; int32 expo; uint256 publishTime; }
    struct Feed { bytes32 id; Price price; Price emaPrice; }
    struct Config { uint256 base; bool inverse; }
    mapping(bytes32 => Config) private config;
    uint256 public target;
    function configure(bytes32 id, uint256 base, bool inverse) external { config[id] = Config(base, inverse); }
    function setTarget(uint256 raw) external { require(raw > 0 && raw < 2e8); target = raw; }
    function getUpdateFee(bytes[] calldata) external pure returns (uint256) { return 0; }
    function updatePriceFeeds(bytes[] calldata) external payable {}
    function priceAt(bytes32 id, uint256 time) public view returns (Price memory) {
        Config memory c = config[id];
        uint256 normalized = c.base * target / 1e8;
        require(normalized > 0, "unconfigured feed");
        // Extra precision keeps the six-feed basket close to the target.
        uint256 value = c.inverse ? 1e20 / normalized : normalized * 1e4;
        require(value <= uint256(uint64(type(int64).max)), "price overflow");
        return Price(int64(uint64(value)), 0, -12, time);
    }
    function getPriceUnsafe(bytes32 id) external view returns (Price memory) { return priceAt(id, block.timestamp); }
    function parsePriceFeedUpdatesUnique(bytes[] calldata, bytes32[] calldata ids, uint64 minTime, uint64 maxTime)
        external payable returns (Feed[] memory feeds) {
        require(minTime <= maxTime && minTime <= block.timestamp);
        feeds = new Feed[](ids.length);
        for (uint256 i; i < ids.length; ++i) {
            Price memory p = priceAt(ids[i], minTime);
            feeds[i] = Feed(ids[i], p, p);
        }
    }
}
