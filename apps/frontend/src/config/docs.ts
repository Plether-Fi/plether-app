const DOCS_BASE_URL = 'https://docs.plether.com'

export const DOCS_LINKS = {
  perpsPrice: {
    href: `${DOCS_BASE_URL}/welcome/understanding-the-plether-dollar-index#the-mark-and-execution-economics-can-differ`,
    title: 'The mark and execution economics can differ',
  },
  direction: {
    href: `${DOCS_BASE_URL}/welcome/understanding-the-plether-dollar-index#why-the-raw-basket-moves-opposite-to-the-dollar`,
    title: 'Why the raw basket moves opposite to the dollar',
  },
  poolLiquidity: {
    href: `${DOCS_BASE_URL}/how-plether-works/the-housepool-and-tranche-waterfall`,
    title: 'The HousePool and tranche waterfall',
  },
  marketCostOfCarry: {
    href: `${DOCS_BASE_URL}/how-plether-works/trading-costs-fees-carry-and-vpi#cost-of-carry`,
    title: 'Cost of carry',
  },
  entryNotional: {
    href: `${DOCS_BASE_URL}/trading-on-plether-perps/read-your-position-and-account-health#exposure-and-entry-notional`,
    title: 'Exposure and entry notional',
  },
  positionLeverage: {
    href: `${DOCS_BASE_URL}/how-plether-works/margin-leverage-and-liquidation#position-leverage-and-effective-account-leverage`,
    title: 'Position leverage and effective account leverage',
  },
  liquidationPrice: {
    href: `${DOCS_BASE_URL}/how-plether-works/margin-leverage-and-liquidation#liquidation-price`,
    title: 'Liquidation price',
  },
  unrealizedPnl: {
    href: `${DOCS_BASE_URL}/trading-on-plether-perps/read-your-position-and-account-health#unrealized-pnl`,
    title: 'Unrealized PnL',
  },
  positionCostOfCarry: {
    href: `${DOCS_BASE_URL}/trading-on-plether-perps/read-your-position-and-account-health#cost-of-carry`,
    title: 'Cost of carry',
  },
  virtualPriceImpact: {
    href: `${DOCS_BASE_URL}/how-plether-works/trading-costs-fees-carry-and-vpi#virtual-price-impact`,
    title: 'Virtual Price Impact',
  },
  oracleConfidence: {
    href: `${DOCS_BASE_URL}/how-plether-works/trading-costs-fees-carry-and-vpi#oracle-confidence-adjustment`,
    title: 'Oracle confidence adjustment',
  },
  frozenCloseSpread: {
    href: `${DOCS_BASE_URL}/trading-on-plether-perps/reduce-or-close-a-position#closing-while-the-oracle-is-frozen`,
    title: 'Closing while the oracle is frozen',
  },
  reduceOnly: {
    href: `${DOCS_BASE_URL}/trading-on-plether-perps/reduce-or-close-a-position#reduce-or-close`,
    title: 'Reduce or close',
  },
  marginCallSimulator: {
    href: `${DOCS_BASE_URL}/how-plether-works/margin-leverage-and-liquidation#margin-call-simulator`,
    title: 'Margin Call Simulator',
  },
  contractNotional: {
    href: `${DOCS_BASE_URL}/how-plether-works/margin-leverage-and-liquidation#the-notional-used-for-margin`,
    title: 'The notional used for margin',
  },
  executionLimit: {
    href: `${DOCS_BASE_URL}/how-plether-works/how-orders-execute#acceptable-price-protection`,
    title: 'Acceptable-price protection',
  },
  maintenanceMargin: {
    href: `${DOCS_BASE_URL}/how-plether-works/margin-leverage-and-liquidation#maintenance-margin`,
    title: 'Maintenance margin',
  },
  executionReward: {
    href: `${DOCS_BASE_URL}/how-plether-works/trading-costs-fees-carry-and-vpi#the-order-execution-reward`,
    title: 'The order execution reward',
  },
  manualFinalization: {
    href: `${DOCS_BASE_URL}/how-plether-works/how-orders-execute#who-finalizes-the-order`,
    title: 'Who finalizes the order?',
  },
  withdrawable: {
    href: `${DOCS_BASE_URL}/trading-on-plether-perps/your-margin-account#available-to-trade-and-withdrawable`,
    title: 'Available to Trade and Withdrawable',
  },
} as const
