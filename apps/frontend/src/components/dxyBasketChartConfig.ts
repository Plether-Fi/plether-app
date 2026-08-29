export type DxyBasketChartInterval = '1m' | '5m' | '15m' | '1h' | '1d'

// Component snapshots are only used for the 24-hour comparison. Hourly
// samples avoid transferring every one-minute, six-component snapshot.
export const DXY_COMPONENT_CHANGE_HISTORY_INTERVAL_SECONDS = 60 * 60

export const DEFAULT_DXY_BASKET_CHART_INTERVAL: DxyBasketChartInterval = '15m'
