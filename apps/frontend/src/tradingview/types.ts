export type TradingViewResolution = '1' | '3' | '5' | '15' | '30' | '60' | '1D'

export type TradingViewColorGradient = [
  string,
  string,
  string,
  string,
  string,
  string,
  string,
  string,
  string,
  string,
  string,
  string,
  string,
  string,
  string,
  string,
  string,
  string,
  string,
]

export interface TradingViewCustomThemeColors {
  color1: TradingViewColorGradient
  color2: TradingViewColorGradient
  color3: TradingViewColorGradient
  color4: TradingViewColorGradient
  color5: TradingViewColorGradient
  color6: TradingViewColorGradient
  color7: TradingViewColorGradient
  white: string
  black: string
}

export interface TradingViewCustomThemes {
  light: TradingViewCustomThemeColors
  dark: TradingViewCustomThemeColors
}

export interface TradingViewBar {
  time: number
  open: number
  high: number
  low: number
  close: number
  volume?: number
}

export interface TradingViewPineSeries {
  adopt: (
    sourceTime: TradingViewPineSeries,
    targetTime: TradingViewPineSeries,
    mode: number
  ) => number
}

export interface TradingViewPineContext {
  symbol: { time: number }
  new_sym: (symbol: string, period: string) => void
  new_var: (value: number) => TradingViewPineSeries
  select_sym: (index: number) => void
}

export interface TradingViewPineJs {
  Std: {
    close: (context: TradingViewPineContext) => number
    period: (context: TradingViewPineContext) => string
    volume: (context: TradingViewPineContext) => number
  }
}

export interface TradingViewCustomIndicatorInstance {
  init: (context: TradingViewPineContext, inputCallback: () => unknown) => void
  main: (context: TradingViewPineContext, inputCallback: () => unknown) => number[]
}

export interface TradingViewCustomIndicator {
  name: string
  metainfo: Record<string, unknown>
  constructor: new () => TradingViewCustomIndicatorInstance
}

export interface TradingViewPeriodParams {
  from: number
  to: number
  countBack: number
  firstDataRequest: boolean
}

export interface TradingViewDatafeedConfiguration {
  supported_resolutions: TradingViewResolution[]
  supports_marks: boolean
  supports_timescale_marks: boolean
  supports_time: boolean
  exchanges: { value: string; name: string; desc: string }[]
  symbols_types: { name: string; value: string }[]
}

export interface TradingViewSymbolInfo {
  name: string
  ticker: string
  description: string
  type: string
  session: string
  timezone: string
  exchange: string
  listed_exchange: string
  format: 'price'
  pricescale: number
  minmov: number
  has_intraday: boolean
  intraday_multipliers: string[]
  has_daily: boolean
  daily_multipliers: string[]
  supported_resolutions: TradingViewResolution[]
  data_status: 'streaming'
  visible_plots_set: 'ohlc' | 'ohlcv'
  volume_precision?: number
  has_empty_bars?: boolean
}

export interface TradingViewSearchResult {
  symbol: string
  full_name: string
  description: string
  exchange: string
  ticker: string
  type: string
}

export interface TradingViewHistoryMetadata {
  noData: boolean
}

export interface TradingViewCustomStatusDropDownContent {
  title: string
  color?: string
  icon?: string
  content: string[]
}

export interface TradingViewCustomSymbolStatusAdapter {
  setVisible: (visible: boolean) => TradingViewCustomSymbolStatusAdapter
  setIcon: (icon: string | null) => TradingViewCustomSymbolStatusAdapter
  setColor: (color: string) => TradingViewCustomSymbolStatusAdapter
  setTooltip: (tooltip: string | null) => TradingViewCustomSymbolStatusAdapter
  setDropDownContent: (
    content: TradingViewCustomStatusDropDownContent[] | null
  ) => TradingViewCustomSymbolStatusAdapter
}

export interface TradingViewCustomSymbolStatusApi {
  symbol: (symbolId: string) => TradingViewCustomSymbolStatusAdapter
  hideAll: () => void
}

export interface TradingViewDatafeed {
  onReady: (callback: (configuration: TradingViewDatafeedConfiguration) => void) => void
  searchSymbols: (
    userInput: string,
    exchange: string,
    symbolType: string,
    onResult: (results: TradingViewSearchResult[]) => void
  ) => void
  resolveSymbol: (
    symbolName: string,
    onResolve: (symbolInfo: TradingViewSymbolInfo) => void,
    onError: (message: string) => void
  ) => void
  getBars: (
    symbolInfo: TradingViewSymbolInfo,
    resolution: TradingViewResolution,
    periodParams: TradingViewPeriodParams,
    onResult: (bars: TradingViewBar[], metadata: TradingViewHistoryMetadata) => void,
    onError: (message: string) => void
  ) => void
  subscribeBars: (
    symbolInfo: TradingViewSymbolInfo,
    resolution: TradingViewResolution,
    onTick: (bar: TradingViewBar) => void,
    listenerGuid: string,
    onResetCacheNeededCallback: () => void
  ) => void
  unsubscribeBars: (listenerGuid: string) => void
}

export interface TradingViewWidgetOptions {
  container: HTMLElement
  datafeed: TradingViewDatafeed
  interval: TradingViewResolution
  symbol: string
  library_path: string
  locale: string
  timezone: string
  timeframe: string
  autosize: boolean
  theme: 'dark'
  custom_themes: TradingViewCustomThemes
  toolbar_bg: string
  custom_css_url: string
  disabled_features: string[]
  enabled_features: string[]
  favorites: {
    intervals: TradingViewResolution[]
  }
  time_frames: {
    text: string
    resolution: TradingViewResolution
    description: string
    title?: string
  }[]
  custom_font_family: string
  loading_screen: {
    backgroundColor: string
    foregroundColor: string
  }
  overrides: Record<string, string | number | boolean>
  settings_overrides: Record<string, string | number | boolean>
  studies_overrides: Record<string, string | number | boolean>
  custom_indicators_getter?: (
    pineJs: TradingViewPineJs
  ) => Promise<TradingViewCustomIndicator[]>
}

export interface TradingViewWidget {
  chartReady: () => Promise<void>
  headerReady: () => Promise<void>
  activeChart: () => TradingViewChart
  customSymbolStatus: () => TradingViewCustomSymbolStatusApi
  remove: () => void
}

export type TradingViewEntityId = string

export interface TradingViewShapePoint {
  time?: number
  price?: number
}

export interface TradingViewCreateShapeOptions {
  shape: 'horizontal_line'
  text?: string
  lock?: boolean
  disableSelection?: boolean
  disableSave?: boolean
  disableUndo?: boolean
  showInObjectsTree?: boolean
  zOrder?: 'top' | 'bottom'
  overrides?: {
    linecolor?: string
    linestyle?: number
    linewidth?: number
    showPrice?: boolean
    textcolor?: string
    fontsize?: number
    bold?: boolean
    horzLabelsAlign?: string
    vertLabelsAlign?: string
  }
}

export interface TradingViewVisibleTimeRange {
  from: number
  to: number
}

export interface TradingViewTimeFrameValue {
  type: 'period-back' | 'time-range'
  value?: string
  from?: number
  to?: number
}

export interface TradingViewIntervalChangeParameters {
  timeframe?: TradingViewTimeFrameValue
}

export type TradingViewIntervalChangedCallback = (
  resolution: string,
  parameters?: TradingViewIntervalChangeParameters
) => void

export type TradingViewVisibleRangeChangedCallback = (
  range: TradingViewVisibleTimeRange
) => void

export interface TradingViewIntervalSubscription {
  subscribe: (context: object | null, callback: TradingViewIntervalChangedCallback) => void
  unsubscribe: (context: object | null, callback: TradingViewIntervalChangedCallback) => void
}

export interface TradingViewVisibleRangeSubscription {
  subscribe: (context: object | null, callback: TradingViewVisibleRangeChangedCallback) => void
  unsubscribe: (context: object | null, callback: TradingViewVisibleRangeChangedCallback) => void
}

export interface TradingViewChart {
  resetData: () => void
  resolution: () => string
  symbol: () => string
  getVisibleRange: () => TradingViewVisibleTimeRange
  setResolution: (resolution: TradingViewResolution) => Promise<boolean>
  onIntervalChanged: () => TradingViewIntervalSubscription
  onVisibleRangeChanged: () => TradingViewVisibleRangeSubscription
  createShape: (
    point: TradingViewShapePoint,
    options: TradingViewCreateShapeOptions
  ) => Promise<TradingViewEntityId>
  createStudy: (
    name: string,
    forceOverlay?: boolean,
    lock?: boolean
  ) => Promise<TradingViewEntityId | null>
  removeEntity: (entityId: TradingViewEntityId) => void
}

export interface TradingViewNamespace {
  widget: new (options: TradingViewWidgetOptions) => TradingViewWidget
}

declare global {
  interface Window {
    TradingView?: TradingViewNamespace
  }
}
