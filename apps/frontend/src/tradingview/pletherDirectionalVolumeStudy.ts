import { PLDXY_DIRECTIONAL_VOLUME_SYMBOL } from './pletherDatafeed'
import type {
  TradingViewCustomIndicator,
  TradingViewCustomIndicatorInstance,
  TradingViewPineContext,
  TradingViewPineJs,
} from './types'

export const PLETHER_DIRECTIONAL_VOLUME_STUDY_NAME = 'Plether Directional Volume'

const LONG_COLOR = '#00FF99'
const SHORT_COLOR = '#FF572D'

/**
 * TradingView custom studies can read another datafeed symbol. The hidden
 * directional-volume symbol stores flow toward long exposure in `close` and
 * flow toward short exposure in `volume`; this study aligns those values with
 * the visible price series and plots short flow below zero.
 */
export function createPletherDirectionalVolumeStudy(
  pineJs: TradingViewPineJs
): TradingViewCustomIndicator {
  class PletherDirectionalVolumeIndicator implements TradingViewCustomIndicatorInstance {
    init(context: TradingViewPineContext): void {
      context.new_sym(PLDXY_DIRECTIONAL_VOLUME_SYMBOL, pineJs.Std.period(context))
    }

    main(context: TradingViewPineContext): number[] {
      context.select_sym(0)
      const mainTime = context.new_var(context.symbol.time)

      context.select_sym(1)
      const sourceTime = context.new_var(context.symbol.time)
      const longFlow = context
        .new_var(pineJs.Std.close(context))
        .adopt(sourceTime, mainTime, 1)
      const shortFlow = context
        .new_var(pineJs.Std.volume(context))
        .adopt(sourceTime, mainTime, 1)

      context.select_sym(0)
      return [longFlow, -shortFlow]
    }
  }

  return {
    name: PLETHER_DIRECTIONAL_VOLUME_STUDY_NAME,
    metainfo: {
      _metainfoVersion: 53,
      id: `${PLETHER_DIRECTIONAL_VOLUME_STUDY_NAME}@tv-basicstudies-1`,
      name: PLETHER_DIRECTIONAL_VOLUME_STUDY_NAME,
      description: PLETHER_DIRECTIONAL_VOLUME_STUDY_NAME,
      shortDescription: 'Directional flow · Long + / Short −',
      isCustomIndicator: true,
      is_price_study: false,
      format: { type: 'volume', precision: 2 },
      plots: [
        { id: 'long_flow', type: 'line' },
        { id: 'short_flow', type: 'line' },
      ],
      defaults: {
        styles: {
          long_flow: {
            color: LONG_COLOR,
            linestyle: 0,
            linewidth: 1,
            plottype: 5,
            trackPrice: false,
            transparency: 12,
            visible: true,
          },
          short_flow: {
            color: SHORT_COLOR,
            linestyle: 0,
            linewidth: 1,
            plottype: 5,
            trackPrice: false,
            transparency: 12,
            visible: true,
          },
        },
        inputs: {},
        precision: 2,
      },
      styles: {
        long_flow: {
          title: 'Toward long',
          histogramBase: 0,
          joinPoints: false,
        },
        short_flow: {
          title: 'Toward short',
          histogramBase: 0,
          joinPoints: false,
        },
      },
      inputs: [],
    },
    constructor: PletherDirectionalVolumeIndicator,
  }
}

export function getPletherCustomIndicators(
  pineJs: TradingViewPineJs
): Promise<TradingViewCustomIndicator[]> {
  return Promise.resolve([createPletherDirectionalVolumeStudy(pineJs)])
}
