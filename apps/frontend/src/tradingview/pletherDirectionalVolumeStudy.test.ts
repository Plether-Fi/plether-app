import { describe, expect, it, vi } from 'vitest'
import { PLDXY_DIRECTIONAL_VOLUME_SYMBOL } from './pletherDatafeed'
import {
  PLETHER_DIRECTIONAL_VOLUME_STUDY_NAME,
  createPletherDirectionalVolumeStudy,
} from './pletherDirectionalVolumeStudy'
import type { TradingViewPineContext, TradingViewPineJs } from './types'

describe('Plether directional-volume study', () => {
  it('plots flow toward long above zero and flow toward short below zero', () => {
    const newSymbol = vi.fn()
    const context: TradingViewPineContext = {
      symbol: { time: 1_000 },
      new_sym: newSymbol,
      select_sym: (index) => {
        context.symbol.time = index === 0 ? 1_000 : 995
      },
      new_var: (value) => ({ adopt: () => value }),
    }
    const pineJs: TradingViewPineJs = {
      Std: {
        period: () => '5',
        close: () => 12,
        volume: () => 5,
      },
    }
    const definition = createPletherDirectionalVolumeStudy(pineJs)
    const indicator = new definition.constructor()

    indicator.init(context, () => undefined)

    expect(newSymbol).toHaveBeenCalledWith(PLDXY_DIRECTIONAL_VOLUME_SYMBOL, '5')
    expect(indicator.main(context, () => undefined)).toEqual([12, -5])
    expect(definition.name).toBe(PLETHER_DIRECTIONAL_VOLUME_STUDY_NAME)
    expect(definition.metainfo.shortDescription).toBe(
      'Directional flow · Long + / Short −'
    )
  })
})
