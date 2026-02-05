import { describe, it, expect, beforeEach } from 'vitest'
import { useSettingsStore } from '../settingsStore'
import {
  DEFAULT_SLIPPAGE,
  MAX_SLIPPAGE,
  DEFAULT_MAX_PRICE_IMPACT,
  MAX_PRICE_IMPACT,
} from '../../config/constants'

describe('settingsStore', () => {
  beforeEach(() => {
    useSettingsStore.setState({
      slippage: DEFAULT_SLIPPAGE,
      maxPriceImpact: DEFAULT_MAX_PRICE_IMPACT,
    })
  })

  describe('initial state', () => {
    it('has correct default slippage', () => {
      const { slippage } = useSettingsStore.getState()
      expect(slippage).toBe(DEFAULT_SLIPPAGE)
    })

    it('has correct default maxPriceImpact', () => {
      const { maxPriceImpact } = useSettingsStore.getState()
      expect(maxPriceImpact).toBe(DEFAULT_MAX_PRICE_IMPACT)
    })
  })

  describe('setSlippage', () => {
    it('sets slippage to a valid value', () => {
      const { setSlippage } = useSettingsStore.getState()
      setSlippage(0.75)

      const { slippage } = useSettingsStore.getState()
      expect(slippage).toBe(0.75)
    })

    it('clamps slippage to MAX_SLIPPAGE when exceeded', () => {
      const { setSlippage } = useSettingsStore.getState()
      setSlippage(5.0)

      const { slippage } = useSettingsStore.getState()
      expect(slippage).toBe(MAX_SLIPPAGE)
    })

    it('allows setting slippage to exactly MAX_SLIPPAGE', () => {
      const { setSlippage } = useSettingsStore.getState()
      setSlippage(MAX_SLIPPAGE)

      const { slippage } = useSettingsStore.getState()
      expect(slippage).toBe(MAX_SLIPPAGE)
    })

    it('allows setting slippage to 0', () => {
      const { setSlippage } = useSettingsStore.getState()
      setSlippage(0)

      const { slippage } = useSettingsStore.getState()
      expect(slippage).toBe(0)
    })

    it('allows setting slippage to values below MAX_SLIPPAGE', () => {
      const { setSlippage } = useSettingsStore.getState()
      setSlippage(0.5)

      const { slippage } = useSettingsStore.getState()
      expect(slippage).toBe(0.5)
    })
  })

  describe('setMaxPriceImpact', () => {
    it('sets maxPriceImpact to a valid value', () => {
      const { setMaxPriceImpact } = useSettingsStore.getState()
      setMaxPriceImpact(3.0)

      const { maxPriceImpact } = useSettingsStore.getState()
      expect(maxPriceImpact).toBe(3.0)
    })

    it('clamps maxPriceImpact to MAX_PRICE_IMPACT when exceeded', () => {
      const { setMaxPriceImpact } = useSettingsStore.getState()
      setMaxPriceImpact(10.0)

      const { maxPriceImpact } = useSettingsStore.getState()
      expect(maxPriceImpact).toBe(MAX_PRICE_IMPACT)
    })

    it('allows setting maxPriceImpact to exactly MAX_PRICE_IMPACT', () => {
      const { setMaxPriceImpact } = useSettingsStore.getState()
      setMaxPriceImpact(MAX_PRICE_IMPACT)

      const { maxPriceImpact } = useSettingsStore.getState()
      expect(maxPriceImpact).toBe(MAX_PRICE_IMPACT)
    })

    it('allows setting maxPriceImpact to 0', () => {
      const { setMaxPriceImpact } = useSettingsStore.getState()
      setMaxPriceImpact(0)

      const { maxPriceImpact } = useSettingsStore.getState()
      expect(maxPriceImpact).toBe(0)
    })

    it('allows setting maxPriceImpact to values below MAX_PRICE_IMPACT', () => {
      const { setMaxPriceImpact } = useSettingsStore.getState()
      setMaxPriceImpact(1.5)

      const { maxPriceImpact } = useSettingsStore.getState()
      expect(maxPriceImpact).toBe(1.5)
    })
  })

  describe('multiple updates', () => {
    it('updates slippage and maxPriceImpact independently', () => {
      const { setSlippage, setMaxPriceImpact } = useSettingsStore.getState()

      setSlippage(0.8)
      setMaxPriceImpact(4.0)

      const state = useSettingsStore.getState()
      expect(state.slippage).toBe(0.8)
      expect(state.maxPriceImpact).toBe(4.0)
    })

    it('preserves other state when updating one field', () => {
      const { setSlippage, setMaxPriceImpact } = useSettingsStore.getState()

      setMaxPriceImpact(3.5)
      const afterPriceImpact = useSettingsStore.getState().slippage

      setSlippage(0.9)
      const afterSlippage = useSettingsStore.getState().maxPriceImpact

      expect(afterPriceImpact).toBe(DEFAULT_SLIPPAGE)
      expect(afterSlippage).toBe(3.5)
    })
  })
})
