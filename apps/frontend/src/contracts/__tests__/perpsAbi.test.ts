import { describe, expect, it } from 'vitest'
import { PERPS_CFD_ENGINE_LENS_ABI } from '../abis'

describe('perps CFD engine lens ABI', () => {
  it('keeps the frozen close spread preview fields appended after the legacy tuple', () => {
    const previewClose = PERPS_CFD_ENGINE_LENS_ABI.find((item) => (
      item.type === 'function' && item.name === 'previewClose'
    ))
    const output = previewClose?.outputs[0]

    expect(output && 'components' in output ? output.components.slice(-3) : undefined).toEqual([
      { name: 'frozenSpreadUsdc', type: 'uint256' },
      { name: 'frozenSpreadPaidUsdc', type: 'uint256' },
      { name: 'frozenSpreadWaivedUsdc', type: 'uint256' },
    ])
    expect(output && 'components' in output ? output.components : []).toHaveLength(24)
  })
})

// Extracted verbatim from the checksum-verified v1.2.1 release ABI bundle.
import releaseAbi from './fixtures/perps-v1.2.1.json'
import {
  PERPS_CFD_ENGINE_ABI,
  PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI,
} from '../abis'

describe('v1.2.1 changed return layouts', () => {
  const bindings = {
    riskParams: PERPS_CFD_ENGINE_ABI,
    previewLiquidation: PERPS_CFD_ENGINE_LENS_ABI,
    getAccountLedgerSnapshot: PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI,
    getAccountCollateralView: PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI,
  }
  type Parameter = { name?: string; type: string; components?: readonly Parameter[] }
  const layout = (parameters: readonly Parameter[]): unknown => parameters.map(parameter => ({
    name: parameter.name,
    type: parameter.type,
    ...(parameter.components ? { components: layout(parameter.components) } : {}),
  }))

  for (const name of Object.keys(bindings) as (keyof typeof bindings)[]) {
    it(`matches the released ${name} output fields and order`, () => {
      const binding = bindings[name].find(item => item.type === 'function' && item.name === name)
      expect(binding && 'outputs' in binding).toBe(true)
      if (!binding || !('outputs' in binding)) return
      const expected = releaseAbi[name].outputs
      // The outer tuple's Solidity variable name is not part of its decoded value.
      const fields = (outputs: readonly Parameter[]) => outputs[0]?.components ?? outputs
      expect(layout(fields(binding.outputs))).toEqual(layout(fields(expected)))
    })
  }
})
