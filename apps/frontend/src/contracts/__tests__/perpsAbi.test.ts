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

// Extracted verbatim from the checksum-verified v1.2.2 release ABI bundle.
import releaseAbi from './fixtures/perps-v1.2.2.json'
import {
  PERPS_CFD_ENGINE_ABI,
  PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI,
} from '../abis'

describe('v1.2.2 return layouts', () => {
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

import { createHash } from 'node:crypto'
import type { Abi, AbiParameter } from 'viem'
import * as perpsBindings from '../abis/Perps'
import { TRANCHE_VAULT_READ_ABI } from '../abis/TrancheVault'
import releaseBindings from './fixtures/perps-v1.2.2-bindings.json'

// Digests are computed from the matching entries in the checksum-verified
// release bundle. Parameter names and Solidity internalType do not affect the
// wire format; tuple order, event indexing, mutability and types do.
describe('v1.2.2 application ABI compatibility', () => {
  const bindings = { ...perpsBindings, TRANCHE_VAULT_READ_ABI }
  const parameter = (value: AbiParameter & { indexed?: boolean }): unknown => ({
    type: value.type,
    ...(value.indexed !== undefined ? { indexed: value.indexed } : {}),
    ...('components' in value ? { components: value.components.map(parameter) } : {}),
  })
  for (const [name, expected] of Object.entries(releaseBindings.bindings)) {
    it(`matches all released entries used by ${name}`, () => {
      const abi = bindings[name as keyof typeof bindings] as Abi
      const layouts = abi.map(entry => ({
        type: entry.type,
        name: 'name' in entry ? entry.name : undefined,
        inputs: 'inputs' in entry ? entry.inputs.map(parameter) : undefined,
        outputs: 'outputs' in entry ? entry.outputs.map(parameter) : undefined,
        stateMutability: 'stateMutability' in entry ? entry.stateMutability : undefined,
        anonymous: entry.type === 'event' ? (entry.anonymous ?? false) : undefined,
      })).sort((a, b) => JSON.stringify(a).localeCompare(JSON.stringify(b)))
      expect(abi).toHaveLength(expected.entries)
      expect(createHash('sha256').update(JSON.stringify(layouts)).digest('hex'))
        .toBe(expected.layoutSha256)
    })
  }
})
