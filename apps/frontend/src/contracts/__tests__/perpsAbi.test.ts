import { createHash } from 'node:crypto'
import { describe, expect, it } from 'vitest'
import type { Abi } from 'viem'
import * as perpsBindings from '../abis/Perps'
import { TRANCHE_VAULT_READ_ABI } from '../abis/TrancheVault'
import sourceBindings from './fixtures/perps-canonical-source-abis.json'
import provenance from './fixtures/perps-canonical-source-abis.provenance.json'

function withoutInternalType(value: unknown): unknown {
  if (Array.isArray(value)) return value.map(withoutInternalType)
  if (value && typeof value === 'object') {
    return Object.fromEntries(Object.entries(value)
      .filter(([key]) => key !== 'internalType')
      .map(([key, child]) => [key, withoutInternalType(child)]))
  }
  return value
}

// These are compiled-source checks. Historical release fixtures remain unchanged
// and are not evidence that the prepared deployment exists onchain.
describe('canonical perps consumer ABIs', () => {
  const bindings = { ...perpsBindings, TRANCHE_VAULT_READ_ABI }

  for (const [name, source] of Object.entries(sourceBindings)) {
    it(`matches the compiled fields, overloads and errors used by ${name}`, () => {
      const abi = bindings[name as keyof typeof bindings] as Abi
      const compiled = withoutInternalType(source) as Abi
      expect(abi.length).toBeGreaterThan(0)
      for (const entry of abi) expect(compiled).toContainEqual(entry)
      expect(abi.filter(entry => entry.type === 'error'))
        .toEqual(compiled.filter(entry => entry.type === 'error'))
      expect(JSON.stringify(abi)).not.toMatch(/usdc/i)
    })

    it(`records the actual compiled source digest for ${name}`, () => {
      const digest = createHash('sha256').update(JSON.stringify(source)).digest('hex')
      expect(Object.values(provenance.contracts).some(contract => contract.abiSha256 === digest)).toBe(true)
    })
  }

  it('records source provenance without claiming an existing deployment', () => {
    expect(provenance.kind).toBe('compiled-source-abis')
    expect(provenance.deployed).toBe(false)
    for (const contract of Object.values(provenance.contracts)) {
      expect(contract.compilerVersion).toMatch(/^0\.8\.35\+/)
      expect(contract.sourceSha256).toMatch(/^[a-f0-9]{64}$/)
      expect(contract.validation).toContain('runtime deployment not verified')
      expect(contract).not.toHaveProperty('runtimeCodeHash')
    }
  })
})
