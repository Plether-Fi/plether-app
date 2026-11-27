import { describe, expect, it } from 'vitest'
import { buildAddressAvatarInnerSvg, deriveAddressAvatar } from './addressAvatar'

describe('3x3 Bauhaus address avatar', () => {
  it('is deterministic and case-insensitive for the same EVM address', () => {
    const lower = '0xfe2c91e9d193a5de484f6b4a7cca3dc05a3533c2'
    const mixed = '0xFE2C91E9d193a5De484f6b4A7cca3dc05a3533C2'
    expect(deriveAddressAvatar(lower)).toEqual(deriveAddressAvatar(mixed))
    expect(buildAddressAvatarInnerSvg(lower)).toBe(buildAddressAvatarInnerSvg(mixed))
  })

  it('produces horizontally symmetric 3x3 tiles with distinct primary/secondary colors and >= 4 filled cells', () => {
    const samples = [
      '0xfe2c91e9d193a5de484f6b4a7cca3dc05a3533c2',
      '0x0000000000000000000000000000000000000001',
      '0x0000000000000000000000000000000000000002',
      '0x7051a80000000000000000000000000000002c80',
      '0x5c29000000000000000000000000000000000fc3',
    ]
    const rendered = new Set<string>()
    for (const addr of samples) {
      const spec = deriveAddressAvatar(addr)
      expect(spec.primary).not.toBe(spec.secondary)
      expect(spec.cells).toHaveLength(9)
      const filled = spec.cells.filter((c) => c.kind !== 'empty').length
      expect(filled).toBeGreaterThanOrEqual(4)
      for (let row = 0; row < 3; row += 1) {
        const left = spec.cells.find((c) => c.row === row && c.col === 0)!
        const right = spec.cells.find((c) => c.row === row && c.col === 2)!
        expect(left.kind).toBe(right.kind)
        expect(left.color).toBe(right.color)
      }
      rendered.add(buildAddressAvatarInnerSvg(addr))
    }
    expect(rendered.size).toBe(samples.length)
  })
})
