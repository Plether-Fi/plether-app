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
