export const EIP2612_PERMIT_TYPES = {
  Permit: [
    { name: 'owner', type: 'address' },
    { name: 'spender', type: 'address' },
    { name: 'value', type: 'uint256' },
    { name: 'nonce', type: 'uint256' },
    { name: 'deadline', type: 'uint256' },
  ],
} as const

export function splitSignature(signature: `0x${string}`): {
  r: `0x${string}`
  s: `0x${string}`
  v: number
} {
  const r: `0x${string}` = signature.slice(0, 66) as `0x${string}`
  const s: `0x${string}` = `0x${signature.slice(66, 130)}`
  const v = parseInt(signature.slice(130, 132), 16)
  return { r, s, v }
}
