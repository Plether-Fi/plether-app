export const kernelTestMounted = import.meta.env.BASE_URL === '/kernel-test/'

export function kernelTestOwnerAllowed(owner: string, configured: unknown): boolean {
  if (typeof configured !== 'string') return false
  const owners = configured.toLowerCase().split(',')
  return owners.length === 2 && new Set(owners).size === 2 &&
    owners.every(address => /^0x[0-9a-f]{40}$/.test(address)) &&
    owners.includes(owner.toLowerCase())
}
