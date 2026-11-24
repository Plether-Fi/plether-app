export function selectedKernelTestOwner(owner: string | undefined, configured: unknown, entry: URL | undefined): boolean {
  if (!owner || typeof configured !== 'string' || entry?.origin !== 'https://app.sepolia.plether.com' || entry.pathname !== '/') return false
  const choices = entry.searchParams.getAll('kernel-test')
  if (choices.length !== 1 || choices[0] !== '1') return false
  const owners = configured.toLowerCase().split(',')
  return owners.length === 2 && new Set(owners).size === 2 &&
    owners.every(address => /^0x[0-9a-f]{40}$/.test(address)) &&
    owners.includes(owner.toLowerCase())
}
