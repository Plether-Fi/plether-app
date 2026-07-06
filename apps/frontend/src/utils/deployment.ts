const TESTNET_HOSTS = new Set([
  'app.sepolia.plether.com',
  'sepolia.plether.com',
  'plether-testnet.pages.dev',
])

const PRIMARY_APP_HOSTS = new Set(['app.plether.com'])
const LOCAL_HOSTS = new Set(['localhost', '127.0.0.1'])
const TESTNET_CHAIN_IDS = new Set([11155111, 421614])

function parseChainId(value: string | undefined): number {
  const parsed = Number(value)
  return Number.isInteger(parsed) ? parsed : 1
}

export function isSepoliaDeployment(
  hostname = window.location.hostname,
  defaultChainId = parseChainId(import.meta.env.VITE_DEFAULT_CHAIN_ID as string | undefined)
): boolean {
  const normalized = hostname.toLowerCase()
  if (TESTNET_HOSTS.has(normalized)) return true
  if (LOCAL_HOSTS.has(normalized)) return TESTNET_CHAIN_IDS.has(defaultChainId)
  return false
}

export function isPrimaryAppDeployment(hostname = window.location.hostname): boolean {
  return PRIMARY_APP_HOSTS.has(hostname.toLowerCase())
}
