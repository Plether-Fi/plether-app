import { type Address } from 'viem'

export interface ContractAddresses {
  USDC: Address
  DXY_BEAR: Address
  DXY_BULL: Address
  SDXY_BEAR: Address
  SDXY_BULL: Address
  SYNTHETIC_SPLITTER: Address
  CURVE_POOL: Address
  ZAP_ROUTER: Address
  LEVERAGE_ROUTER: Address
  BULL_LEVERAGE_ROUTER: Address
  STAKING_BEAR: Address
  STAKING_BULL: Address
  BASKET_ORACLE: Address
  MOCK_ADAPTER: Address
  MORPHO_ORACLE_BEAR: Address
  MORPHO_ORACLE_BULL: Address
  STAKED_ORACLE_BEAR: Address
  STAKED_ORACLE_BULL: Address
}

// Load addresses from JSON files at build time
const addressModules = import.meta.glob<{ default: Record<string, string> }>(
  './addresses.*.json',
  { eager: true }
)

function loadAddresses(filename: string): ContractAddresses | null {
  const module = addressModules[`./${filename}`] as { default: ContractAddresses } | undefined
  return module?.default ?? null
}

const MAINNET_ADDRESSES = loadAddresses('addresses.mainnet.json')
const LOCAL_ADDRESSES = loadAddresses('addresses.local.json')

const sepoliaAddresses = loadAddresses('addresses.sepolia.json')
if (!sepoliaAddresses) {
  throw new Error('Sepolia addresses not found')
}
export const SEPOLIA_ADDRESSES = sepoliaAddresses

export function getAddresses(chainId: number): ContractAddresses {
  switch (chainId) {
    case 1:
      if (!MAINNET_ADDRESSES) {
        throw new Error('Mainnet addresses not found')
      }
      return MAINNET_ADDRESSES
    case 11155111:
      return SEPOLIA_ADDRESSES
    case 31337:
      if (!LOCAL_ADDRESSES) {
        console.warn('Local addresses not found. Copy addresses.local.example.json to addresses.local.json')
        return SEPOLIA_ADDRESSES
      }
      return LOCAL_ADDRESSES
    default:
      if (!MAINNET_ADDRESSES) {
        return SEPOLIA_ADDRESSES
      }
      return MAINNET_ADDRESSES
  }
}
