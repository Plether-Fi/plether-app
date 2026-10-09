import { keccak256, parseAbi, type PublicClient } from 'viem'
import type { FundingDestination, FundingManifest, FundingQuote } from './types'
import { assertDestination, sameAddress } from './validation'

const FACTORY_ABI = parseAbi([
  'function clearinghouse() view returns (address)',
  'function usdc() view returns (address)',
  'function predictReceiver(address beneficiary,bytes32 intentSalt) view returns (address)',
])
const RECEIVER_ABI = parseAbi([
  'function beneficiary() view returns (address)',
  'function clearinghouse() view returns (address)',
  'function usdc() view returns (address)',
])
export type FundingReadClient = Pick<PublicClient, 'getChainId' | 'getBlockNumber' | 'getCode' | 'readContract'>

/** Bind the API's receiver to the pinned beneficiary using the reviewed onchain factory. */
export async function verifyFundingReceiver(client: FundingReadClient | undefined, manifest: FundingManifest, quote: FundingQuote, destination: FundingDestination): Promise<void> {
  assertDestination(quote, destination)
  if (manifest.releaseId !== destination.releaseId || manifest.destinationChainId !== destination.destinationChainId || !sameAddress(manifest.receiverFactory, destination.receiverFactory) || !sameAddress(manifest.clearinghouse, destination.clearinghouse) || !sameAddress(manifest.token, destination.token)) throw new Error('The pinned funding destination does not match the reviewed release.')
  if (!client) throw new Error('The destination RPC is unavailable. No source transaction was requested.')
  const chainId = await client.getChainId()
  if (chainId !== manifest.destinationChainId) throw new Error('The destination RPC is connected to the wrong chain.')
  const blockNumber = await client.getBlockNumber()
  const [factoryCode, clearinghouseCode] = await Promise.all([
    client.getCode({ address: manifest.receiverFactory, blockNumber }),
    client.getCode({ address: manifest.clearinghouse, blockNumber }),
  ])
  if (!factoryCode || factoryCode === '0x' || keccak256(factoryCode).toLowerCase() !== manifest.factoryCodeHash.toLowerCase() || !clearinghouseCode || clearinghouseCode === '0x' || keccak256(clearinghouseCode).toLowerCase() !== manifest.clearinghouseCodeHash.toLowerCase()) throw new Error('The funding deployment code does not match the reviewed release.')
  const [clearinghouse, token, receiver] = await Promise.all([
    client.readContract({ address: manifest.receiverFactory, abi: FACTORY_ABI, functionName: 'clearinghouse', blockNumber }),
    client.readContract({ address: manifest.receiverFactory, abi: FACTORY_ABI, functionName: 'usdc', blockNumber }),
    client.readContract({ address: manifest.receiverFactory, abi: FACTORY_ABI, functionName: 'predictReceiver', args: [destination.beneficiary, quote.intentSalt], blockNumber }),
  ])
  if (!sameAddress(clearinghouse, manifest.clearinghouse) || !sameAddress(token, manifest.token) || !sameAddress(receiver, quote.receiver)) throw new Error('The receiver is not bound to your reviewed Trading Account and USDC clearinghouse.')
  const receiverCode = await client.getCode({ address: receiver, blockNumber })
  if (receiverCode && receiverCode !== '0x') {
    const [beneficiary, receiverClearinghouse, receiverToken] = await Promise.all([
      client.readContract({ address: receiver, abi: RECEIVER_ABI, functionName: 'beneficiary', blockNumber }),
      client.readContract({ address: receiver, abi: RECEIVER_ABI, functionName: 'clearinghouse', blockNumber }),
      client.readContract({ address: receiver, abi: RECEIVER_ABI, functionName: 'usdc', blockNumber }),
    ])
    if (!sameAddress(beneficiary, destination.beneficiary) || !sameAddress(receiverClearinghouse, manifest.clearinghouse) || !sameAddress(receiverToken, manifest.token)) throw new Error('The deployed receiver bindings changed. No source transaction was requested.')
  }
}
