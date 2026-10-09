import { keccak256, pad, parseAbi, type Hex, type PublicClient } from 'viem'
import type { FundingDestination, FundingManifest, FundingQuote } from './types'
import { assertDestination, sameAddress } from './validation'
import { ACROSS_ARBITRUM_LOGGER, ACROSS_ARBITRUM_LOGGER_CODE_HASH } from './sources'

const CLEARINGHOUSE_ABI = parseAbi(['function settlementAsset() view returns (address)'])
export const EIP1967_IMPLEMENTATION_SLOT: Hex = '0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc'
export type FundingReadClient = Pick<PublicClient, 'getChainId' | 'getBlockNumber' | 'getCode' | 'getStorageAt' | 'readContract'>

/** Verify the existing handler, proxy implementation and clearinghouse at one destination block. */
export async function verifyFundingDeployment(client: FundingReadClient | undefined, manifest: FundingManifest, quote: FundingQuote, destination: FundingDestination): Promise<void> {
  assertDestination(quote, destination)
  if (manifest.releaseId !== destination.releaseId || manifest.destinationChainId !== destination.destinationChainId || !sameAddress(manifest.multicallHandler, destination.multicallHandler) || !sameAddress(manifest.destinationSpokePool, destination.destinationSpokePool) || !sameAddress(manifest.clearinghouse, destination.clearinghouse) || !sameAddress(manifest.token, destination.token)) throw new Error('The pinned funding destination does not match the reviewed release.')
  if (!client) throw new Error('The destination RPC is unavailable. No source transaction was requested.')
  if (await client.getChainId() !== manifest.destinationChainId) throw new Error('The destination RPC is connected to the wrong chain.')
  const blockNumber = await client.getBlockNumber()
  const pins = [
    [manifest.multicallHandler, manifest.multicallHandlerCodeHash],
    [manifest.destinationSpokePool, manifest.destinationSpokePoolCodeHash],
    [manifest.destinationSpokePoolImplementation, manifest.destinationSpokePoolImplementationCodeHash],
    [manifest.clearinghouse, manifest.clearinghouseCodeHash],
    [ACROSS_ARBITRUM_LOGGER, ACROSS_ARBITRUM_LOGGER_CODE_HASH],
  ] as const
  await Promise.all(pins.map(async ([address, expectedHash]) => {
    const code = await client.getCode({ address, blockNumber })
    if (!code || code === '0x' || keccak256(code).toLowerCase() !== expectedHash.toLowerCase()) throw new Error('The funding deployment code does not match the reviewed release.')
  }))
  const [implementation, token] = await Promise.all([
    client.getStorageAt({ address: manifest.destinationSpokePool, slot: EIP1967_IMPLEMENTATION_SLOT, blockNumber }),
    client.readContract({ address: manifest.clearinghouse, abi: CLEARINGHOUSE_ABI, functionName: 'settlementAsset', blockNumber }),
  ])
  if (implementation?.toLowerCase() !== pad(manifest.destinationSpokePoolImplementation).toLowerCase()) throw new Error('The destination SpokePool implementation changed.')
  if (!sameAddress(token, manifest.token)) throw new Error('The clearinghouse settlement token does not match this funding release.')
}
