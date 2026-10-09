import { describe, expect, it, vi } from 'vitest'
import { keccak256, pad, type Hex } from 'viem'
import { verifyFundingDeployment, EIP1967_IMPLEMENTATION_SLOT, type FundingReadClient } from './deployment'
import { parseFundingManifest } from './validation'
import { ACROSS_ARBITRUM_LOGGER } from './sources'
import runtime from './acrossRuntime.fixture.json'
import { CLEARINGHOUSE, MULTICALL_HANDLER, DESTINATION_SPOKE_POOL, DESTINATION_SPOKE_POOL_IMPLEMENTATION, OTHER_ADDRESS, DESTINATION_USDC, destinationFixture, quoteFixture, releaseFixture } from './testFixtures'

const code = '0x6001' as const
const manifest = parseFundingManifest({ ...releaseFixture(), multicallHandlerCodeHash: keccak256(code), destinationSpokePoolCodeHash: keccak256(code), destinationSpokePoolImplementationCodeHash: keccak256(code), clearinghouseCodeHash: keccak256(code) })
function clientFixture() {
  const getChainId = vi.fn(async () => 42161)
  const getBlockNumber = vi.fn(async () => 500000n)
  const getCode = vi.fn(async ({ address }: { address: string }) => address.toLowerCase() === ACROSS_ARBITRUM_LOGGER ? runtime.eventEmitterRuntime as Hex : code)
  const readContract = vi.fn(async () => DESTINATION_USDC)
  const getStorageAt = vi.fn(async () => pad(DESTINATION_SPOKE_POOL_IMPLEMENTATION))
  return { getChainId, getBlockNumber, getCode, readContract, getStorageAt }
}
const verify = (client: ReturnType<typeof clientFixture>) => verifyFundingDeployment(client as unknown as FundingReadClient, manifest, quoteFixture(), destinationFixture())

describe('shared destination deployment proof', () => {
  it('checks shared contract code and EIP-1967 implementation at one destination block', async () => {
    const client = clientFixture()
    await verify(client)
    expect(client.getCode).toHaveBeenCalledTimes(5)
    for (const [call] of client.getCode.mock.calls) expect(call).toMatchObject({ blockNumber: 500000n })
    expect(client.getStorageAt).toHaveBeenCalledWith({ address: DESTINATION_SPOKE_POOL, slot: EIP1967_IMPLEMENTATION_SLOT, blockNumber: 500000n })
    expect(client.readContract).toHaveBeenCalledWith(expect.objectContaining({ address: CLEARINGHOUSE, functionName: 'settlementAsset', blockNumber: 500000n }))
  })
  it('rejects unavailable or wrong-chain destination RPCs', async () => {
    await expect(verifyFundingDeployment(undefined, manifest, quoteFixture(), destinationFixture())).rejects.toThrow('unavailable')
    const client = clientFixture(); client.getChainId.mockResolvedValue(1)
    await expect(verify(client)).rejects.toThrow('wrong chain')
    expect(client.getCode).not.toHaveBeenCalled()
  })
  it.each([MULTICALL_HANDLER, DESTINATION_SPOKE_POOL, DESTINATION_SPOKE_POOL_IMPLEMENTATION, CLEARINGHOUSE, ACROSS_ARBITRUM_LOGGER])('rejects changed runtime at %s', async changed => {
    const client = clientFixture()
    client.getCode.mockImplementation(async ({ address }) => address.toLowerCase() === changed.toLowerCase() ? '0xdead' : address.toLowerCase() === ACROSS_ARBITRUM_LOGGER ? runtime.eventEmitterRuntime as Hex : code)
    await expect(verify(client)).rejects.toThrow('code does not match')
    expect(client.readContract).not.toHaveBeenCalled()
  })
  it('rejects a proxy upgrade despite unchanged proxy bytecode', async () => {
    const client = clientFixture(); client.getStorageAt.mockResolvedValue(pad(OTHER_ADDRESS))
    await expect(verify(client)).rejects.toThrow('implementation changed')
  })
  it('rejects a clearinghouse configured for another settlement asset', async () => {
    const client = clientFixture(); client.readContract.mockResolvedValue(OTHER_ADDRESS)
    await expect(verify(client)).rejects.toThrow('settlement token')
  })
  it('rejects a pinned destination from another release before RPC access', async () => {
    const client = clientFixture(), destination = { ...destinationFixture(), releaseId: 'changed' }
    await expect(verifyFundingDeployment(client as unknown as FundingReadClient, manifest, { ...quoteFixture(), releaseId: 'changed' }, destination)).rejects.toThrow('does not match')
    expect(client.getChainId).not.toHaveBeenCalled()
  })
})
