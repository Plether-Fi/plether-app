import { describe, expect, it, vi } from 'vitest'
import { keccak256 } from 'viem'
import { verifyFundingReceiver, type FundingReadClient } from './receiver'
import { parseFundingManifest } from './validation'
import { BENEFICIARY, CLEARINGHOUSE, FACTORY, OTHER_ADDRESS, RECEIVER, DESTINATION_USDC, destinationFixture, quoteFixture, releaseFixture } from './testFixtures'

const factoryCode = '0x6001'
const clearinghouseCode = '0x6002'
const manifest = parseFundingManifest({ ...releaseFixture(), factoryCodeHash: keccak256(factoryCode), clearinghouseCodeHash: keccak256(clearinghouseCode) })
function clientFixture(receiverDeployed = false) {
  const getChainId = vi.fn(async () => 42161)
  const getBlockNumber = vi.fn(async () => 500000n)
  const getCode = vi.fn(async ({ address }: { address: string }) => address.toLowerCase() === FACTORY ? factoryCode : address.toLowerCase() === CLEARINGHOUSE ? clearinghouseCode : receiverDeployed ? '0x6003' : '0x')
  const readContract = vi.fn(async ({ functionName }: { functionName: string }) => ({ clearinghouse: CLEARINGHOUSE, usdc: DESTINATION_USDC, predictReceiver: RECEIVER, beneficiary: BENEFICIARY })[functionName])
  return { getChainId, getBlockNumber, getCode, readContract }
}
function verify(client: ReturnType<typeof clientFixture>) {
  return verifyFundingReceiver(client as unknown as FundingReadClient, manifest, quoteFixture(), destinationFixture())
}

describe('onchain destination receiver proof', () => {
  it('proves a counterfactual receiver from the reviewed factory and beneficiary at one block', async () => {
    const client = clientFixture()
    await verify(client)
    expect(client.readContract).toHaveBeenCalledWith(expect.objectContaining({ address: FACTORY, functionName: 'predictReceiver', args: [BENEFICIARY, quoteFixture().intentSalt], blockNumber: 500000n }))
    for (const [call] of client.getCode.mock.calls) expect(call).toMatchObject({ blockNumber: 500000n })
    for (const [call] of client.readContract.mock.calls) expect(call).toMatchObject({ blockNumber: 500000n })
  })
  it('checks the deployed receiver beneficiary and clearinghouse bindings too', async () => {
    const client = clientFixture(true)
    await verify(client)
    expect(client.readContract).toHaveBeenCalledWith(expect.objectContaining({ address: RECEIVER, functionName: 'beneficiary' }))
    client.readContract.mockImplementation(async ({ functionName }) => functionName === 'beneficiary' ? OTHER_ADDRESS : ({ clearinghouse: CLEARINGHOUSE, usdc: DESTINATION_USDC, predictReceiver: RECEIVER })[functionName])
    await expect(verify(client)).rejects.toThrow('deployed receiver bindings')
  })
  it('rejects unavailable and wrong-chain destination RPCs before reading code', async () => {
    await expect(verifyFundingReceiver(undefined, manifest, quoteFixture(), destinationFixture())).rejects.toThrow('unavailable')
    const client = clientFixture()
    client.getChainId.mockResolvedValue(1)
    await expect(verify(client)).rejects.toThrow('wrong chain')
    expect(client.getCode).not.toHaveBeenCalled()
  })
  it.each([FACTORY, CLEARINGHOUSE])('rejects a runtime code mismatch at %s', async badAddress => {
    const client = clientFixture()
    client.getCode.mockImplementation(async ({ address }) => address.toLowerCase() === badAddress ? '0xdead' : address.toLowerCase() === FACTORY ? factoryCode : clearinghouseCode)
    await expect(verify(client)).rejects.toThrow('code does not match')
    expect(client.readContract).not.toHaveBeenCalled()
  })
  it.each(['predictReceiver', 'clearinghouse', 'usdc'])('rejects a changed factory %s binding even when the provider quote is internally consistent', async badFunction => {
    const client = clientFixture()
    client.readContract.mockImplementation(async ({ functionName }) => functionName === badFunction ? OTHER_ADDRESS : ({ clearinghouse: CLEARINGHOUSE, usdc: DESTINATION_USDC, predictReceiver: RECEIVER, beneficiary: BENEFICIARY })[functionName])
    await expect(verify(client)).rejects.toThrow('not bound')
  })
  it('rejects a pinned destination from another release before RPC access', async () => {
    const client = clientFixture()
    const destination = { ...destinationFixture(), releaseId: 'changed-release' }
    await expect(verifyFundingReceiver(client as unknown as FundingReadClient, manifest, { ...quoteFixture(), releaseId: destination.releaseId }, destination)).rejects.toThrow('does not match')
    expect(client.getChainId).not.toHaveBeenCalled()
  })
  it('fails closed if the RPC cannot read the reviewed factory', async () => {
    const client = clientFixture()
    client.readContract.mockRejectedValue(new Error('RPC unavailable'))
    await expect(verify(client)).rejects.toThrow('RPC unavailable')
  })
})
