import { describe, expect, it, vi } from 'vitest'
import { decodeEventLog, encodeAbiParameters, encodeEventTopics, encodeFunctionData, keccak256, stringToHex, type PublicClient } from 'viem'
import { PERPS_POSITION_PROTECTION_BOOK_ABI, PERPS_ORDER_LIFECYCLE_BOOK_ABI } from '../abis'
import { BOUNTY_DISPOSITION, POSITION_PROTECTION_STATUS, PERPS_CONFIG_SCHEMA_HASH, PERPS_RECEIPT_TYPEHASH, positionProtectionMessage } from '../perpsProtection'
import { verifyPerpsReceiptSchema } from '../verifyPerpsV2Bindings'

const account = '0x0000000000000000000000000000000000000001'

describe('latched position protection', () => {
  it('keeps canonical enum ordinals and explains the irreversible trade lock', () => {
    expect(POSITION_PROTECTION_STATUS.Latched).toBe(8)
    expect(BOUNTY_DISPOSITION.RetainedForProtectionRetry).toBe(4)
    expect(BOUNTY_DISPOSITION.Forfeited).toBe(2)
    expect(positionProtectionMessage(7n, 8)).toContain('cannot be cancelled')
    expect(positionProtectionMessage(7n, 8)).toContain('reward is retained')
    expect(positionProtectionMessage(7n, 3)).toContain('market close is queued')
    expect(positionProtectionMessage(7n, 2)).toContain('Cancel it')
  })

  it('encodes the nonpayable retry against the protection Book', () => {
    expect(encodeFunctionData({ abi: PERPS_POSITION_PROTECTION_BOOK_ABI,
      functionName: 'retryPositionProtectionClose', args: [7n] })).toBe(
      keccak256(stringToHex('retryPositionProtectionClose(uint64)')).slice(0, 10) + '7'.padStart(64, '0')
    )
    expect(PERPS_POSITION_PROTECTION_BOOK_ABI.find(x => x.name === 'retryPositionProtectionClose')).toMatchObject({ stateMutability: 'nonpayable' })
  })

  it('retains previous-attempt linkage in queued events', () => {
    const topics = encodeEventTopics({ abi: PERPS_POSITION_PROTECTION_BOOK_ABI,
      eventName: 'PositionProtectionCloseAttemptQueued', args: { protectionId: 7n, account, linkedOrderId: 19n } })
    const decoded = decodeEventLog({ abi: PERPS_POSITION_PROTECTION_BOOK_ABI, topics,
      data: encodeAbiParameters([{ type: 'uint64' }], [11n]) })
    expect(decoded.args).toEqual({ protectionId: 7n, account, linkedOrderId: 19n, previousLinkedOrderId: 11n })
    expect(PERPS_ORDER_LIFECYCLE_BOOK_ABI.find(x => x.name === 'ProtectionAttemptRegistered')).toBeDefined()
  })

  it('accepts V3 schemas at a coherent block and rejects V2 or a mixed generation', async () => {
    const readContract = vi.fn(({ functionName }: { functionName: string }) => Promise.resolve(
      functionName === 'CONFIG_SCHEMA_HASH' ? PERPS_CONFIG_SCHEMA_HASH : PERPS_RECEIPT_TYPEHASH
    ))
    const client = { readContract } as unknown as PublicClient
    await expect(verifyPerpsReceiptSchema(client, account, 12n)).resolves.toBeUndefined()
    expect(readContract).toHaveBeenCalledWith(expect.objectContaining({ blockNumber: 12n }))
    readContract.mockResolvedValueOnce(keccak256(stringToHex('PletherExecutionConfigV2')))
    await expect(verifyPerpsReceiptSchema(client, account, 12n)).rejects.toThrow('complete perps stack')
    readContract.mockResolvedValueOnce(PERPS_CONFIG_SCHEMA_HASH).mockResolvedValueOnce(keccak256(stringToHex('old receipt')))
    await expect(verifyPerpsReceiptSchema(client, account, 12n)).rejects.toThrow('complete perps stack')
  })
})
