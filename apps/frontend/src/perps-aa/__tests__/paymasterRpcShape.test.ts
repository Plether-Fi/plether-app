import { custom, numberToHex, type Address, type Hex } from 'viem'
import { createPaymasterClient } from 'viem/account-abstraction'
import { describe, expect, it, vi } from 'vitest'
import { createUnsignedPaymasterActions } from '../managedPimlicoRuntime'

const ENTRY_POINT =
  '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108' as Address
const ACCOUNT = '0x1111111111111111111111111111111111111111' as Address
const FACTORY = '0x13E9ed32155810FDbd067D4522C492D6f68E5944' as Address
const PAYMASTER = '0x1234567890123456789012345678901234567890' as Address
const CHAIN_ID = 421_614
const PAYMASTER_DATA = `0x${'11'.repeat(157)}` as Hex

function paymasterParameters(): Record<string, unknown> {
  return {
    chainId: CHAIN_ID,
    entryPointAddress: ENTRY_POINT,
    context: {},
    sender: ACCOUNT,
    nonce: 0n,
    factory: FACTORY,
    factoryData: '0x1234',
    callData: '0x5678',
    callGasLimit: 1n,
    verificationGasLimit: 2n,
    preVerificationGas: 3n,
    maxFeePerGas: 4n,
    maxPriorityFeePerGas: 5n,
    paymaster: PAYMASTER,
    paymasterData: `0x${'22'.repeat(157)}`,
    paymasterVerificationGasLimit: 6n,
    paymasterPostOpGasLimit: 7n,
    signature: '0xowner-account-stub',
  }
}

describe('native paymaster JSON-RPC request shape', () => {
  it.each([
    'pm_getPaymasterStubData',
    'pm_getPaymasterData',
  ] as const)('sends an unsigned operation to %s', async (expectedMethod) => {
    const request = vi.fn(async ({ method }: {
      method: string
      params?: unknown
    }) => {
      expect(method).toBe(expectedMethod)
      return {
        paymaster: PAYMASTER,
        paymasterData: PAYMASTER_DATA,
        paymasterVerificationGasLimit: '0x186a0',
        paymasterPostOpGasLimit: '0x0',
      }
    })
    const paymasterClient = createPaymasterClient({
      transport: custom({ request }),
    })
    const actions = createUnsignedPaymasterActions(paymasterClient)
    const call = expectedMethod === 'pm_getPaymasterStubData'
      ? actions.getPaymasterStubData
      : actions.getPaymasterData

    await (call as unknown as (
      parameters: Record<string, unknown>
    ) => Promise<unknown>)(paymasterParameters())

    const rpcRequest = request.mock.calls[0]?.[0]
    expect(rpcRequest).toEqual({
      method: expectedMethod,
      params: [
        {
          sender: ACCOUNT,
          nonce: '0x0',
          factory: FACTORY,
          factoryData: '0x1234',
          callData: '0x5678',
          callGasLimit: '0x1',
          verificationGasLimit: '0x2',
          preVerificationGas: '0x3',
          maxFeePerGas: '0x4',
          maxPriorityFeePerGas: '0x5',
          paymasterVerificationGasLimit: '0x6',
          paymasterPostOpGasLimit: '0x7',
        },
        ENTRY_POINT,
        numberToHex(CHAIN_ID),
        {},
      ],
    })
  })
})
