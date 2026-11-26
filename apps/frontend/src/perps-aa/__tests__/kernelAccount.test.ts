import { describe, expect, it, vi } from 'vitest'
import { concatHex, createPublicClient, custom, encodeAbiParameters, getAddress, keccak256, padHex, parseAbiParameters, toFunctionSelector, toHex } from 'viem'
import { entryPoint09Address, getUserOperationHash, type UserOperation } from 'viem/account-abstraction'
import { privateKeyToAccount } from 'viem/accounts'
import { arbitrumSepolia } from 'viem/chains'
import { createKernelTradingAccount, encodeKernelCalls, normalizeKernelSignature, verifyKernelTradingAccountAtBlock, type KernelAccountProfile } from '../kernelAccount'

const factory = getAddress('0xa299a4efee7bbfb2ea5668b30218c45fff78356c')
const sender = getAddress('0xb24394d3704d52aa1d1acdedd4ae08ba426c998a')
const validator = getAddress('0x439cde78158d8d5249395e40467b44182d57e4ec')
const profile: KernelAccountProfile = {
  accountVersion: 'kernel-v4-uups',
  accountIndex: 0,
  chainId: arbitrumSepolia.id,
  entryPoint: entryPoint09Address,
  entryPointVersion: '0.9',
  factory,
  validator,
}

describe('Kernel account adapter', () => {
  it('normalizes mixed-case target and data in single and batch execution calldata', () => {
    const checksummed = { to: factory, data: '0xAbCd' as const }
    const lower = { to: factory.toLowerCase() as typeof factory, data: '0xabcd' as const }
    expect(encodeKernelCalls([checksummed])).toBe(encodeKernelCalls([lower]))
    expect(encodeKernelCalls([checksummed, checksummed])).toBe(encodeKernelCalls([lower, lower]))
    expect(encodeKernelCalls([checksummed])).toContain(lower.to.slice(2))
  })

  it('encodes raw wallet recovery bits as Solidity v and rejects high-s signatures', () => {
    const low = toHex(1n, { size: 32 })
    const high = toHex(0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140n, { size: 32 })
    expect(normalizeKernelSignature(concatHex([low, low, '0x00']))).toBe(concatHex([low, low, '0x1b']))
    expect(normalizeKernelSignature(concatHex([low, low, '0x01']))).toBe(concatHex([low, low, '0x1c']))
    expect(() => normalizeKernelSignature(concatHex([low, high, '0x1b']))).toThrow('noncanonical signature')
  })

  it('derives the same address and root nonce in both payer modes and signs the selected operation hash', async () => {
    const owner = privateKeyToAccount(toHex(1, { size: 32 }))
    const signMessage = vi.fn(owner.signMessage)
    const packageArgs = encodeAbiParameters(
      parseAbiParameters('(uint256 moduleType,address module,bytes moduleData,bytes internalData)[] initialPackages,uint256 nonce'),
      [[{ moduleType: 1n, module: validator, moduleData: owner.address, internalData: '0x' }], 0n],
    )
    const client = createPublicClient({
      chain: arbitrumSepolia,
      transport: custom({
        request: async ({ method, params }) => {
          if (method === 'eth_getCode') return '0x'
          if (method !== 'eth_call') throw new Error(`Unexpected RPC method ${method}`)
          const call = (params as [{ to: string; data: string }])[0]
          if (getAddress(call.to) === factory) {
            expect(call.data.toLowerCase()).toBe(concatHex([
              toFunctionSelector('getAddress((uint256,address,bytes,bytes)[],uint256)'), packageArgs,
            ]).toLowerCase())
            return encodeAbiParameters(parseAbiParameters('address'), [sender])
          }
          expect(getAddress(call.to)).toBe(entryPoint09Address)
          expect(call.data.endsWith('0'.repeat(64))).toBe(true)
          return encodeAbiParameters(parseAbiParameters('uint256'), [2n])
        },
      }),
    })
    const account = await createKernelTradingAccount(client, profile, { address: owner.address, signMessage })
    const factoryArgs = await account.getFactoryArgs()
    expect(factoryArgs.factory).toBe(factory)
    expect(factoryArgs.factoryData).toBe(concatHex(['0x0609747b', packageArgs]).toLowerCase())
    expect(account.address).toBe(sender)
    await expect(account.getNonce()).resolves.toBe(2n)
    await expect(account.getNonce({ key: 1n })).rejects.toThrow('root nonce')

    const eth: UserOperation<'0.9'> = {
      sender,
      nonce: 2n,
      callData: encodeKernelCalls([{ to: validator, data: '0x1234' }]),
      callGasLimit: 100_000n,
      verificationGasLimit: 150_000n,
      preVerificationGas: 50_000n,
      maxFeePerGas: 2n,
      maxPriorityFeePerGas: 1n,
      signature: '0x',
      ...factoryArgs,
    }
    const sponsored: UserOperation<'0.9'> = {
      ...eth,
      paymaster: validator,
      paymasterData: '0x1234',
      paymasterVerificationGasLimit: 50_000n,
      paymasterPostOpGasLimit: 0n,
    }
    for (const userOperation of [eth, sponsored]) {
      const hash = getUserOperationHash({
        userOperation,
        entryPointAddress: entryPoint09Address,
        entryPointVersion: '0.9',
        chainId: arbitrumSepolia.id,
      })
      await expect(account.signUserOperation(userOperation)).resolves.toBe(await owner.signMessage({ message: { raw: hash } }))
      expect(signMessage).toHaveBeenLastCalledWith({ message: { raw: hash } })
    }
    await expect(account.signUserOperation({ ...eth, sender: factory })).rejects.toThrow('Kernel account')
    await expect(account.signUserOperation({ ...eth, factoryData: '0x0609747b' })).rejects.toThrow('unsupported Kernel deployment')
    await expect(account.signUserOperation({ ...eth, factory: owner.address })).rejects.toThrow('unsupported Kernel deployment')
    expect(signMessage).toHaveBeenCalledTimes(2)
  })

  it('attests the same address before funding and checks the deployed implementation and owner at one block', async () => {
    const owner = privateKeyToAccount(toHex(1, { size: 32 })).address
    const implementation = getAddress('0x4444444444444444444444444444444444444444')
    const code = '0x6000'
    const codeHash = keccak256(code)
    const pins = { ...profile, implementation, entryPointCodeHash: codeHash, factoryCodeHash: codeHash,
      implementationCodeHash: codeHash, validatorCodeHash: codeHash, accountCodeHash: codeHash }
    let deployed = false
    let ownerHasCode = false
    let registeredOwner = owner
    let proxyImplementation = implementation
    let factoryImplementation = implementation
    let factoryCode = code
    const client = createPublicClient({ chain: arbitrumSepolia, transport: custom({
      request: async ({ method, params }) => {
        if (method === 'eth_chainId') return toHex(arbitrumSepolia.id)
        if (method === 'eth_getCode') {
          expect(params?.[1]).toBe('0x2a')
          if (params?.[0] === owner) return ownerHasCode ? code : '0x'
          if (params?.[0] === factory) return factoryCode
          return params?.[0] === sender && !deployed ? '0x' : code
        }
        if (method === 'eth_getStorageAt') {
          expect(params?.[2]).toBe('0x2a')
          return String(params?.[1]).startsWith('0x3608') ? padHex(proxyImplementation, { size: 32 }) : toHex(0n, { size: 32 })
        }
        if (method !== 'eth_call') throw new Error(`Unexpected RPC method ${method}`)
        const [call, block] = params as [{ data: string }, string]
        expect(block).toBe('0x2a')
        switch (call.data.slice(0, 10)) {
          case toFunctionSelector('UUPS()'):
            return encodeAbiParameters(parseAbiParameters('address'), [factoryImplementation])
          case toFunctionSelector('root()'):
            return encodeAbiParameters(parseAbiParameters('bytes21'), [concatHex(['0x01', validator])])
          case toFunctionSelector('ecdsaValidatorStorage(address)'):
            return encodeAbiParameters(parseAbiParameters('address'), [registeredOwner])
          case toFunctionSelector('getAddress((uint256,address,bytes,bytes)[],uint256)'):
            return encodeAbiParameters(parseAbiParameters('address'), [sender])
          default:
            throw new Error(`Unexpected contract call ${call.data}`)
        }
      },
    }) })

    await expect(verifyKernelTradingAccountAtBlock(client, pins, owner, 42n, sender)).resolves.toEqual({
      address: sender, ownerAddress: owner, blockNumber: 42n, deployed: false,
    })
    factoryImplementation = factory
    await expect(verifyKernelTradingAccountAtBlock(client, pins, owner, 42n, sender)).rejects.toThrow('could not be verified')
    factoryImplementation = implementation
    factoryCode = '0x6001'
    await expect(verifyKernelTradingAccountAtBlock(client, pins, owner, 42n, sender)).rejects.toThrow('could not be verified')
    factoryCode = code
    ownerHasCode = true
    await expect(verifyKernelTradingAccountAtBlock(client, pins, owner, 42n, sender)).rejects.toThrow('externally owned wallet')
    ownerHasCode = false
    await expect(verifyKernelTradingAccountAtBlock(client, pins, owner, 42n, factory)).rejects.toThrow('could not be verified')
    deployed = true
    await expect(verifyKernelTradingAccountAtBlock(client, pins, owner, 42n, sender)).resolves.toMatchObject({ deployed: true })
    registeredOwner = factory
    await expect(verifyKernelTradingAccountAtBlock(client, pins, owner, 42n, sender)).rejects.toThrow('root validator or owner')
    registeredOwner = owner
    proxyImplementation = factory
    await expect(verifyKernelTradingAccountAtBlock(client, pins, owner, 42n, sender)).rejects.toThrow('implementation')
  })
})
