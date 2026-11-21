import {
  concatHex,
  encodeAbiParameters,
  encodeFunctionData,
  getAddress,
  isAddressEqual,
  keccak256,
  padHex,
  parseAbi,
  parseAbiParameters,
  parseSignature,
  recoverMessageAddress,
  serializeSignature,
  toHex,
  zeroAddress,
  type Address,
  type Hex,
  type PublicClient,
} from 'viem'
import {
  entryPoint09Abi,
  entryPoint09Address,
  getUserOperationHash,
  toSmartAccount,
  type UserOperation,
} from 'viem/account-abstraction'
import { arbitrumSepolia } from 'viem/chains'

const factoryAbi = parseAbi([
  'struct Install { uint256 moduleType; address module; bytes moduleData; bytes internalData; }',
  'function deploy(Install[] initialPackages, uint256 nonce) payable returns (address)',
  'function getAddress(Install[] initialPackages, uint256 nonce) view returns (address)',
  'function UUPS() view returns (address)',
])
const rootAbi = parseAbi(['function root() view returns (bytes21)'])
const validatorAbi = parseAbi(['function ecdsaValidatorStorage(address account) view returns (address)'])
const implementationSlot = '0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc'
const beaconSlot = '0xa3f0ad74e5423aebfd80d3ef4346578335a9a72aeaee59ff6cb3582b35133d50'
const emptySlot = toHex(0n, { size: 32 })
const executionAbi = parseAbi(['function execute(bytes32 mode, bytes executionData) payable'])
const executionBatchAbi = parseAbiParameters('(address target, uint256 value, bytes callData)[]')
const singleCallMode = toHex(0n, { size: 32 })
const batchCallMode = toHex(1n << 248n, { size: 32 })
const kernelStubSignature = '0xfffffffffffffffffffffffffffffff0000000000000000000000000000000007aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1c'
const secp256k1HalfOrder = 0x7fffffffffffffffffffffffffffffff5d576e7357a4501ddfe92f46681b20a0n

export interface KernelAccountProfile {
  chainId: number
  entryPoint: Address
  entryPointVersion: string
  factory: Address
  validator: Address
  accountVersion: string
  accountIndex: number
}

export interface KernelOwnerSigner {
  address: Address
  signMessage(parameters: { message: { raw: Hex } }): Promise<Hex>
}

export interface KernelAccountPins extends KernelAccountProfile {
  entryPointCodeHash: Hex
  factoryCodeHash: Hex
  implementation: Address
  implementationCodeHash: Hex
  validatorCodeHash: Hex
  accountCodeHash: Hex
}

export interface VerifiedKernelTradingAccount {
  address: Address
  ownerAddress: Address
  deployed: boolean
  blockNumber: bigint
}

export class UnsupportedKernelOwnerError extends Error {
  constructor() {
    super('Connect an externally owned wallet with no deployed or delegated code to use this Trading Account.')
    this.name = 'UnsupportedKernelOwnerError'
  }
}

/** Encodes only the ERC-7579 atomic execution modes admitted by the gateway. */
export function encodeKernelCalls(calls: readonly { to: Address; value?: bigint | undefined; data?: Hex | undefined }[]): Hex {
  if (calls.length === 0) throw new Error('A Kernel operation requires at least one call')
  const [first] = calls
  const executionData = calls.length === 1
    ? concatHex([first.to, toHex(first.value ?? 0n, { size: 32 }), first.data ?? '0x'])
    : encodeAbiParameters(executionBatchAbi, [calls.map(({ to, value, data }) => ({
        target: to,
        value: value ?? 0n,
        callData: data ?? '0x',
      }))])
  return encodeFunctionData({
    abi: executionAbi,
    functionName: 'execute',
    args: [calls.length === 1 ? singleCallMode : batchCallMode, executionData],
  }).toLowerCase() as Hex
}

/** Match the deployed Solidity ECDSA validator's accepted signature encoding. */
export function normalizeKernelSignature(signature: Hex): Hex {
  if (!/^0x[0-9a-fA-F]{130}$/.test(signature)) throw new Error('The Kernel wallet returned an invalid signature')
  const parsed = parseSignature(signature)
  if (BigInt(parsed.s) > secp256k1HalfOrder) throw new Error('The Kernel wallet returned a noncanonical signature')
  return serializeSignature(parsed).toLowerCase() as Hex
}

async function deriveKernelAccount(
  client: PublicClient,
  profile: KernelAccountProfile,
  owner: Address,
  blockNumber?: bigint,
) {
  if (profile.chainId !== arbitrumSepolia.id || client.chain?.id !== profile.chainId ||
      !isAddressEqual(profile.entryPoint, entryPoint09Address) || profile.entryPointVersion !== '0.9' ||
      profile.accountVersion !== 'kernel-v4-uups' || profile.accountIndex !== 0) {
    throw new Error('Unsupported Kernel trading account profile')
  }
  const ownerAddress = getAddress(owner)
  const factory = getAddress(profile.factory)
  const validator = getAddress(profile.validator)
  if ([ownerAddress, factory, validator].includes(zeroAddress)) {
    throw new Error('Kernel owner, factory and validator must be valid')
  }
  const install = [{ moduleType: 1n, module: validator, moduleData: ownerAddress, internalData: '0x' as const }]
  const args = [install, 0n] as const
  const factoryData = encodeFunctionData({ abi: factoryAbi, functionName: 'deploy', args }).toLowerCase() as Hex
  const factoryArgs = { factory, factoryData }
  const address = await client.readContract({ abi: factoryAbi, address: factory, functionName: 'getAddress', args, blockNumber })
  if (isAddressEqual(address, zeroAddress) || isAddressEqual(address, ownerAddress)) {
    throw new Error('Kernel factory returned an invalid Trading Account address')
  }
  return { address, ownerAddress, factoryArgs }
}

/** Verify the funding recipient and, if deployed, the exact current root and UUPS implementation. */
export async function verifyKernelTradingAccountAtBlock(
  client: PublicClient,
  profile: KernelAccountPins,
  owner: Address,
  blockNumber: bigint,
  expectedAddress?: Address,
): Promise<VerifiedKernelTradingAccount> {
  const contracts = [
    [profile.entryPoint, profile.entryPointCodeHash], [profile.factory, profile.factoryCodeHash],
    [profile.implementation, profile.implementationCodeHash], [profile.validator, profile.validatorCodeHash],
  ] as const
  const [{ address, ownerAddress }, chainId, factoryImplementation, codeMatches] = await Promise.all([
    deriveKernelAccount(client, profile, owner, blockNumber),
    client.getChainId(),
    client.readContract({ abi: factoryAbi, address: profile.factory, functionName: 'UUPS', blockNumber }),
    Promise.all(contracts.map(async ([address, hash]) => {
      const code = await client.getBytecode({ address, blockNumber })
      return code !== undefined && code !== '0x' && keccak256(code).toLowerCase() === hash.toLowerCase()
    })),
  ])
  if (chainId !== profile.chainId || !isAddressEqual(factoryImplementation, profile.implementation) ||
      codeMatches.includes(false) || (expectedAddress !== undefined && !isAddressEqual(address, expectedAddress))) {
    throw new Error('The Kernel account profile could not be verified onchain')
  }
  const [code, ownerCode] = await Promise.all([
    client.getBytecode({ address, blockNumber }),
    client.getBytecode({ address: ownerAddress, blockNumber }),
  ])
  if (ownerCode && ownerCode !== '0x') throw new UnsupportedKernelOwnerError()
  if (!code || code === '0x') return { address, ownerAddress, deployed: false, blockNumber }
  if (keccak256(code).toLowerCase() !== profile.accountCodeHash.toLowerCase()) {
    throw new Error('The Trading Account runtime does not match the reviewed Kernel profile')
  }

  const [implementation, beacon, root, liveOwner] = await Promise.all([
    client.getStorageAt({ address, slot: implementationSlot, blockNumber }),
    client.getStorageAt({ address, slot: beaconSlot, blockNumber }),
    client.readContract({ address, abi: rootAbi, functionName: 'root', blockNumber }),
    client.readContract({ address: profile.validator, abi: validatorAbi, functionName: 'ecdsaValidatorStorage', args: [address], blockNumber }),
  ])
  if (implementation?.toLowerCase() !== padHex(profile.implementation, { size: 32 }).toLowerCase() || beacon !== emptySlot ||
      root.toLowerCase() !== concatHex(['0x01', profile.validator]).toLowerCase() || !isAddressEqual(liveOwner, ownerAddress)) {
    throw new Error('The Trading Account implementation, root validator or owner does not match the reviewed Kernel profile')
  }
  return { address, ownerAddress, deployed: true, blockNumber }
}

/** The adapter derives the index-zero account from the same owner package used for verification. */
export async function createKernelTradingAccount(
  client: PublicClient,
  profile: KernelAccountProfile,
  owner: KernelOwnerSigner,
  blockNumber?: bigint,
) {
  const { address, ownerAddress, factoryArgs } = await deriveKernelAccount(client, profile, owner.address, blockNumber)
  const { factory, factoryData } = factoryArgs
  const entryPoint = { address: entryPoint09Address, abi: entryPoint09Abi, version: '0.9' as const }

  const readRootNonce = async ({ key = 0n }: { key?: bigint } = {}) => {
    if (key !== 0n) throw new Error('Only the Kernel root nonce is supported')
    return client.readContract({ ...entryPoint, functionName: 'getNonce', args: [address, 0n] })
  }
  const unsupportedSignature = async (): Promise<never> => {
    throw new Error('This Kernel adapter signs UserOperations only')
  }
  const account = await toSmartAccount({
    client,
    entryPoint,
    getAddress: async () => address,
    getFactoryArgs: async () => factoryArgs,
    encodeCalls: async calls => encodeKernelCalls(calls),
    getStubSignature: async () => kernelStubSignature,
    signMessage: unsupportedSignature,
    signTypedData: unsupportedSignature,
    signUserOperation: async ({ chainId, sender = address, ...parameters }) => {
      if ((chainId !== undefined && chainId !== profile.chainId) || !isAddressEqual(sender, address) || (parameters.nonce >> 64n) !== 0n) {
        throw new Error('UserOperation does not match the Kernel account, chain or root nonce')
      }
      if (('eip7702Auth' in parameters && parameters.eip7702Auth !== undefined) ||
          ('paymasterAndData' in parameters && parameters.paymasterAndData !== undefined) ||
          ('paymasterSignature' in parameters && parameters.paymasterSignature !== undefined) ||
          (parameters.factory !== undefined && (!isAddressEqual(parameters.factory, factory) || parameters.factoryData?.toLowerCase() !== factoryData)) ||
          (parameters.factory === undefined && parameters.factoryData !== undefined)) {
        throw new Error('UserOperation has unsupported Kernel deployment or signature fields')
      }
      const userOperation: UserOperation<'0.9'> = { ...parameters, sender }
      const hash = getUserOperationHash({ userOperation, chainId: profile.chainId, entryPointAddress: entryPoint.address, entryPointVersion: entryPoint.version })
      const signature = normalizeKernelSignature(await owner.signMessage({ message: { raw: hash } }))
      const signer = await recoverMessageAddress({ message: { raw: hash }, signature })
      if (!isAddressEqual(signer, ownerAddress)) throw new Error('UserOperation was signed by a different Kernel owner')
      return signature
    },
  })

  // Viem's default nonce manager generates a new key when one is omitted.
  // Plether serializes both payment modes on the same root nonce key. Keep the
  // fixed factory args so callers can decide deployment against the safe block.
  return { ...account, factoryArgs, getNonce: readRootNonce }
}
