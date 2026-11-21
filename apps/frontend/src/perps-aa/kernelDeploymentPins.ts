import type { Address, Hex } from 'viem'
import type { KernelDeploymentProfile } from './kernelRpc'

// Shared contracts retain the Arbitrum Sepolia runtime hashes attested at safe
// block 331404971. This profile authorizes direct factory deployment only.
export const kernelSepoliaCanaryProfile = Object.freeze({
  deployment: 'kernel-sepolia-direct-v1',
  chainId: 421614,
  entryPoint: '0x433709009b8330fda32311df1c2afa402ed8d009',
  entryPointVersion: '0.9',
  entryPointCodeHash: '0x66104e61203e3b9cf29303ececd9367b6329f6caa2bb063361ea75b7b92a7678',
  factory: '0xa299a4efee7bbfb2ea5668b30218c45fff78356c',
  factoryCodeHash: '0x0813fbfc2432a3ea3f7c06a1ac6a7a118faba748ab602aa711f567f212958426',
  implementation: '0xc842fe2ac44046ae3cef033a16c67a9bc287cbd2',
  implementationCodeHash: '0x0cf809e8e95a40ca07da3762911609871facd76a6d717e5a95b813ba03e6e5c0',
  validator: '0x439cde78158d8d5249395e40467b44182d57e4ec',
  validatorCodeHash: '0x9ceb47b953e8d105f632c46394c66b9a4498cbea889fd45feab855feed4608fe',
  accountCodeHash: '0xaaa52c8cc8a0e3fd27ce756cc6b4e70c51423e9b597b11f32d3e49f8b1fc890d',
  accountVersion: 'kernel-v4-uups',
  accountIndex: 0,
} as const satisfies KernelDeploymentProfile)

// Direct-factory paymaster confirmed by both RPCs at safe block 331475162.
export const kernelSepoliaCanaryPaymaster: Readonly<{ address: Address; codeHash: Hex }> | undefined = Object.freeze({
  address: '0x9ff288e30489c4e29b13d6ba960612b5d6db8d50',
  codeHash: '0x0ff0bb843a663a4a22dbcc64b2efda8654eca777298d867a812db9073cd8c115',
})
