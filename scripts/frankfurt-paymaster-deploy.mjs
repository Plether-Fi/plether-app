// One-time, bounded Arbitrum Sepolia deployment from the tested upstream artifact.
// Default is simulation only. Secrets are retained in memory, never argv/log/files.
import { readFileSync } from 'node:fs'
import { createHash } from 'node:crypto'
import { createPublicClient, createWalletClient, encodeDeployData, getContractAddress,
  http, keccak256, parseEther, formatEther } from '../apps/frontend/node_modules/viem/_esm/index.js'
import { privateKeyToAccount } from '../apps/frontend/node_modules/viem/_esm/accounts/index.js'
import { arbitrumSepolia } from '../apps/frontend/node_modules/viem/_esm/chains/index.js'
import { verifyAccount, readSecret } from './frankfurt-aa-secrets.mjs'

const artifactPath = '/var/folders/xz/scmmjjld2pq8k763z54mzjtr0000gn/T/plether-frankfurt-paymaster-RQZVyA/packages/perps-aa/out/PletherVerifyingPaymaster.sol/PletherVerifyingPaymaster.json'
const owner = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
const signer = '0x015736E1F47E37938236e481F7a3B7c57F922b80'
const entryPoint = '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108'
const factory = '0x13E9ed32155810FDbd067D4522C492D6f68E5944'
const implementation = '0x28426d752372D68d34340bd94390950DcE3C9ec3'
const policy = '0x8dd77324b94da492342191f762a32cdf99e828a7f24d77c8ed5ace90cf4f5ae3'
const proxyHash = '0x41ee894da413cc99e8dec0a1784470eceb736845ad1591e06ff0ecdf0aca26c9'
const profiles = [
  [entryPoint, '0xe3f30f78ae55058acdefea00952c8e44f2263215cf720fe1b27b6f148add0278'],
  [factory, '0xa2e635152a61e180383c7afc045620b7461ef6f43ba27d592262513106b991b7'],
  [implementation, '0x689a90eff03926a12aedad2fc6d4fdbcbdd9ffac86e7d0d70ce6355961305c74'],
]
function requireThat(ok, label) { if (!ok) throw new Error(label) }
async function main() {
  const broadcast = process.argv[2] === '--broadcast'
  requireThat(process.argv.length === 2 || (broadcast && process.argv.length === 3), 'Invalid arguments')
  verifyAccount()
  const raw = readFileSync(artifactPath)
  requireThat(createHash('sha256').update(raw).digest('hex') === '653bb0a8dee34613949cf5c727f86598158e6099b4bf11354cb703cd22718870', 'Artifact mismatch')
  const artifact = JSON.parse(raw)
  const account = privateKeyToAccount(readSecret('/plether/bootstrap/sepolia-aa-temp/deployer-private-key'))
  requireThat(account.address === '0x4dB6E8d6f9cd43D3e3e1cdC2eB9C9A4C41D1795c', 'Deployer mismatch')
  const transport = http(readSecret('/plether/sepolia-aa-temp/perps-rpc-url'))
  const client = createPublicClient({ chain: arbitrumSepolia, transport })
  requireThat(await client.getChainId() === 421614, 'Chain mismatch')
  const funding = await client.getTransactionReceipt({ hash: '0x54462f31490b538b623ecc854aba510f6716e383fec1dd52c939ebac8dad34c2' })
  requireThat(funding.status === 'success', 'Funding receipt failed')
  for (const [address, hash] of profiles) requireThat(keccak256(await client.getCode({ address }) ?? '0x') === hash, 'Profile hash mismatch')
  const predicted = getContractAddress({ from: account.address, nonce: 0n })
  requireThat(await client.getTransactionCount({ address: account.address, blockTag: 'pending' }) === 0, 'Deployer already used; inspect receipts, do not redeploy')
  requireThat(!await client.getCode({ address: predicted }), 'Predicted address already deployed')
  const data = encodeDeployData({ abi: artifact.abi, bytecode: artifact.bytecode.object,
    args: [entryPoint, owner, signer, parseEther('0.01'), policy, proxyHash, factory, implementation] })
  const gas = (await client.estimateGas({ account, data, value: 0n })) * 12n / 10n
  const fees = await client.estimateFeesPerGas()
  requireThat(fees.maxFeePerGas && gas * fees.maxFeePerGas < parseEther('0.003'), 'Deployment fee exceeds 0.003 test ETH ceiling')
  requireThat(await client.getBalance({ address: account.address }) >= parseEther('0.075') + gas * fees.maxFeePerGas, 'Funding budget insufficient')
  console.log(JSON.stringify({ mode: broadcast ? 'broadcast' : 'simulation-only', predicted, owner, signer, gas: gas.toString(), maximumFeeETH: formatEther(gas * fees.maxFeePerGas), deposit: '0', stake: '0', startsPaused: true }))
  if (!broadcast) return
  const wallet = createWalletClient({ account, chain: arbitrumSepolia, transport })
  const hash = await wallet.sendTransaction({ data, value: 0n, nonce: 0, gas, ...fees })
  console.log(JSON.stringify({ submitted: hash })) // Safe recovery checkpoint; never retry blindly.
  const receipt = await client.waitForTransactionReceipt({ hash, timeout: 45000 })
  requireThat(receipt.status === 'success' && receipt.contractAddress?.toLowerCase() === predicted.toLowerCase(), 'Deployment receipt mismatch')
  const read = functionName => client.readContract({ address: predicted, abi: artifact.abi, functionName })
  for (const [name, value] of [['owner', owner], ['sponsorSigner', signer], ['entryPoint', entryPoint], ['policyId', policy], ['approvedAccountCodeHash', proxyHash], ['accountFactory', factory], ['accountImplementation', implementation]]) {
    requireThat((await read(name)).toLowerCase() === value.toLowerCase(), `Readback mismatch: ${name}`)
  }
  requireThat(await read('paused') === true && await read('getDeposit') === 0n && await read('maxSponsoredCost') === parseEther('0.01'), 'Initial safety state mismatch')
  console.log(JSON.stringify({ verified: true, hash, address: predicted, block: receipt.blockNumber.toString(), blockHash: receipt.blockHash,
    runtimeCodeHash: keccak256(await client.getCode({ address: predicted })), gasCostETH: formatEther(receipt.gasUsed * receipt.effectiveGasPrice) }))
}
main().catch(() => { console.error('Deployment stopped; inspect the last safe checkpoint. Error details withheld to protect secrets.'); process.exitCode = 1 })
