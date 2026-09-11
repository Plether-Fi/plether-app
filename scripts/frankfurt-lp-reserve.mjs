// One-time reserve adjustment after a measured, unaffordable LP settlement.
import { createPublicClient, createWalletClient, http, parseEther } from '../apps/frontend/node_modules/viem/_esm/index.js'
import { privateKeyToAccount } from '../apps/frontend/node_modules/viem/_esm/accounts/index.js'
import { arbitrumSepolia } from '../apps/frontend/node_modules/viem/_esm/chains/index.js'
import { readSecret, verifyAccount } from './frankfurt-aa-secrets.mjs'

try {
  if (process.argv[2] !== '--fund') throw new Error('Explicit mode required')
  verifyAccount()
  const account = privateKeyToAccount(readSecret('/plether/bootstrap/sepolia-aa-temp/deployer-private-key'))
  if (account.address.toLowerCase() !== '0x4db6e8d6f9cd43d3e3e1cdc2eb9c9a4c41d1795c') throw new Error('Wrong source')
  const transport = http(readSecret('/plether/sepolia-aa-temp/perps-rpc-url'))
  const client = createPublicClient({ chain: arbitrumSepolia, transport })
  const wallet = createWalletClient({ account, chain: arbitrumSepolia, transport })
  if (await client.getChainId() !== 421614) throw new Error('Wrong chain')
  const to = '0x37492D21a64Ed2Fc1E841EA2FC294cC0aaff6D4b'
  if (await client.getBalance({ address: to }) !== parseEther('.001')) throw new Error('Balance changed; inspect before funding')
  const value = parseEther('.0012')
  const fees = await client.estimateFeesPerGas()
  const gas = (await client.estimateGas({ account, to, value })) * 120n / 100n
  if (gas * fees.maxFeePerGas > parseEther('.0001')) throw new Error('Fee cap exceeded')
  if (await client.getBalance({ address: account.address }) < value + gas * fees.maxFeePerGas) throw new Error('Insufficient existing reserve')
  const hash = await wallet.sendTransaction({ to, value, gas, ...fees })
  const receipt = await client.waitForTransactionReceipt({ hash })
  if (receipt.status !== 'success') throw new Error('Funding failed')
  console.log(JSON.stringify({ hash, to, valueWei: String(value), feeWei: String(receipt.gasUsed * receipt.effectiveGasPrice), balanceWei: String(await client.getBalance({ address: to })) }))
} catch { console.error('LP reserve funding stopped; details withheld'); process.exitCode = 1 }
