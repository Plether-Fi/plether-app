// Explicit user approval raised the total owner ceiling to 0.15 test ETH.
// This allocates 0.023 from the separately approved 0.025 setup-wallet transfer.
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
  const targets = [
    { role: 'oracle', to: '0x12Fb09e86d263Cd43a118fC25bAde50931d1484B', value: parseEther('.02') },
    { role: 'keeper', to: '0x1092D102f615d451d3f82eF8CD283eB1a3e2c051', value: parseEther('.003') },
  ]
  // Retry requires inspection, never an automatic second allocation.
  if (await client.getBalance({ address: targets[0].to }) >= parseEther('.0005')) throw new Error('Oracle balance changed')
  if (await client.getBalance({ address: targets[1].to }) !== parseEther('.002')) throw new Error('Keeper balance changed')
  if (await client.getBalance({ address: account.address }) < parseEther('.0232')) throw new Error('Approved funding not available')
  for (const target of targets) {
    const fees = await client.estimateFeesPerGas()
    const gas = (await client.estimateGas({ account, to: target.to, value: target.value })) * 120n / 100n
    if (gas * fees.maxFeePerGas > parseEther('.0001')) throw new Error('Fee cap exceeded')
    const hash = await wallet.sendTransaction({ to: target.to, value: target.value, gas, ...fees })
    const receipt = await client.waitForTransactionReceipt({ hash })
    if (receipt.status !== 'success') throw new Error('Funding failed')
    console.log(JSON.stringify({ role: target.role, hash, valueWei: String(target.value), feeWei: String(receipt.gasUsed * receipt.effectiveGasPrice) }))
  }
} catch { console.error('Live runtime funding stopped; details withheld'); process.exitCode = 1 }
