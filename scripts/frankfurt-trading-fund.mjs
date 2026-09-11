import { createPublicClient, createWalletClient, http, parseEther } from '../apps/frontend/node_modules/viem/_esm/index.js'
import { privateKeyToAccount } from '../apps/frontend/node_modules/viem/_esm/accounts/index.js'
import { arbitrumSepolia } from '../apps/frontend/node_modules/viem/_esm/chains/index.js'
import { loadPreparedInputs, readSecret, verifyAccount } from './frankfurt-aa-secrets.mjs'
import { loadTradingInputs } from './frankfurt-trading-inputs.mjs'

try {
  if (!['--check', '--fund'].includes(process.argv[2])) throw new Error('Explicit mode required')
  verifyAccount()
  const input = { ...loadPreparedInputs(), ...loadTradingInputs() }
  const source = privateKeyToAccount(readSecret('/plether/bootstrap/sepolia-aa-temp/deployer-private-key'))
  if (source.address.toLowerCase() !== '0x4db6e8d6f9cd43d3e3e1cdc2eb9c9a4c41d1795c') throw new Error('Unexpected source')
  const transport = http(input.perps_rpc_url)
  const client = createPublicClient({ chain: arbitrumSepolia, transport })
  const wallet = createWalletClient({ chain: arbitrumSepolia, transport, account: source })
  if (await client.getChainId() !== 421614) throw new Error('Unexpected chain')
  const allocations = [['oracle_updater', '.003'], ['keeper', '.002'], ['liquidation_keeper', '.001'], ['faucet', '.0005'], ['lp_settlement', '.001'], ['protection_worker', '.0005']]
  const targets = []
  for (const [role, eth] of allocations) {
    const address = privateKeyToAccount(input[`${role}_private_key`]).address
    const code = await client.getCode({ address })
    if (code && code !== '0x') throw new Error('Unexpected target contract')
    const balance = await client.getBalance({ address })
    const desired = parseEther(eth)
    targets.push({ role, address, balance, value: balance < desired ? desired - balance : 0n })
  }
  const total = targets.reduce((sum, t) => sum + t.value, 0n)
  if (total > parseEther('.008')) throw new Error('Allocation cap exceeded')
  const sourceBalance = await client.getBalance({ address: source.address })
  console.log(JSON.stringify({ source: source.address, sourceBalance, total, targets }, (_, v) => typeof v === 'bigint' ? v.toString() : v))
  if (process.argv[2] === '--fund') {
    if (sourceBalance < total + parseEther('.0006')) throw new Error('Insufficient bounded funding reserve')
    for (const t of targets.filter(t => t.value > 0n)) {
      const fees = await client.estimateFeesPerGas()
      const gas = (await client.estimateGas({ account: source, to: t.address, value: t.value })) * 120n / 100n
      if (gas * fees.maxFeePerGas > parseEther('.0001')) throw new Error('Per-transfer fee cap exceeded')
      const hash = await wallet.sendTransaction({ to: t.address, value: t.value, gas, ...fees })
      const receipt = await client.waitForTransactionReceipt({ hash })
      if (receipt.status !== 'success') throw new Error('Funding receipt failed')
      console.log(JSON.stringify({ role: t.role, address: t.address, hash, valueWei: t.value.toString(), feeWei: (receipt.gasUsed * receipt.effectiveGasPrice).toString() }))
    }
  }
} catch { console.error('Frankfurt funding stopped; sensitive details withheld'); process.exitCode = 1 }
