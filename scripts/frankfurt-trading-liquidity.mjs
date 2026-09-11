// Normal asynchronous LP request; no owner-only bypass, Core deployment or governance changes.
import { readFileSync } from 'node:fs'
import { createPublicClient, createWalletClient, http, parseAbi, parseEther, keccak256 } from '../apps/frontend/node_modules/viem/_esm/index.js'
import { privateKeyToAccount } from '../apps/frontend/node_modules/viem/_esm/accounts/index.js'
import { arbitrumSepolia } from '../apps/frontend/node_modules/viem/_esm/chains/index.js'
import { readSecret, verifyAccount } from './frankfurt-aa-secrets.mjs'

try {
  if (!['--check', '--provide'].includes(process.argv[2])) throw new Error('Explicit mode required')
  verifyAccount()
  const manifest = JSON.parse(readFileSync(new URL('../config/perps/arbitrum-sepolia-v2.json', import.meta.url)))
  if (manifest.release.sourceCommit !== 'ffe45937b7f38133133ad292c5435828bf99357d') throw new Error('Wrong release')
  const transport = http(readSecret('/plether/sepolia-aa-temp/perps-rpc-url'))
  const client = createPublicClient({ chain: arbitrumSepolia, transport })
  const account = privateKeyToAccount(readSecret('/plether/bootstrap/sepolia-aa-temp/deployer-private-key'))
  const wallet = createWalletClient({ account, chain: arbitrumSepolia, transport })
  if (await client.getChainId() !== 421614 || account.address.toLowerCase() !== '0x4db6e8d6f9cd43d3e3e1cdc2eb9c9a4c41d1795c') throw new Error('Wrong chain/source')
  for (const name of ['mockUsdc', 'housePool', 'juniorVault']) {
    const c = manifest.contracts[name]
    if (keccak256(await client.getCode({ address: c.address })) !== c.runtimeCodeHash) throw new Error('Code mismatch')
  }
  const token = manifest.contracts.mockUsdc.address
  const vault = manifest.contracts.juniorVault.address
  const controller = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
  const amount = 100_000n * 1_000_000n
  const tokenAbi = parseAbi(['function balanceOf(address) view returns (uint256)', 'function allowance(address,address) view returns (uint256)', 'function mint(address,uint256)', 'function approve(address,uint256) returns (bool)'])
  const vaultAbi = parseAbi(['function maxRequestDeposit(address) view returns (uint256)', 'function getRequestEpochWindow() view returns (uint256,uint256)', 'function pendingDepositEscrowAssets() view returns (uint256)', 'function totalAssets() view returns (uint256)', 'function requestDeposit(uint256,address,address) returns (uint256)'])
  const read = (functionName, args = []) => client.readContract({ address: vault, abi: vaultAbi, functionName, args })
  const max = await read('maxRequestDeposit', [controller])
  const [epoch, cutoff] = await read('getRequestEpochWindow')
  const escrow = await read('pendingDepositEscrowAssets')
  const assets = await read('totalAssets')
  console.log(JSON.stringify({ max, epoch, cutoff, settleAt: epoch * 3600n, escrow, assets, controller, amount }, (_, v) => typeof v === 'bigint' ? v.toString() : v))
  if (process.argv[2] === '--provide') {
    // Any existing substantial liquidity or queued request requires inspection,
    // not an automatic second mint/deposit on retry.
    if (escrow !== 0n || assets >= amount) throw new Error('Existing liquidity requires inspection')
    if (max < amount) throw new Error('Deposit gate is not ready')
    const send = async (address, abi, functionName, args) => {
      const { request } = await client.simulateContract({ account, address, abi, functionName, args })
      const fees = await client.estimateFeesPerGas()
      const gas = (await client.estimateContractGas(request)) * 120n / 100n
      console.log(JSON.stringify({ functionName, estimatedGas: String(gas), maxCostWei: String(gas * fees.maxFeePerGas) }))
      if (gas * fees.maxFeePerGas > parseEther(functionName === 'requestDeposit' ? '.0003' : '.0001')) throw new Error('Fee cap exceeded')
      const hash = await wallet.writeContract({ ...request, ...fees, gas })
      const receipt = await client.waitForTransactionReceipt({ hash })
      if (receipt.status !== 'success') throw new Error('Transaction failed')
      console.log(JSON.stringify({ functionName, hash, feeWei: String(receipt.gasUsed * receipt.effectiveGasPrice) }))
    }
    const balance = await client.readContract({ address: token, abi: tokenAbi, functionName: 'balanceOf', args: [account.address] })
    if (balance < amount) await send(token, tokenAbi, 'mint', [account.address, amount - balance])
    const allowance = await client.readContract({ address: token, abi: tokenAbi, functionName: 'allowance', args: [account.address, vault] })
    if (allowance !== amount) await send(token, tokenAbi, 'approve', [vault, amount])
    await send(vault, vaultAbi, 'requestDeposit', [amount, controller, account.address])
  }
} catch (error) {
  const reverted = error.walk?.(cause => cause.name === 'ContractFunctionRevertedError')
  console.error(JSON.stringify({ error: 'Frankfurt LP preparation stopped', type: error.name,
    reason: ['Existing liquidity requires inspection', 'Deposit gate is not ready', 'Fee cap exceeded', 'Transaction failed'].includes(error.message) ? error.message : undefined,
    revert: reverted?.data?.errorName, revertData: reverted?.raw,
  }))
  process.exitCode = 1
}
