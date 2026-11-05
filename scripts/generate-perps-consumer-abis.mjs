#!/usr/bin/env node
// Consumer ABIs are generated from compiled source, never inferred from deployed addresses.
import { createHash } from 'node:crypto'
import { execFileSync } from 'node:child_process'
import { readFileSync, writeFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const args = process.argv.slice(2)
const option = name => { const at = args.indexOf(name); return at < 0 ? undefined : args[at + 1] }
const artifactsDir = option('--artifacts-dir')
const sourceRoot = option('--source-root')
const check = args.includes('--check')
if (!artifactsDir) throw new Error('Usage: node scripts/generate-perps-consumer-abis.mjs --artifacts-dir <compiled-artifacts> [--source-root <core-checkout>] [--check]')
const sha256 = value => createHash('sha256').update(value).digest('hex')
const stripInternalType = value => Array.isArray(value) ? value.map(stripInternalType)
  : value && typeof value === 'object' ? Object.fromEntries(Object.entries(value).filter(([key]) => key !== 'internalType').map(([key, child]) => [key, stripInternalType(child)])) : value

// Function/event allowlists are deliberate application dependencies. Errors follow
// the compiled contract so decoding cannot retain deleted or renamed selectors.
const consumers =
{
  "PERPS_PUBLIC_LENS_ABI": {
    "contract": "PerpsPublicLens",
    "functions": [
      "ENGINE",
      "ORDER_ROUTER",
      "HOUSE_POOL",
      "getProtocolStatus",
      "getLpStatus",
      "getTraderAccount",
      "getPosition",
      "getPendingOrders",
      "getActivePositionProtection",
      "isLiquidatable",
      "getSeniorTranche",
      "getJuniorTranche",
      "getTrancheQueues",
      "getLpRequestState",
      "getLpDepositCooldownState"
    ],
    "events": []
  },
  "PERPS_MARGIN_CLEARINGHOUSE_ABI": {
    "contract": "MarginClearinghouse",
    "functions": [
      "getOrderReservation",
      "vpiRebateReserveAssets",
      "depositMargin",
      "withdrawMargin",
      "getAccountEquityAssets",
      "getFreeBuyingPowerAssets"
    ],
    "events": [
      "Deposit",
      "Withdraw"
    ]
  },
  "PERPS_ORDER_ROUTER_ABI": {
    "contract": "OrderRouter",
    "functions": [
      "commitOrder",
      "executeOrder",
      "expireOrder",
      "getPendingOrderView",
      "maxExecutionWindowSeconds",
      "maxPendingOrders",
      "minOpenNotionalAssets",
      "openOrderExecutionBountyBps",
      "minOpenOrderExecutionBountyAssets",
      "maxOpenOrderExecutionBountyAssets",
      "engine",
      "lifecycleBook",
      "policyEvaluator",
      "positionProtectionBook",
      "executeOrderBatch",
      "executeLiquidation",
      "updateMarkPrice"
    ],
    "events": [
      "OrderCommitted"
    ]
  },
  "PERPS_ORDER_LIFECYCLE_BOOK_ABI": {
    "contract": "OrderLifecycleBook",
    "functions": [
      "INTENT_TYPEHASH",
      "ROUTER",
      "ENGINE",
      "CLEARINGHOUSE",
      "HOUSE_POOL",
      "currentExecutionConfigHash",
      "resolveClientIntent",
      "clientIntent",
      "orderTiming",
      "pendingPolicy",
      "lifecycleStatus",
      "terminalOutcome"
    ],
    "events": [
      "IntentRegistered",
      "OrderFinalized"
    ]
  },
  "PERPS_ORDER_POLICY_EVALUATOR_ABI": {
    "contract": "CfdOrderPolicyEvaluator",
    "functions": [
      "assessOrder"
    ],
    "events": []
  },
  "PERPS_CFD_ENGINE_ABI": {
    "contract": "CfdEngine",
    "functions": [
      "positionEntryCostAssetAtoms",
      "positionCarryState",
      "unsettledCarryAssets",
      "sideCarryIndex",
      "sideCarryTimestamp",
      "sideBorrowBaseAssets",
      "sides",
      "riskParams",
      "CAP_PRICE",
      "lastMarkPrice",
      "clearinghouse",
      "pool",
      "isFadWindow",
      "positions",
      "executionFeeBps",
      "settleTraderClaim",
      "addMargin"
    ],
    "events": [
      "PositionOpened",
      "PositionClosed",
      "PositionLiquidated",
      "MarginAdded"
    ]
  },
  "PERPS_HOUSE_POOL_ABI": {
    "contract": "HousePool",
    "functions": [
      "totalAssets",
      "getPoolLiquidityView",
      "getPendingTrancheState",
      "maxSeniorExposureAssets",
      "maxSeniorShareBps",
      "seniorRateBps",
      "getSeniorDepositCapacity",
      "reservedSeniorDepositAssets",
      "areSeniorDepositReservationsWithinLimits",
      "minTrancheDepositAssets"
    ],
    "events": []
  },
  "PERPS_PLETHER_ORACLE_ABI": {
    "contract": "PletherOracle",
    "functions": [
      "getUpdateFee",
      "orderSettlementWindow",
      "getLatestPrice",
      "claimableEth",
      "claimEthRefund",
      "getOrderExecutionPolicy"
    ],
    "events": []
  },
  "PERPS_CFD_ENGINE_LENS_ABI": {
    "contract": "CfdEngineLens",
    "functions": [
      "quoteMaxOpen",
      "previewOpen",
      "previewOpenRevertCode",
      "previewOpenFailurePolicyCategory",
      "previewClose",
      "previewLiquidation"
    ],
    "events": []
  },
  "PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI": {
    "contract": "CfdEngineAccountLens",
    "functions": [
      "getAccountCollateralView",
      "getWithdrawableAssets",
      "getAccountLedgerView",
      "getAccountLedgerSnapshot"
    ],
    "events": []
  },
  "PERPS_CFD_CLOSE_PREVIEW_ABI": {
    "contract": "CfdClosePreview",
    "functions": [
      "previewClose"
    ],
    "events": []
  },
  "TRANCHE_VAULT_READ_ABI": {
    "contract": "TrancheVault",
    "functions": [
      "totalAssets",
      "totalSupply",
      "convertToAssets",
      "balanceOf",
      "getRequestEpochWindow",
      "maxRequestDeposit",
      "maxRequestRedeem",
      "DEPOSIT_COOLDOWN",
      "lastDepositTime",
      "depositEpochActivationTime",
      "maxRequestRedeemFromClaimableDeposit",
      "estimateDepositShares",
      "estimateMintAssets",
      "estimateWithdrawShares",
      "estimateRedeemAssets",
      "requestDeposit",
      "requestRedeem",
      "requestRedeemFromClaimableDeposit",
      "cancelPendingDeposit",
      "cancelRedeemRequest",
      "claimDepositShares",
      "claimRedeem",
      "claimRedeemRefund"
    ],
    "events": []
  },
  "PERPS_POSITION_PROTECTION_BOOK_ABI": {
    "contract": "PositionProtectionBook",
    "full": true
  }
}

const sourceAbis = {}
const provenance = { kind: 'compiled-source-abis', deployed: false, contracts: {} }
if (sourceRoot) {
  // Attribute the snapshot to a commit only when the tracked source tree is clean.
  // Content hashes below remain available for a dirty or non-Git source directory.
  try {
    const git = (...args) => execFileSync('git', ['-C', resolve(sourceRoot), ...args], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore'] }).trim()
    if (!git('status', '--porcelain', '--untracked-files=no')) provenance.sourceCommit = git('rev-parse', 'HEAD')
  } catch { /* Source hashes are sufficient when Git metadata is unavailable. */ }
}
const cache = new Map()
function compiled(contract) {
  if (cache.has(contract)) return cache.get(contract)
  const artifact = JSON.parse(readFileSync(resolve(artifactsDir, `${contract}.sol`, `${contract}.json`), 'utf8'))
  if (!Array.isArray(artifact.abi)) throw new Error(`Missing compiled ABI for ${contract}`)
  let metadata = artifact.metadata
  if (typeof metadata === 'string') metadata = JSON.parse(metadata)
  if (!metadata && artifact.rawMetadata) metadata = JSON.parse(artifact.rawMetadata)
  const sourceName = artifact.sourceName ?? Object.keys(metadata?.settings?.compilationTarget ?? {}).find(key => metadata.settings.compilationTarget[key] === contract)
  const compilerVersion = metadata?.compiler?.version ?? (artifact.validation?.includes('Solc0.8.35') ? '0.8.35' : undefined)
  if (!sourceName || !compilerVersion) throw new Error(`Missing source/compiler provenance for ${contract}`)
  provenance.contracts[contract] = {
    sourceName,
    compilerVersion,
    abiSha256: sha256(JSON.stringify(artifact.abi)),
    validation: artifact.validation ?? 'Compiled source ABI; runtime deployment not verified',
    ...(sourceRoot ? { sourceSha256: sha256(readFileSync(resolve(sourceRoot, sourceName))) } : {}),
  }
  cache.set(contract, artifact.abi)
  return artifact.abi
}
function consumerAbi(name) {
  const spec = consumers[name]
  const abi = compiled(spec.contract)
  sourceAbis[name] = abi
  if (spec.full) return stripInternalType(abi)
  for (const type of ['function', 'event']) {
    for (const name of spec[type === 'function' ? 'functions' : 'events']) {
      if (!abi.some(item => item.type === type && item.name === name)) throw new Error(`Missing ${spec.contract}.${name} in compiled source`)
    }
  }
  return stripInternalType(abi.filter(item => item.type === 'error' || (item.type === 'function' && spec.functions.includes(item.name)) || (item.type === 'event' && spec.events.includes(item.name))))
}
const generated = Object.fromEntries(Object.keys(consumers).map(name => [name, consumerAbi(name)]))
const request = generated.PERPS_ORDER_ROUTER_ABI.find(item => item.type === 'function' && item.name === 'commitOrder').inputs[0].components
const bounds = request.find(item => item.name === 'bounds').components
const header = '// Generated by scripts/generate-perps-consumer-abis.mjs from compiled source ABIs.\n// See perps-canonical-source-abis.provenance.json for source/compiler hashes; no deployed-bytecode claim.\n'
const constant = (name, abi) => `export const ${name} = ${JSON.stringify(abi, null, 2)} as const\n`
const outputs = new Map()
const perps = Object.entries(generated).filter(([name]) => !['PERPS_CFD_CLOSE_PREVIEW_ABI', 'TRANCHE_VAULT_READ_ABI'].includes(name))
outputs.set('apps/frontend/src/contracts/abis/Perps.ts', header + "export { PERPS_CFD_CLOSE_PREVIEW_ABI } from './CfdClosePreview'\n\n" + constant('PERPS_ORDER_REQUEST_V3_COMPONENTS', request) + '\n' + constant('PERPS_EXECUTION_BOUNDS_COMPONENTS', bounds) + '\n' + perps.map(([name, abi]) => constant(name, abi)).join('\n'))
outputs.set('apps/frontend/src/contracts/abis/CfdClosePreview.ts', header + constant('PERPS_CFD_CLOSE_PREVIEW_ABI', generated.PERPS_CFD_CLOSE_PREVIEW_ABI))
outputs.set('apps/frontend/src/contracts/abis/TrancheVault.ts', header + constant('TRANCHE_VAULT_READ_ABI', generated.TRANCHE_VAULT_READ_ABI))
outputs.set('apps/frontend/src/contracts/__tests__/fixtures/perps-canonical-source-abis.json', JSON.stringify(sourceAbis, null, 2) + '\n')
const worker = Object.fromEntries(['PositionProtectionBook', 'OrderRouter', 'OrderLifecycleBook', 'PletherOracle'].map(name => [name, compiled(name)]))
outputs.set('apps/backend/protection-worker/abi.mjs', header + `export default ${JSON.stringify(worker)}\n`)
outputs.set('apps/frontend/src/contracts/__tests__/fixtures/perps-canonical-source-abis.provenance.json', JSON.stringify(provenance, null, 2) + '\n')
for (const [file, text] of outputs) {
  const target = resolve(root, file)
  if (check) {
    if (readFileSync(target, 'utf8') !== text) throw new Error(`Generated ABI is stale: ${file}`)
  } else writeFileSync(target, text)
}
console.log(`${check ? 'Verified' : 'Generated'} ${outputs.size} consumer ABI artifacts from compiled source.`)
