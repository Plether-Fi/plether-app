import { type Config, getPublicClient, getWalletClient, waitForTransactionReceipt, writeContract, readContract, signTypedData } from '@wagmi/core'
import { useTransactionStore, type TransactionType } from '../stores/transactionStore'
import { useTransactionModal } from '../hooks/useTransactionModal'
import { type ContractAddresses, getAddresses } from '../contracts/addresses'
import { STAKED_TOKEN_ABI, ERC20_ABI, CURVE_POOL_ABI, ZAP_ROUTER_ABI, PLETH_CORE_ABI, LEVERAGE_ROUTER_ABI, MORPHO_ABI } from '../contracts/abis'
import { parseTransactionError, getErrorMessage } from '../utils/errors'
import { getDeadline } from '../utils/deadline'
import { EIP2612_PERMIT_TYPES, splitSignature } from '../utils/permit'

type OperationStatus = 'pending' | 'signing' | 'approving' | 'submitted' | 'confirming' | 'success' | 'error'
type PermitResult = { v: number; r: `0x${string}`; s: `0x${string}`; deadline: bigint }

interface PendingOperation {
  id: string
  transactionId: string
  status: OperationStatus
  hash?: `0x${string}`
  error?: string
}

interface Prerequisite {
  label: string
  execute: (config: Config, onConfirming: () => void) => Promise<void>
}

interface OperationConfig {
  operationKey: string
  txType: TransactionType
  title: string
  permitSign?: () => Promise<void>
  prerequisites: Prerequisite[]
  mainStep: {
    label: string
    execute: (config: Config) => Promise<`0x${string}`>
  }
  onRetry: () => void
}

interface OperationContext {
  config: Config
  chainId: number
  address: `0x${string}`
  addresses: ContractAddresses
}

class TransactionManager {
  private config: Config | null = null
  private pendingOperations = new Map<string, PendingOperation>()

  setConfig(config: Config) {
    this.config = config
  }

  private getConfig(): Config {
    if (!this.config) {
      throw new Error('TransactionManager not initialized. Call setConfig first.')
    }
    return this.config
  }

  private async getOperationContext(operationKey: string): Promise<OperationContext | null> {
    const config = this.getConfig()

    const existing = this.pendingOperations.get(operationKey)
    if (existing && existing.status !== 'success' && existing.status !== 'error') {
      console.warn(`[TxManager] Operation ${operationKey} already in progress`)
      return null
    }

    const chainId = config.state.chainId
    if (!chainId) throw new Error('No chain connected')

    const walletClient = await getWalletClient(config)
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition -- defensive runtime check
    if (!walletClient) throw new Error('No wallet connected')

    return {
      config,
      chainId,
      address: walletClient.account.address,
      addresses: getAddresses(chainId),
    }
  }

  private makeApprovalPrerequisite(
    label: string,
    token: `0x${string}`,
    spender: `0x${string}`,
    amount: bigint,
  ): Prerequisite {
    return {
      label,
      execute: async (_config, onConfirming) => {
        await this.executeApproval(token, spender, amount, onConfirming)
      },
    }
  }

  private async executeOperation(ctx: OperationContext, opConfig: OperationConfig): Promise<void> {
    const { config, chainId } = ctx
    const { operationKey, txType, title, permitSign, prerequisites, mainStep, onRetry } = opConfig

    const steps: string[] = []
    if (permitSign) steps.push('Sign permit')
    for (const prereq of prerequisites) {
      steps.push(prereq.label, 'Confirming onchain (~12s)')
    }
    steps.push(mainStep.label, 'Confirming onchain (~12s)')

    const transactionId = crypto.randomUUID()
    const txStore = useTransactionStore.getState()
    const txModal = useTransactionModal.getState()

    txStore.addTransaction({
      id: transactionId,
      type: txType,
      status: 'pending',
      title,
      steps: steps.map(label => ({ label, status: 'pending' as const })),
      chainId,
    })

    txStore.setActiveOperation(operationKey, transactionId)
    txModal.open({ transactionId, onRetry })

    const operation: PendingOperation = {
      id: operationKey,
      transactionId,
      status: permitSign ? 'signing' : prerequisites.length > 0 ? 'approving' : 'submitted',
    }
    this.pendingOperations.set(operationKey, operation)

    try {
      let stepIndex = 0

      if (permitSign) {
        txStore.setStepInProgress(transactionId, stepIndex)
        await permitSign()
        stepIndex = 1
        operation.status = prerequisites.length > 0 ? 'approving' : 'submitted'
      }

      for (const prereq of prerequisites) {
        txStore.setStepInProgress(transactionId, stepIndex)
        const confirmStep = stepIndex + 1
        await prereq.execute(config, () => {
          txStore.setStepInProgress(transactionId, confirmStep)
        })
        stepIndex += 2
      }

      operation.status = 'submitted'
      txStore.setStepInProgress(transactionId, stepIndex)

      const hash = await mainStep.execute(config)

      operation.hash = hash
      operation.status = 'confirming'
      txStore.updateTransaction(transactionId, { hash })
      txStore.setStepInProgress(transactionId, stepIndex + 1)

      const receipt = await waitForTransactionReceipt(config, { hash })

      if (receipt.status === 'reverted') {
        const client = getPublicClient(config)
        if (client) {
          const tx = await client.getTransaction({ hash })
          if (tx.to) {
            await client.call({
              to: tx.to,
              data: tx.input,
              account: tx.from,
              value: tx.value,
              blockNumber: receipt.blockNumber,
            })
          }
        }
        throw new Error('Transaction reverted')
      }

      operation.status = 'success'
      txStore.setStepSuccess(transactionId, hash)
      txStore.clearActiveOperation(operationKey)

    } catch (err) {
      operation.status = 'error'
      operation.error = err instanceof Error ? err.message : String(err)

      const error = parseTransactionError(err)
      const message = getErrorMessage(error)

      txStore.setStepError(transactionId, 0, message)
      txStore.clearActiveOperation(operationKey)
    }
  }

  private async checkAllowance(
    tokenAddress: `0x${string}`,
    spenderAddress: `0x${string}`,
    ownerAddress: `0x${string}`,
    amount: bigint
  ): Promise<boolean> {
    const config = this.getConfig()
    const allowance = await readContract(config, {
      address: tokenAddress,
      abi: ERC20_ABI,
      functionName: 'allowance',
      args: [ownerAddress, spenderAddress],
    })
    return allowance >= amount
  }

  private async executeApproval(
    tokenAddress: `0x${string}`,
    spenderAddress: `0x${string}`,
    amount: bigint,
    onSubmitted?: () => void
  ): Promise<`0x${string}`> {
    const config = this.getConfig()
    const hash = await writeContract(config, {
      address: tokenAddress,
      abi: ERC20_ABI,
      functionName: 'approve',
      args: [spenderAddress, amount],
    })
    onSubmitted?.()
    await waitForTransactionReceipt(config, { hash })
    return hash
  }

  private async signPermit(
    tokenAddress: `0x${string}`,
    spender: `0x${string}`,
    amount: bigint,
    ctx: OperationContext,
  ): Promise<{ v: number; r: `0x${string}`; s: `0x${string}`; deadline: bigint }> {
    const { config, chainId, address } = ctx

    const [nonce, eip712Domain] = await Promise.all([
      readContract(config, {
        address: tokenAddress,
        abi: ERC20_ABI,
        functionName: 'nonces',
        args: [address],
      }),
      readContract(config, {
        address: tokenAddress,
        abi: ERC20_ABI,
        functionName: 'eip712Domain',
      }),
    ])

    const [, name, version] = eip712Domain
    const deadline = getDeadline(60)

    const signature = await signTypedData(config, {
      domain: {
        name,
        version,
        chainId,
        verifyingContract: tokenAddress,
      },
      types: EIP2612_PERMIT_TYPES,
      primaryType: 'Permit',
      message: {
        owner: address,
        spender,
        value: amount,
        nonce,
        deadline,
      },
    })

    const { r, s, v } = splitSignature(signature)
    return { v, r, s, deadline }
  }

  async executeUnstake(
    side: 'BEAR' | 'BULL',
    shares: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const operationKey = `unstake-${side}`
    const ctx = await this.getOperationContext(operationKey)
    if (!ctx) return

    const { addresses, address } = ctx
    const stakingAddress = side === 'BEAR' ? addresses.STAKING_BEAR : addresses.STAKING_BULL

    await this.executeOperation(ctx, {
      operationKey,
      txType: 'unstake',
      title: `Unstaking splDXY-${side}`,
      prerequisites: [],
      mainStep: {
        label: `Unstake splDXY-${side}`,
        execute: (config) => writeContract(config, {
          address: stakingAddress,
          abi: STAKED_TOKEN_ABI,
          functionName: 'redeem',
          args: [shares, address, address],
        }),
      },
      onRetry: options?.onRetry ?? (() => void this.executeUnstake(side, shares, options)),
    })
  }

  async executeCloseLeverage(
    side: 'BEAR' | 'BULL',
    collateralToWithdraw: bigint,
    slippageBps: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const operationKey = `leverage-close-${side}`
    const ctx = await this.getOperationContext(operationKey)
    if (!ctx) return

    const { addresses } = ctx
    const routerAddress = side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER
    const deadline = getDeadline()

    await this.executeOperation(ctx, {
      operationKey,
      txType: 'leverage',
      title: `Closing ${side} leverage position`,
      prerequisites: [],
      mainStep: {
        label: `Close ${side} position`,
        execute: (config) => writeContract(config, {
          address: routerAddress,
          abi: LEVERAGE_ROUTER_ABI,
          functionName: 'closeLeverage',
          args: [collateralToWithdraw, slippageBps, deadline],
        }),
      },
      onRetry: options?.onRetry ?? (() => void this.executeCloseLeverage(side, collateralToWithdraw, slippageBps, options)),
    })
  }

  async executeMint(
    pairAmount: bigint,
    usdcRequired: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const operationKey = 'mint'
    const ctx = await this.getOperationContext(operationKey)
    if (!ctx) return

    const { addresses } = ctx
    let permit!: PermitResult

    await this.executeOperation(ctx, {
      operationKey,
      txType: 'mint',
      title: 'Minting token pairs',
      permitSign: async () => {
        permit = await this.signPermit(addresses.USDC, addresses.SYNTHETIC_SPLITTER, usdcRequired, ctx)
      },
      prerequisites: [],
      mainStep: {
        label: 'Mint pairs',
        execute: (config) => writeContract(config, {
          address: addresses.SYNTHETIC_SPLITTER,
          abi: PLETH_CORE_ABI,
          functionName: 'mintWithPermit',
          args: [pairAmount, permit.deadline, permit.v, permit.r, permit.s],
        }),
      },
      onRetry: options?.onRetry ?? (() => void this.executeMint(pairAmount, usdcRequired, options)),
    })
  }

  async executeStake(
    side: 'BEAR' | 'BULL',
    amount: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const operationKey = `stake-${side}`
    const ctx = await this.getOperationContext(operationKey)
    if (!ctx) return

    const { addresses, address } = ctx
    const tokenAddress = side === 'BEAR' ? addresses.DXY_BEAR : addresses.DXY_BULL
    const stakingAddress = side === 'BEAR' ? addresses.STAKING_BEAR : addresses.STAKING_BULL

    let permit!: PermitResult

    await this.executeOperation(ctx, {
      operationKey,
      txType: 'stake',
      title: `Staking plDXY-${side}`,
      permitSign: async () => {
        permit = await this.signPermit(tokenAddress, stakingAddress, amount, ctx)
      },
      prerequisites: [],
      mainStep: {
        label: `Stake plDXY-${side}`,
        execute: (config) => writeContract(config, {
          address: stakingAddress,
          abi: STAKED_TOKEN_ABI,
          functionName: 'depositWithPermit',
          args: [amount, address, permit.deadline, permit.v, permit.r, permit.s],
        }),
      },
      onRetry: options?.onRetry ?? (() => void this.executeStake(side, amount, options)),
    })
  }

  async executeCurveSwap(
    mode: 'buy' | 'sell',
    amount: bigint,
    minAmountOut: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const operationKey = `swap-${mode}-bear`
    const ctx = await this.getOperationContext(operationKey)
    if (!ctx) return

    const { addresses, address } = ctx
    const tokenAddress = mode === 'buy' ? addresses.USDC : addresses.DXY_BEAR
    const tokenSymbol = mode === 'buy' ? 'USDC' : 'plDXY-BEAR'

    const hasAllowance = await this.checkAllowance(tokenAddress, addresses.CURVE_POOL, address, amount)

    const prerequisites: Prerequisite[] = []
    if (!hasAllowance) {
      prerequisites.push(this.makeApprovalPrerequisite(`Approve ${tokenSymbol}`, tokenAddress, addresses.CURVE_POOL, amount))
    }

    const USDC_INDEX = 0n
    const BEAR_INDEX = 1n
    const i = mode === 'buy' ? USDC_INDEX : BEAR_INDEX
    const j = mode === 'buy' ? BEAR_INDEX : USDC_INDEX

    await this.executeOperation(ctx, {
      operationKey,
      txType: 'swap',
      title: mode === 'buy' ? 'Buying plDXY-BEAR' : 'Selling plDXY-BEAR',
      prerequisites,
      mainStep: {
        label: mode === 'buy' ? 'Buy plDXY-BEAR' : 'Sell plDXY-BEAR',
        execute: (config) => writeContract(config, {
          address: addresses.CURVE_POOL,
          abi: CURVE_POOL_ABI,
          functionName: 'exchange',
          args: [i, j, amount, minAmountOut, address],
        }),
      },
      onRetry: options?.onRetry ?? (() => void this.executeCurveSwap(mode, amount, minAmountOut, options)),
    })
  }

  async executeZapBuy(
    usdcAmount: bigint,
    minBullOut: bigint,
    slippageBps: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const operationKey = 'swap-buy-bull'
    const ctx = await this.getOperationContext(operationKey)
    if (!ctx) return

    const { addresses } = ctx
    let permit!: PermitResult

    await this.executeOperation(ctx, {
      operationKey,
      txType: 'swap',
      title: 'Buying plDXY-BULL',
      permitSign: async () => {
        permit = await this.signPermit(addresses.USDC, addresses.ZAP_ROUTER, usdcAmount, ctx)
      },
      prerequisites: [],
      mainStep: {
        label: 'Buy plDXY-BULL',
        execute: (config) => writeContract(config, {
          address: addresses.ZAP_ROUTER,
          abi: ZAP_ROUTER_ABI,
          functionName: 'zapMintWithPermit',
          args: [usdcAmount, minBullOut, slippageBps, permit.deadline, permit.v, permit.r, permit.s],
        }),
      },
      onRetry: options?.onRetry ?? (() => void this.executeZapBuy(usdcAmount, minBullOut, slippageBps, options)),
    })
  }

  async executeZapSell(
    bullAmount: bigint,
    minUsdcOut: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const operationKey = 'swap-sell-bull'
    const ctx = await this.getOperationContext(operationKey)
    if (!ctx) return

    const { addresses, address } = ctx
    const hasAllowance = await this.checkAllowance(addresses.DXY_BULL, addresses.ZAP_ROUTER, address, bullAmount)

    const prerequisites: Prerequisite[] = []
    if (!hasAllowance) {
      prerequisites.push(this.makeApprovalPrerequisite('Approve plDXY-BULL', addresses.DXY_BULL, addresses.ZAP_ROUTER, bullAmount))
    }

    const deadline = getDeadline()

    await this.executeOperation(ctx, {
      operationKey,
      txType: 'swap',
      title: 'Selling plDXY-BULL',
      prerequisites,
      mainStep: {
        label: 'Sell plDXY-BULL',
        execute: (config) => writeContract(config, {
          address: addresses.ZAP_ROUTER,
          abi: ZAP_ROUTER_ABI,
          functionName: 'zapBurn',
          args: [bullAmount, minUsdcOut, deadline],
        }),
      },
      onRetry: options?.onRetry ?? (() => void this.executeZapSell(bullAmount, minUsdcOut, options)),
    })
  }

  private async adjustBurnAmount(
    splitterAddress: `0x${string}`,
    pairAmount: bigint,
    config: Config,
  ): Promise<bigint> {
    const USDC_MULTIPLIER = 10n ** 12n

    const [cap, previewResult] = await Promise.all([
      readContract(config, {
        address: splitterAddress,
        abi: PLETH_CORE_ABI,
        functionName: 'CAP',
      }),
      readContract(config, {
        address: splitterAddress,
        abi: PLETH_CORE_ABI,
        functionName: 'previewBurn',
        args: [pairAmount],
      }),
    ])

    const theoreticalRefund = (pairAmount * cap) / USDC_MULTIPLIER
    const [usdcToReturn] = previewResult

    if (usdcToReturn < theoreticalRefund) {
      return pairAmount - 1n
    }
    return pairAmount
  }

  async executeRedeem(
    pairAmount: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const operationKey = 'redeem'
    const ctx = await this.getOperationContext(operationKey)
    if (!ctx) return

    const { config, addresses, address } = ctx
    const adjustedAmount = await this.adjustBurnAmount(addresses.SYNTHETIC_SPLITTER, pairAmount, config)

    const hasBearAllowance = await this.checkAllowance(addresses.DXY_BEAR, addresses.SYNTHETIC_SPLITTER, address, adjustedAmount)
    const hasBullAllowance = await this.checkAllowance(addresses.DXY_BULL, addresses.SYNTHETIC_SPLITTER, address, adjustedAmount)

    const prerequisites: Prerequisite[] = []
    if (!hasBearAllowance) {
      prerequisites.push(this.makeApprovalPrerequisite('Approve plDXY-BEAR', addresses.DXY_BEAR, addresses.SYNTHETIC_SPLITTER, adjustedAmount))
    }
    if (!hasBullAllowance) {
      prerequisites.push(this.makeApprovalPrerequisite('Approve plDXY-BULL', addresses.DXY_BULL, addresses.SYNTHETIC_SPLITTER, adjustedAmount))
    }

    await this.executeOperation(ctx, {
      operationKey,
      txType: 'burn',
      title: 'Redeeming token pairs',
      prerequisites,
      mainStep: {
        label: 'Redeem pairs',
        execute: (config) => writeContract(config, {
          address: addresses.SYNTHETIC_SPLITTER,
          abi: PLETH_CORE_ABI,
          functionName: 'burn',
          args: [adjustedAmount],
        }),
      },
      onRetry: options?.onRetry ?? (() => void this.executeRedeem(pairAmount, options)),
    })
  }

  async executeOpenLeverage(
    side: 'BEAR' | 'BULL',
    principal: bigint,
    leverage: bigint,
    slippageBps: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const operationKey = `leverage-open-${side}`
    const ctx = await this.getOperationContext(operationKey)
    if (!ctx) return

    const { config, addresses, address } = ctx
    const routerAddress = side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER

    const morphoAddress = await readContract(config, {
      address: routerAddress,
      abi: LEVERAGE_ROUTER_ABI,
      functionName: 'MORPHO',
    })

    const isAuthorized = await readContract(config, {
      address: morphoAddress,
      abi: MORPHO_ABI,
      functionName: 'isAuthorized',
      args: [address, routerAddress],
    })

    const prerequisites: Prerequisite[] = []
    if (!isAuthorized) {
      prerequisites.push({
        label: 'Authorize Morpho',
        execute: async (cfg, onConfirming) => {
          const authHash = await writeContract(cfg, {
            address: morphoAddress,
            abi: MORPHO_ABI,
            functionName: 'setAuthorization',
            args: [routerAddress, true],
          })
          onConfirming()
          await waitForTransactionReceipt(cfg, { hash: authHash })
        },
      })
    }

    let permit!: PermitResult

    await this.executeOperation(ctx, {
      operationKey,
      txType: 'leverage',
      title: `Opening ${side} leverage position`,
      permitSign: async () => {
        permit = await this.signPermit(addresses.USDC, routerAddress, principal, ctx)
      },
      prerequisites,
      mainStep: {
        label: `Open ${side} position`,
        execute: (cfg) => writeContract(cfg, {
          address: routerAddress,
          abi: LEVERAGE_ROUTER_ABI,
          functionName: 'openLeverageWithPermit',
          args: [principal, leverage, slippageBps, permit.deadline, permit.v, permit.r, permit.s],
        }),
      },
      onRetry: options?.onRetry ?? (() => void this.executeOpenLeverage(side, principal, leverage, slippageBps, options)),
    })
  }

  getOperation(operationKey: string): PendingOperation | undefined {
    return this.pendingOperations.get(operationKey)
  }

  isOperationPending(operationKey: string): boolean {
    const op = this.getOperation(operationKey)
    return !!op && op.status !== 'success' && op.status !== 'error'
  }
}

export const transactionManager = new TransactionManager()

if (import.meta.env.DEV) {
  (window as unknown as { __txManager: TransactionManager }).__txManager = transactionManager
}
