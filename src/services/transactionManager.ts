import { type Config, getPublicClient, getWalletClient, signTypedData, waitForTransactionReceipt, writeContract, readContract } from '@wagmi/core'
import { useTransactionStore } from '../stores/transactionStore'
import { useTransactionModal } from '../hooks/useTransactionModal'
import { getAddresses } from '../contracts/addresses'
import { STAKED_TOKEN_ABI, ERC20_ABI, CURVE_POOL_ABI, ZAP_ROUTER_ABI, PLETH_CORE_ABI, LEVERAGE_ROUTER_ABI, MORPHO_ABI } from '../contracts/abis'
import { parseTransactionError, getErrorMessage } from '../utils/errors'

type OperationStatus = 'pending' | 'signing' | 'approving' | 'submitted' | 'confirming' | 'success' | 'error'

interface PendingOperation {
  id: string
  transactionId: string
  status: OperationStatus
  hash?: `0x${string}`
  error?: string
}

class TransactionManager {
  private config: Config | null = null
  private pendingOperations: Map<string, PendingOperation> = new Map()

  setConfig(config: Config) {
    this.config = config
  }

  private getConfig(): Config {
    if (!this.config) {
      throw new Error('TransactionManager not initialized. Call setConfig first.')
    }
    return this.config
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
    }) as bigint
    return allowance >= amount
  }

  private async executeApproval(
    tokenAddress: `0x${string}`,
    spenderAddress: `0x${string}`,
    amount: bigint
  ): Promise<`0x${string}`> {
    const config = this.getConfig()
    const hash = await writeContract(config, {
      address: tokenAddress,
      abi: ERC20_ABI,
      functionName: 'approve',
      args: [spenderAddress, amount],
    })
    await waitForTransactionReceipt(config, { hash })
    return hash
  }

  async executeStakeWithPermit(
    side: 'BEAR' | 'BULL',
    amount: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const config = this.getConfig()
    const operationKey = `stake-${side}`

    // Check if there's already a pending operation
    const existing = this.pendingOperations.get(operationKey)
    if (existing && existing.status !== 'success' && existing.status !== 'error') {
      console.warn(`[TxManager] Operation ${operationKey} already in progress`)
      return
    }

    const chainId = config.state.chainId
    if (!chainId) {
      throw new Error('No chain connected')
    }

    const walletClient = await getWalletClient(config)
    if (!walletClient) {
      throw new Error('No wallet connected')
    }

    const address = walletClient.account.address
    const addresses = getAddresses(chainId)
    const tokenAddress = side === 'BEAR' ? addresses.DXY_BEAR : addresses.DXY_BULL
    const stakingAddress = side === 'BEAR' ? addresses.STAKING_BEAR : addresses.STAKING_BULL

    // Create transaction in store
    const transactionId = crypto.randomUUID()
    const txStore = useTransactionStore.getState()
    const txModal = useTransactionModal.getState()

    const steps = ['Sign permit', `Stake plDXY-${side}`, 'Awaiting confirmation']
    txStore.addTransaction({
      id: transactionId,
      type: 'stake',
      status: 'pending',
      title: `Staking plDXY-${side}`,
      steps: steps.map(label => ({ label, status: 'pending' as const })),
      chainId,
    })

    txStore.setActiveOperation(operationKey, transactionId)

    txModal.open({
      transactionId,
      onRetry: options?.onRetry ?? (() => void this.executeStakeWithPermit(side, amount, options)),
    })

    const operation: PendingOperation = {
      id: operationKey,
      transactionId,
      status: 'signing',
    }
    this.pendingOperations.set(operationKey, operation)

    try {
      // Step 1: Sign permit
      txStore.setStepInProgress(transactionId, 0)

      const nonce = await readContract(config, {
        address: tokenAddress,
        abi: ERC20_ABI,
        functionName: 'nonces',
        args: [address],
      })

      const tokenName = await readContract(config, {
        address: tokenAddress,
        abi: ERC20_ABI,
        functionName: 'name',
      })

      const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)

      const signature = await signTypedData(config, {
        domain: {
          name: tokenName as string,
          version: '1',
          chainId: chainId,
          verifyingContract: tokenAddress,
        },
        types: {
          Permit: [
            { name: 'owner', type: 'address' },
            { name: 'spender', type: 'address' },
            { name: 'value', type: 'uint256' },
            { name: 'nonce', type: 'uint256' },
            { name: 'deadline', type: 'uint256' },
          ],
        },
        primaryType: 'Permit',
        message: {
          owner: address,
          spender: stakingAddress,
          value: amount,
          nonce: nonce as bigint,
          deadline: deadline,
        },
      })

      // Step 2: Send stake transaction
      operation.status = 'submitted'
      txStore.setStepInProgress(transactionId, 1)

      const r: `0x${string}` = signature.slice(0, 66) as `0x${string}`
      const s: `0x${string}` = `0x${signature.slice(66, 130)}`
      const v = parseInt(signature.slice(130, 132), 16)

      const hash = await writeContract(config, {
        address: stakingAddress,
        abi: STAKED_TOKEN_ABI,
        functionName: 'depositWithPermit',
        args: [amount, address, deadline, v, r, s],
      })

      operation.hash = hash
      operation.status = 'confirming'

      // Store hash immediately in case of navigation
      txStore.updateTransaction(transactionId, { hash })

      // Step 3: Wait for confirmation
      txStore.setStepInProgress(transactionId, 2)

      const publicClient = getPublicClient(config)
      if (!publicClient) {
        throw new Error('No public client')
      }

      await waitForTransactionReceipt(config, { hash })

      // Success
      operation.status = 'success'
      txStore.setStepSuccess(transactionId, hash)
      txStore.clearActiveOperation(operationKey)

    } catch (err) {
      const previousStatus = operation.status
      operation.status = 'error'
      operation.error = err instanceof Error ? err.message : String(err)

      const error = parseTransactionError(err)
      const message = getErrorMessage(error)

      const stepIndex = previousStatus === 'signing' ? 0 : previousStatus === 'submitted' ? 1 : 2
      txStore.setStepError(transactionId, stepIndex, message)
      txStore.clearActiveOperation(operationKey)
    }
  }

  async executeUnstake(
    side: 'BEAR' | 'BULL',
    shares: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const config = this.getConfig()
    const operationKey = `unstake-${side}`

    // Check if there's already a pending operation
    const existing = this.pendingOperations.get(operationKey)
    if (existing && existing.status !== 'success' && existing.status !== 'error') {
      console.warn(`[TxManager] Operation ${operationKey} already in progress`)
      return
    }

    const chainId = config.state.chainId
    if (!chainId) {
      throw new Error('No chain connected')
    }

    const walletClient = await getWalletClient(config)
    if (!walletClient) {
      throw new Error('No wallet connected')
    }

    const address = walletClient.account.address
    const addresses = getAddresses(chainId)
    const stakingAddress = side === 'BEAR' ? addresses.STAKING_BEAR : addresses.STAKING_BULL

    // Create transaction in store
    const transactionId = crypto.randomUUID()
    const txStore = useTransactionStore.getState()
    const txModal = useTransactionModal.getState()

    const steps = [`Unstake splDXY-${side}`, 'Awaiting confirmation']
    txStore.addTransaction({
      id: transactionId,
      type: 'unstake',
      status: 'pending',
      title: `Unstaking splDXY-${side}`,
      steps: steps.map(label => ({ label, status: 'pending' as const })),
      chainId,
    })

    txStore.setActiveOperation(operationKey, transactionId)

    txModal.open({
      transactionId,
      onRetry: options?.onRetry ?? (() => void this.executeUnstake(side, shares, options)),
    })

    const operation: PendingOperation = {
      id: operationKey,
      transactionId,
      status: 'submitted',
    }
    this.pendingOperations.set(operationKey, operation)

    try {
      // Step 1: Send unstake transaction
      txStore.setStepInProgress(transactionId, 0)

      const hash = await writeContract(config, {
        address: stakingAddress,
        abi: STAKED_TOKEN_ABI,
        functionName: 'redeem',
        args: [shares, address, address],
      })

      operation.hash = hash
      operation.status = 'confirming'

      // Store hash immediately
      txStore.updateTransaction(transactionId, { hash })

      // Step 2: Wait for confirmation
      txStore.setStepInProgress(transactionId, 1)

      await waitForTransactionReceipt(config, { hash })

      // Success
      operation.status = 'success'
      txStore.setStepSuccess(transactionId, hash)
      txStore.clearActiveOperation(operationKey)

    } catch (err) {
      const previousStatus = operation.status
      operation.status = 'error'
      operation.error = err instanceof Error ? err.message : String(err)

      const error = parseTransactionError(err)
      const message = getErrorMessage(error)

      const stepIndex = previousStatus === 'submitted' ? 0 : 1
      txStore.setStepError(transactionId, stepIndex, message)
      txStore.clearActiveOperation(operationKey)
    }
  }

  async executeCurveSwap(
    mode: 'buy' | 'sell',
    amount: bigint,
    minAmountOut: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const config = this.getConfig()
    const operationKey = `swap-${mode}-bear`

    const existing = this.pendingOperations.get(operationKey)
    if (existing && existing.status !== 'success' && existing.status !== 'error') {
      console.warn(`[TxManager] Operation ${operationKey} already in progress`)
      return
    }

    const chainId = config.state.chainId
    if (!chainId) throw new Error('No chain connected')

    const walletClient = await getWalletClient(config)
    if (!walletClient) throw new Error('No wallet connected')

    const address = walletClient.account.address
    const addresses = getAddresses(chainId)
    const tokenAddress = mode === 'buy' ? addresses.USDC : addresses.DXY_BEAR
    const tokenSymbol = mode === 'buy' ? 'USDC' : 'plDXY-BEAR'

    const hasAllowance = await this.checkAllowance(tokenAddress, addresses.CURVE_POOL, address, amount)

    const transactionId = crypto.randomUUID()
    const txStore = useTransactionStore.getState()
    const txModal = useTransactionModal.getState()

    const steps = hasAllowance
      ? [mode === 'buy' ? 'Buy plDXY-BEAR' : 'Sell plDXY-BEAR', 'Awaiting confirmation']
      : [`Approve ${tokenSymbol}`, 'Confirming approval', mode === 'buy' ? 'Buy plDXY-BEAR' : 'Sell plDXY-BEAR', 'Awaiting confirmation']

    txStore.addTransaction({
      id: transactionId,
      type: 'swap',
      status: 'pending',
      title: mode === 'buy' ? 'Buying plDXY-BEAR' : 'Selling plDXY-BEAR',
      steps: steps.map(label => ({ label, status: 'pending' as const })),
      chainId,
    })

    txStore.setActiveOperation(operationKey, transactionId)
    txModal.open({
      transactionId,
      onRetry: options?.onRetry ?? (() => void this.executeCurveSwap(mode, amount, minAmountOut, options)),
    })

    const operation: PendingOperation = {
      id: operationKey,
      transactionId,
      status: hasAllowance ? 'submitted' : 'approving',
    }
    this.pendingOperations.set(operationKey, operation)

    const USDC_INDEX = 0n
    const BEAR_INDEX = 1n
    const i = mode === 'buy' ? USDC_INDEX : BEAR_INDEX
    const j = mode === 'buy' ? BEAR_INDEX : USDC_INDEX

    try {
      let stepOffset = 0

      if (!hasAllowance) {
        txStore.setStepInProgress(transactionId, 0)
        await this.executeApproval(tokenAddress, addresses.CURVE_POOL, amount)
        txStore.setStepInProgress(transactionId, 1)
        stepOffset = 2
      }

      operation.status = 'submitted'
      txStore.setStepInProgress(transactionId, stepOffset)

      const hash = await writeContract(config, {
        address: addresses.CURVE_POOL,
        abi: CURVE_POOL_ABI,
        functionName: 'exchange',
        args: [i, j, amount, minAmountOut, address],
      })

      operation.hash = hash
      operation.status = 'confirming'
      txStore.updateTransaction(transactionId, { hash })
      txStore.setStepInProgress(transactionId, stepOffset + 1)

      await waitForTransactionReceipt(config, { hash })

      operation.status = 'success'
      txStore.setStepSuccess(transactionId, hash)
      txStore.clearActiveOperation(operationKey)

    } catch (err) {
      const previousStatus = operation.status
      operation.status = 'error'
      operation.error = err instanceof Error ? err.message : String(err)

      const error = parseTransactionError(err)
      const message = getErrorMessage(error)

      const stepIndex = previousStatus === 'approving' ? 0 : (hasAllowance ? 0 : 2)
      txStore.setStepError(transactionId, stepIndex, message)
      txStore.clearActiveOperation(operationKey)
    }
  }

  async executeZapBuy(
    usdcAmount: bigint,
    minBullOut: bigint,
    slippageBps: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const config = this.getConfig()
    const operationKey = 'swap-buy-bull'

    const existing = this.pendingOperations.get(operationKey)
    if (existing && existing.status !== 'success' && existing.status !== 'error') {
      console.warn(`[TxManager] Operation ${operationKey} already in progress`)
      return
    }

    const chainId = config.state.chainId
    if (!chainId) throw new Error('No chain connected')

    const walletClient = await getWalletClient(config)
    if (!walletClient) throw new Error('No wallet connected')

    const address = walletClient.account.address
    const addresses = getAddresses(chainId)

    const hasAllowance = await this.checkAllowance(addresses.USDC, addresses.ZAP_ROUTER, address, usdcAmount)

    const transactionId = crypto.randomUUID()
    const txStore = useTransactionStore.getState()
    const txModal = useTransactionModal.getState()

    const steps = hasAllowance
      ? ['Buy plDXY-BULL', 'Awaiting confirmation']
      : ['Approve USDC', 'Confirming approval', 'Buy plDXY-BULL', 'Awaiting confirmation']

    txStore.addTransaction({
      id: transactionId,
      type: 'swap',
      status: 'pending',
      title: 'Buying plDXY-BULL',
      steps: steps.map(label => ({ label, status: 'pending' as const })),
      chainId,
    })

    txStore.setActiveOperation(operationKey, transactionId)
    txModal.open({
      transactionId,
      onRetry: options?.onRetry ?? (() => void this.executeZapBuy(usdcAmount, minBullOut, slippageBps, options)),
    })

    const operation: PendingOperation = {
      id: operationKey,
      transactionId,
      status: hasAllowance ? 'submitted' : 'approving',
    }
    this.pendingOperations.set(operationKey, operation)

    const deadline = BigInt(Math.floor(Date.now() / 1000) + 1800)

    try {
      let stepOffset = 0

      if (!hasAllowance) {
        txStore.setStepInProgress(transactionId, 0)
        await this.executeApproval(addresses.USDC, addresses.ZAP_ROUTER, usdcAmount)
        txStore.setStepInProgress(transactionId, 1)
        stepOffset = 2
      }

      operation.status = 'submitted'
      txStore.setStepInProgress(transactionId, stepOffset)

      const hash = await writeContract(config, {
        address: addresses.ZAP_ROUTER,
        abi: ZAP_ROUTER_ABI,
        functionName: 'zapMint',
        args: [usdcAmount, minBullOut, slippageBps, deadline],
      })

      operation.hash = hash
      operation.status = 'confirming'
      txStore.updateTransaction(transactionId, { hash })
      txStore.setStepInProgress(transactionId, stepOffset + 1)

      await waitForTransactionReceipt(config, { hash })

      operation.status = 'success'
      txStore.setStepSuccess(transactionId, hash)
      txStore.clearActiveOperation(operationKey)

    } catch (err) {
      const previousStatus = operation.status
      operation.status = 'error'
      operation.error = err instanceof Error ? err.message : String(err)

      const error = parseTransactionError(err)
      const message = getErrorMessage(error)

      const stepIndex = previousStatus === 'approving' ? 0 : (hasAllowance ? 0 : 2)
      txStore.setStepError(transactionId, stepIndex, message)
      txStore.clearActiveOperation(operationKey)
    }
  }

  async executeZapSell(
    bullAmount: bigint,
    minUsdcOut: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const config = this.getConfig()
    const operationKey = 'swap-sell-bull'

    const existing = this.pendingOperations.get(operationKey)
    if (existing && existing.status !== 'success' && existing.status !== 'error') {
      console.warn(`[TxManager] Operation ${operationKey} already in progress`)
      return
    }

    const chainId = config.state.chainId
    if (!chainId) throw new Error('No chain connected')

    const walletClient = await getWalletClient(config)
    if (!walletClient) throw new Error('No wallet connected')

    const address = walletClient.account.address
    const addresses = getAddresses(chainId)

    const hasAllowance = await this.checkAllowance(addresses.DXY_BULL, addresses.ZAP_ROUTER, address, bullAmount)

    const transactionId = crypto.randomUUID()
    const txStore = useTransactionStore.getState()
    const txModal = useTransactionModal.getState()

    const steps = hasAllowance
      ? ['Sell plDXY-BULL', 'Awaiting confirmation']
      : ['Approve plDXY-BULL', 'Confirming approval', 'Sell plDXY-BULL', 'Awaiting confirmation']

    txStore.addTransaction({
      id: transactionId,
      type: 'swap',
      status: 'pending',
      title: 'Selling plDXY-BULL',
      steps: steps.map(label => ({ label, status: 'pending' as const })),
      chainId,
    })

    txStore.setActiveOperation(operationKey, transactionId)
    txModal.open({
      transactionId,
      onRetry: options?.onRetry ?? (() => void this.executeZapSell(bullAmount, minUsdcOut, options)),
    })

    const operation: PendingOperation = {
      id: operationKey,
      transactionId,
      status: hasAllowance ? 'submitted' : 'approving',
    }
    this.pendingOperations.set(operationKey, operation)

    const deadline = BigInt(Math.floor(Date.now() / 1000) + 1800)

    try {
      let stepOffset = 0

      if (!hasAllowance) {
        txStore.setStepInProgress(transactionId, 0)
        await this.executeApproval(addresses.DXY_BULL, addresses.ZAP_ROUTER, bullAmount)
        txStore.setStepInProgress(transactionId, 1)
        stepOffset = 2
      }

      operation.status = 'submitted'
      txStore.setStepInProgress(transactionId, stepOffset)

      const hash = await writeContract(config, {
        address: addresses.ZAP_ROUTER,
        abi: ZAP_ROUTER_ABI,
        functionName: 'zapBurn',
        args: [bullAmount, minUsdcOut, deadline],
      })

      operation.hash = hash
      operation.status = 'confirming'
      txStore.updateTransaction(transactionId, { hash })
      txStore.setStepInProgress(transactionId, stepOffset + 1)

      await waitForTransactionReceipt(config, { hash })

      operation.status = 'success'
      txStore.setStepSuccess(transactionId, hash)
      txStore.clearActiveOperation(operationKey)

    } catch (err) {
      const previousStatus = operation.status
      operation.status = 'error'
      operation.error = err instanceof Error ? err.message : String(err)

      const error = parseTransactionError(err)
      const message = getErrorMessage(error)

      const stepIndex = previousStatus === 'approving' ? 0 : (hasAllowance ? 0 : 2)
      txStore.setStepError(transactionId, stepIndex, message)
      txStore.clearActiveOperation(operationKey)
    }
  }

  async executeMint(
    pairAmount: bigint,
    usdcRequired: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const config = this.getConfig()
    const operationKey = 'mint'

    const existing = this.pendingOperations.get(operationKey)
    if (existing && existing.status !== 'success' && existing.status !== 'error') {
      console.warn(`[TxManager] Operation ${operationKey} already in progress`)
      return
    }

    const chainId = config.state.chainId
    if (!chainId) throw new Error('No chain connected')

    const walletClient = await getWalletClient(config)
    if (!walletClient) throw new Error('No wallet connected')

    const address = walletClient.account.address
    const addresses = getAddresses(chainId)

    const hasUsdcAllowance = await this.checkAllowance(addresses.USDC, addresses.SYNTHETIC_SPLITTER, address, usdcRequired)

    const transactionId = crypto.randomUUID()
    const txStore = useTransactionStore.getState()
    const txModal = useTransactionModal.getState()

    const steps = hasUsdcAllowance
      ? ['Mint pairs', 'Confirming...']
      : ['Approve USDC', 'Confirming...', 'Mint pairs', 'Confirming...']

    txStore.addTransaction({
      id: transactionId,
      type: 'mint',
      status: 'pending',
      title: 'Minting token pairs',
      steps: steps.map(label => ({ label, status: 'pending' as const })),
      chainId,
    })

    txStore.setActiveOperation(operationKey, transactionId)
    txModal.open({
      transactionId,
      onRetry: options?.onRetry ?? (() => void this.executeMint(pairAmount, usdcRequired, options)),
    })

    const operation: PendingOperation = {
      id: operationKey,
      transactionId,
      status: hasUsdcAllowance ? 'submitted' : 'approving',
    }
    this.pendingOperations.set(operationKey, operation)

    try {
      let stepOffset = 0

      if (!hasUsdcAllowance) {
        txStore.setStepInProgress(transactionId, 0)
        await this.executeApproval(addresses.USDC, addresses.SYNTHETIC_SPLITTER, usdcRequired)
        txStore.setStepInProgress(transactionId, 1)
        stepOffset = 2
      }

      operation.status = 'submitted'
      txStore.setStepInProgress(transactionId, stepOffset)

      const hash = await writeContract(config, {
        address: addresses.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'mint',
        args: [pairAmount],
      })

      operation.hash = hash
      operation.status = 'confirming'
      txStore.updateTransaction(transactionId, { hash })
      txStore.setStepInProgress(transactionId, stepOffset + 1)

      await waitForTransactionReceipt(config, { hash })

      operation.status = 'success'
      txStore.setStepSuccess(transactionId, hash)
      txStore.clearActiveOperation(operationKey)

    } catch (err) {
      const previousStatus = operation.status
      operation.status = 'error'
      operation.error = err instanceof Error ? err.message : String(err)

      const error = parseTransactionError(err)
      const message = getErrorMessage(error)

      const stepIndex = previousStatus === 'approving' ? 0 : (hasUsdcAllowance ? 0 : 2)
      txStore.setStepError(transactionId, stepIndex, message)
      txStore.clearActiveOperation(operationKey)
    }
  }

  async executeRedeem(
    pairAmount: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const config = this.getConfig()
    const operationKey = 'redeem'

    const existing = this.pendingOperations.get(operationKey)
    if (existing && existing.status !== 'success' && existing.status !== 'error') {
      console.warn(`[TxManager] Operation ${operationKey} already in progress`)
      return
    }

    const chainId = config.state.chainId
    if (!chainId) throw new Error('No chain connected')

    const walletClient = await getWalletClient(config)
    if (!walletClient) throw new Error('No wallet connected')

    const address = walletClient.account.address
    const addresses = getAddresses(chainId)

    const hasBearAllowance = await this.checkAllowance(addresses.DXY_BEAR, addresses.SYNTHETIC_SPLITTER, address, pairAmount)
    const hasBullAllowance = await this.checkAllowance(addresses.DXY_BULL, addresses.SYNTHETIC_SPLITTER, address, pairAmount)

    const transactionId = crypto.randomUUID()
    const txStore = useTransactionStore.getState()
    const txModal = useTransactionModal.getState()

    const steps: string[] = []
    if (!hasBearAllowance) steps.push('Approve plDXY-BEAR', 'Confirming...')
    if (!hasBullAllowance) steps.push('Approve plDXY-BULL', 'Confirming...')
    steps.push('Redeem pairs', 'Confirming...')

    txStore.addTransaction({
      id: transactionId,
      type: 'burn',
      status: 'pending',
      title: 'Redeeming token pairs',
      steps: steps.map(label => ({ label, status: 'pending' as const })),
      chainId,
    })

    txStore.setActiveOperation(operationKey, transactionId)
    txModal.open({
      transactionId,
      onRetry: options?.onRetry ?? (() => void this.executeRedeem(pairAmount, options)),
    })

    const operation: PendingOperation = {
      id: operationKey,
      transactionId,
      status: (hasBearAllowance && hasBullAllowance) ? 'submitted' : 'approving',
    }
    this.pendingOperations.set(operationKey, operation)

    try {
      let stepIndex = 0

      if (!hasBearAllowance) {
        txStore.setStepInProgress(transactionId, stepIndex)
        await this.executeApproval(addresses.DXY_BEAR, addresses.SYNTHETIC_SPLITTER, pairAmount)
        txStore.setStepInProgress(transactionId, stepIndex + 1)
        stepIndex += 2
      }

      if (!hasBullAllowance) {
        txStore.setStepInProgress(transactionId, stepIndex)
        await this.executeApproval(addresses.DXY_BULL, addresses.SYNTHETIC_SPLITTER, pairAmount)
        txStore.setStepInProgress(transactionId, stepIndex + 1)
        stepIndex += 2
      }

      operation.status = 'submitted'
      txStore.setStepInProgress(transactionId, stepIndex)

      const hash = await writeContract(config, {
        address: addresses.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'burn',
        args: [pairAmount],
      })

      operation.hash = hash
      operation.status = 'confirming'
      txStore.updateTransaction(transactionId, { hash })
      txStore.setStepInProgress(transactionId, stepIndex + 1)

      await waitForTransactionReceipt(config, { hash })

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

  async executeOpenLeverage(
    side: 'BEAR' | 'BULL',
    principal: bigint,
    leverage: bigint,
    slippageBps: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const config = this.getConfig()
    const operationKey = `leverage-open-${side}`

    const existing = this.pendingOperations.get(operationKey)
    if (existing && existing.status !== 'success' && existing.status !== 'error') {
      console.warn(`[TxManager] Operation ${operationKey} already in progress`)
      return
    }

    const chainId = config.state.chainId
    if (!chainId) throw new Error('No chain connected')

    const walletClient = await getWalletClient(config)
    if (!walletClient) throw new Error('No wallet connected')

    const address = walletClient.account.address
    const addresses = getAddresses(chainId)
    const routerAddress = side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER

    const morphoAddress = await readContract(config, {
      address: routerAddress,
      abi: LEVERAGE_ROUTER_ABI,
      functionName: 'MORPHO',
    }) as `0x${string}`

    const isAuthorized = await readContract(config, {
      address: morphoAddress,
      abi: MORPHO_ABI,
      functionName: 'isAuthorized',
      args: [address, routerAddress],
    }) as boolean

    const hasUsdcAllowance = await this.checkAllowance(addresses.USDC, routerAddress, address, principal)

    const transactionId = crypto.randomUUID()
    const txStore = useTransactionStore.getState()
    const txModal = useTransactionModal.getState()

    const steps: string[] = []
    if (!isAuthorized) steps.push('Authorize Morpho', 'Confirming...')
    if (!hasUsdcAllowance) steps.push('Approve USDC', 'Confirming...')
    steps.push(`Open ${side} position`, 'Confirming...')

    txStore.addTransaction({
      id: transactionId,
      type: 'leverage',
      status: 'pending',
      title: `Opening ${side} leverage position`,
      steps: steps.map(label => ({ label, status: 'pending' as const })),
      chainId,
    })

    txStore.setActiveOperation(operationKey, transactionId)
    txModal.open({
      transactionId,
      onRetry: options?.onRetry ?? (() => void this.executeOpenLeverage(side, principal, leverage, slippageBps, options)),
    })

    const operation: PendingOperation = {
      id: operationKey,
      transactionId,
      status: (isAuthorized && hasUsdcAllowance) ? 'submitted' : 'approving',
    }
    this.pendingOperations.set(operationKey, operation)

    const deadline = BigInt(Math.floor(Date.now() / 1000) + 1800)

    try {
      let stepIndex = 0

      if (!isAuthorized) {
        txStore.setStepInProgress(transactionId, stepIndex)
        const authHash = await writeContract(config, {
          address: morphoAddress,
          abi: MORPHO_ABI,
          functionName: 'setAuthorization',
          args: [routerAddress, true],
        })
        await waitForTransactionReceipt(config, { hash: authHash })
        txStore.setStepInProgress(transactionId, stepIndex + 1)
        stepIndex += 2
      }

      if (!hasUsdcAllowance) {
        txStore.setStepInProgress(transactionId, stepIndex)
        await this.executeApproval(addresses.USDC, routerAddress, principal)
        txStore.setStepInProgress(transactionId, stepIndex + 1)
        stepIndex += 2
      }

      operation.status = 'submitted'
      txStore.setStepInProgress(transactionId, stepIndex)

      const hash = await writeContract(config, {
        address: routerAddress,
        abi: LEVERAGE_ROUTER_ABI,
        functionName: 'openLeverage',
        args: [principal, leverage, slippageBps, deadline],
      })

      operation.hash = hash
      operation.status = 'confirming'
      txStore.updateTransaction(transactionId, { hash })
      txStore.setStepInProgress(transactionId, stepIndex + 1)

      await waitForTransactionReceipt(config, { hash })

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

  async executeCloseLeverage(
    side: 'BEAR' | 'BULL',
    collateralToWithdraw: bigint,
    slippageBps: bigint,
    options?: { onRetry?: () => void }
  ): Promise<void> {
    const config = this.getConfig()
    const operationKey = `leverage-close-${side}`

    const existing = this.pendingOperations.get(operationKey)
    if (existing && existing.status !== 'success' && existing.status !== 'error') {
      console.warn(`[TxManager] Operation ${operationKey} already in progress`)
      return
    }

    const chainId = config.state.chainId
    if (!chainId) throw new Error('No chain connected')

    const walletClient = await getWalletClient(config)
    if (!walletClient) throw new Error('No wallet connected')

    const addresses = getAddresses(chainId)
    const routerAddress = side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER

    const transactionId = crypto.randomUUID()
    const txStore = useTransactionStore.getState()
    const txModal = useTransactionModal.getState()

    const steps = [`Close ${side} position`, 'Confirming...']

    txStore.addTransaction({
      id: transactionId,
      type: 'leverage',
      status: 'pending',
      title: `Closing ${side} leverage position`,
      steps: steps.map(label => ({ label, status: 'pending' as const })),
      chainId,
    })

    txStore.setActiveOperation(operationKey, transactionId)
    txModal.open({
      transactionId,
      onRetry: options?.onRetry ?? (() => void this.executeCloseLeverage(side, collateralToWithdraw, slippageBps, options)),
    })

    const operation: PendingOperation = {
      id: operationKey,
      transactionId,
      status: 'submitted',
    }
    this.pendingOperations.set(operationKey, operation)

    const deadline = BigInt(Math.floor(Date.now() / 1000) + 1800)

    try {
      txStore.setStepInProgress(transactionId, 0)

      const hash = await writeContract(config, {
        address: routerAddress,
        abi: LEVERAGE_ROUTER_ABI,
        functionName: 'closeLeverage',
        args: [collateralToWithdraw, slippageBps, deadline],
      })

      operation.hash = hash
      operation.status = 'confirming'
      txStore.updateTransaction(transactionId, { hash })
      txStore.setStepInProgress(transactionId, 1)

      await waitForTransactionReceipt(config, { hash })

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

  getOperation(operationKey: string): PendingOperation | undefined {
    return this.pendingOperations.get(operationKey)
  }

  isOperationPending(operationKey: string): boolean {
    const op = this.getOperation(operationKey)
    return !!op && op.status !== 'success' && op.status !== 'error'
  }
}

export const transactionManager = new TransactionManager()

// Expose for debugging
if (typeof window !== 'undefined') {
  (window as unknown as { __txManager: TransactionManager }).__txManager = transactionManager
}
