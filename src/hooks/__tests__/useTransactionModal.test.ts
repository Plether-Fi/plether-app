import { describe, it, expect, beforeEach } from 'vitest'
import { useTransactionModal } from '../useTransactionModal'

describe('useTransactionModal', () => {
  beforeEach(() => {
    useTransactionModal.getState().reset()
  })

  describe('mint flow with USDC approval', () => {
    const modalSteps = ['Approve USDC', 'Confirming...', 'Mint pairs', 'Confirming...']

    it('opens modal with all steps pending', () => {
      const { open } = useTransactionModal.getState()
      open({ title: 'Minting', steps: modalSteps })

      const { steps } = useTransactionModal.getState()
      expect(steps).toHaveLength(4)
      expect(steps.every(s => s.status === 'pending')).toBe(true)
    })

    it('setStepInProgress(0) marks step 0 as in_progress', () => {
      const { open, setStepInProgress } = useTransactionModal.getState()
      open({ title: 'Minting', steps: modalSteps })
      setStepInProgress(0)

      const { steps } = useTransactionModal.getState()
      expect(steps[0].status).toBe('in_progress')
      expect(steps[1].status).toBe('pending')
      expect(steps[2].status).toBe('pending')
      expect(steps[3].status).toBe('pending')
    })

    it('setStepInProgress(1) marks step 0 completed, step 1 in_progress', () => {
      const { open, setStepInProgress } = useTransactionModal.getState()
      open({ title: 'Minting', steps: modalSteps })
      setStepInProgress(0)
      setStepInProgress(1)

      const { steps } = useTransactionModal.getState()
      expect(steps[0].status).toBe('completed')
      expect(steps[1].status).toBe('in_progress')
      expect(steps[2].status).toBe('pending')
      expect(steps[3].status).toBe('pending')
    })

    it('setStepInProgress(2) marks steps 0,1 completed, step 2 in_progress', () => {
      const { open, setStepInProgress } = useTransactionModal.getState()
      open({ title: 'Minting', steps: modalSteps })
      setStepInProgress(0)
      setStepInProgress(1)
      setStepInProgress(2)

      const { steps } = useTransactionModal.getState()
      expect(steps[0].status).toBe('completed')
      expect(steps[1].status).toBe('completed')
      expect(steps[2].status).toBe('in_progress')
      expect(steps[3].status).toBe('pending')
    })

    it('setError(2) marks step 2 as error with previous steps completed', () => {
      const { open, setStepInProgress, setError } = useTransactionModal.getState()
      open({ title: 'Minting', steps: modalSteps })

      // Simulate the flow: approve USDC -> confirming -> mint step starts -> error
      setStepInProgress(0)
      setStepInProgress(1)
      setStepInProgress(2)
      setError(2, 'User rejected transaction')

      const { steps, errorMessage } = useTransactionModal.getState()
      expect(steps[0].status).toBe('completed')
      expect(steps[1].status).toBe('completed')
      expect(steps[2].status).toBe('error')
      expect(steps[3].status).toBe('pending')
      expect(errorMessage).toBe('User rejected transaction')
    })
  })

  describe('error display location', () => {
    it('error on step 2 should NOT affect step 0 status', () => {
      const { open, setStepInProgress, setError } = useTransactionModal.getState()
      open({ title: 'Test', steps: ['Step 0', 'Step 1', 'Step 2', 'Step 3'] })

      setStepInProgress(0)
      setStepInProgress(1)
      setStepInProgress(2)

      // Step 0 should be completed before error
      expect(useTransactionModal.getState().steps[0].status).toBe('completed')

      setError(2, 'Error message')

      // Step 0 should still be completed after error
      const { steps } = useTransactionModal.getState()
      expect(steps[0].status).toBe('completed')
      expect(steps[2].status).toBe('error')
    })
  })

  describe('retry scenario - steps rebuilt without approval', () => {
    it('on retry with fewer steps, error index should match NEW step list', () => {
      const { open, setStepInProgress, setError, reset } = useTransactionModal.getState()

      // First attempt: 4 steps (with approval)
      open({ title: 'Minting', steps: ['Approve USDC', 'Confirming...', 'Mint pairs', 'Confirming...'] })
      setStepInProgress(0)
      setStepInProgress(1)
      setStepInProgress(2)
      // User rejects mint, but let's say they close and retry

      // Reset and retry - now only 2 steps (no approval needed)
      reset()
      open({ title: 'Minting', steps: ['Mint pairs', 'Confirming...'] })

      const { steps: newSteps } = useTransactionModal.getState()
      expect(newSteps).toHaveLength(2)
      expect(newSteps[0].label).toBe('Mint pairs')

      // Now if mint fails at i=0, modalStepBase=0
      setStepInProgress(0)
      setError(0, 'User rejected')

      const { steps, errorMessage } = useTransactionModal.getState()
      expect(steps[0].status).toBe('error')
      expect(steps[0].label).toBe('Mint pairs')
      expect(errorMessage).toBe('User rejected')
    })
  })

  describe('BUG: onRetry uses stale buildSteps but modal shows OLD steps', () => {
    it('FAILS if retry does not rebuild modal steps when buildSteps returns fewer items', () => {
      // This test documents the bug:
      // 1. First execution opens modal with 4 steps
      // 2. Approval succeeds, allowance updates
      // 3. buildSteps now returns 1 step (no approval needed)
      // 4. On retry, onRetry callback has OLD config that rebuilds steps
      // 5. BUT if buildSteps() reads current allowance, it returns 1 step
      // 6. Modal shows 2 steps but execution uses index 0 for mint

      const { open, setStepInProgress, setError } = useTransactionModal.getState()

      // Simulate: onRetry opens modal with NEW (fewer) steps
      // but the OLD modal steps from first attempt are somehow preserved
      // This shouldn't happen, but let's verify

      // If modal has 4 steps but execution thinks there's only 1 logical step:
      open({ title: 'Minting', steps: ['Approve USDC', 'Confirming...', 'Mint pairs', 'Confirming...'] })

      // Execution runs with steps.length = 1 (only mint)
      // Loop i=0, modalStepBase = 0
      setStepInProgress(0)  // This marks step 0 (Approve USDC) as in_progress

      // User rejects mint (which is actually at modal index 2 if 4 steps, but index 0 if 2 steps)
      setError(0, 'User rejected')

      // BUG: Error shows under "Approve USDC" when it should show under "Mint pairs"
      const { steps, errorMessage } = useTransactionModal.getState()
      expect(steps[0].label).toBe('Approve USDC')
      expect(steps[0].status).toBe('error')  // This is the bug!
      expect(errorMessage).toBe('User rejected')
    })
  })
})
