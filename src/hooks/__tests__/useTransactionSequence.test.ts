import { describe, it, expect, beforeEach } from 'vitest'
import { useTransactionModal } from '../useTransactionModal'

/**
 * These tests simulate the useTransactionSequence behavior by directly
 * calling modal store methods with the same indices the sequence hook would use.
 */
describe('useTransactionSequence simulation', () => {
  beforeEach(() => {
    useTransactionModal.getState().reset()
  })

  describe('retry with stale buildSteps closure', () => {
    /**
     * BUG SCENARIO:
     * 1. First execution: buildSteps returns 2 logical steps (approve + mint)
     *    - Modal has 4 steps: [Approve, Confirming, Mint, Confirming]
     * 2. Approve step succeeds, refetch() updates allowance
     * 3. needsUsdcApproval becomes false
     * 4. Mint step fails, user clicks retry
     * 5. onRetry calls execute(config) where config.buildSteps is the OLD function
     * 6. BUT buildSteps captures needsUsdcApproval which is now FALSE
     * 7. buildSteps() returns 1 logical step (just mint)
     * 8. Modal is opened with 2 steps: [Mint, Confirming]
     * 9. When mint fails at i=0, modalStepBase=0, error shows under "Mint"
     *
     * This SHOULD work correctly because:
     * - buildSteps is a useCallback that captures needsUsdcApproval
     * - The OLD buildSteps function has the OLD value captured
     * - So on retry, it should still return 2 steps
     *
     * UNLESS buildSteps doesn't capture the value but reads it from component scope
     */
    it('simulates retry where buildSteps captures value correctly', () => {
      const { open, setStepInProgress, setError } = useTransactionModal.getState()

      // Simulate useCallback capturing needsUsdcApproval = true
      const capturedNeedsApproval = true
      const buildSteps = () => {
        const steps = []
        if (capturedNeedsApproval) {
          steps.push({ label: 'Approve USDC', action: async () => '0x1' as `0x${string}` })
        }
        steps.push({ label: 'Mint pairs', action: async () => '0x2' as `0x${string}` })
        return steps
      }

      // First execution
      const steps = buildSteps()
      const modalSteps = steps.flatMap(s => [s.label, 'Confirming...'])
      open({ title: 'Minting', steps: modalSteps })

      expect(useTransactionModal.getState().steps).toHaveLength(4)

      // Simulate approve succeeding then mint failing
      setStepInProgress(0) // Approve USDC
      setStepInProgress(1) // Confirming
      setStepInProgress(2) // Mint pairs
      setError(2, 'User rejected')

      expect(useTransactionModal.getState().steps[2].status).toBe('error')
      expect(useTransactionModal.getState().steps[2].label).toBe('Mint pairs')

      // Now simulate retry where capturedNeedsApproval is still true (correct behavior)
      useTransactionModal.getState().reset()

      // buildSteps still has capturedNeedsApproval = true
      const retrySteps = buildSteps()
      const retryModalSteps = retrySteps.flatMap(s => [s.label, 'Confirming...'])
      open({ title: 'Minting', steps: retryModalSteps })

      expect(useTransactionModal.getState().steps).toHaveLength(4)
      expect(useTransactionModal.getState().steps[0].label).toBe('Approve USDC')
    })

    it('REPRODUCES BUG: buildSteps reads current value instead of captured value', () => {
      const { open, setStepInProgress, setError } = useTransactionModal.getState()

      // Simulate buildSteps that reads current value (not captured)
      let currentNeedsApproval = true
      const buildSteps = () => {
        const steps = []
        if (currentNeedsApproval) { // Reads current, not captured!
          steps.push({ label: 'Approve USDC', action: async () => '0x1' as `0x${string}` })
        }
        steps.push({ label: 'Mint pairs', action: async () => '0x2' as `0x${string}` })
        return steps
      }

      // First execution
      const steps = buildSteps()
      const modalSteps = steps.flatMap(s => [s.label, 'Confirming...'])
      open({ title: 'Minting', steps: modalSteps })

      expect(useTransactionModal.getState().steps).toHaveLength(4)

      // Approve succeeds, update allowance
      setStepInProgress(0)
      setStepInProgress(1)

      // Simulate refetch() updating allowance
      currentNeedsApproval = false

      // Continue with mint step
      setStepInProgress(2)
      setError(2, 'User rejected')

      expect(useTransactionModal.getState().steps[2].status).toBe('error')

      // Retry - buildSteps now returns different results!
      useTransactionModal.getState().reset()

      const retrySteps = buildSteps() // Now returns only mint!
      expect(retrySteps).toHaveLength(1)
      expect(retrySteps[0].label).toBe('Mint pairs')

      const retryModalSteps = retrySteps.flatMap(s => [s.label, 'Confirming...'])
      open({ title: 'Minting', steps: retryModalSteps })

      // Modal now has only 2 steps
      expect(useTransactionModal.getState().steps).toHaveLength(2)
      expect(useTransactionModal.getState().steps[0].label).toBe('Mint pairs')

      // When mint fails at i=0, modalStepBase=0
      setStepInProgress(0)
      setError(0, 'User rejected again')

      // Error correctly shows under "Mint pairs" (which is now step 0)
      const { steps: finalSteps } = useTransactionModal.getState()
      expect(finalSteps[0].status).toBe('error')
      expect(finalSteps[0].label).toBe('Mint pairs')
    })
  })

  describe('first attempt bug - not retry related', () => {
    /**
     * Alternative theory: The bug happens on FIRST attempt, not retry.
     * User says: "after approving usdc, when approving transaction step begins"
     *
     * Maybe the issue is that execute() is called twice somehow?
     * Or buildSteps() is called with wrong state?
     */
    it('simulates double execute call during first attempt', () => {
      const { open, setStepInProgress } = useTransactionModal.getState()

      // First execute call - 4 steps
      open({ title: 'Minting', steps: ['Approve USDC', 'Confirming...', 'Mint pairs', 'Confirming...'] })
      setStepInProgress(0) // i=0, modalStepBase=0
      setStepInProgress(1) // After approve hash returned

      // Simulate: component re-renders during refetch, somehow triggers second execute
      // Second execute with only 2 steps (approval not needed per current state)
      open({ title: 'Minting', steps: ['Mint pairs', 'Confirming...'] })

      // Now modal has 2 steps but first execution continues thinking there are 4
      // setStepInProgress(2) would be out of bounds for new 2-step modal!
      const { steps } = useTransactionModal.getState()
      expect(steps).toHaveLength(2)

      // First execution calls setStepInProgress(2) for "Mint pairs"
      // But modal only has 2 steps (indices 0, 1)
      // This would set step 2 which doesn't exist...
      setStepInProgress(2) // What happens here?

      // Actually setStepInProgress(2) would just mark steps 0,1 as completed
      // and try to mark step 2 as in_progress, but step 2 doesn't exist
      expect(useTransactionModal.getState().steps[0].status).toBe('completed')
      expect(useTransactionModal.getState().steps[1].status).toBe('completed')
      // Step 2 doesn't exist, so no in_progress step
    })
  })

  describe('setError with out-of-bounds index', () => {
    it('FIXED: setError with out-of-bounds index clamps to last step', () => {
      const { open, setError } = useTransactionModal.getState()

      // Modal has 2 steps
      open({ title: 'Test', steps: ['Mint pairs', 'Confirming...'] })

      // setError is called with index 2 (out of bounds)
      setError(2, 'Error message')

      const { steps, errorMessage } = useTransactionModal.getState()
      expect(errorMessage).toBe('Error message')

      // FIX: Last step is marked as error (clamped index)
      expect(steps[0].status).toBe('completed')
      expect(steps[1].status).toBe('error')
    })
  })

  describe('EXACT BUG REPRODUCTION: modal has 4 steps but execution uses 1-step indices', () => {
    /**
     * User's exact bug scenario:
     * 1. Modal opens with 4 steps (USDC approval + mint)
     * 2. USDC approval succeeds
     * 3. During execution, something causes buildSteps to be called again
     * 4. buildSteps now returns only 1 step (mint)
     * 5. Execution loop uses i=0 for mint (because steps.length=1)
     * 6. modalStepBase = 0 * 2 = 0
     * 7. setStepInProgress(0) marks "Approve USDC" as in_progress (BUG 1: checkbox spins)
     * 8. setError(0) marks "Approve USDC" as error (BUG 2: wrong step)
     */
    it('FIXED: 4-step modal but 1-step execution - error shows on in_progress step', () => {
      const { open, setStepInProgress, setError } = useTransactionModal.getState()

      // Modal opened with 4 steps (first execution, USDC approval needed)
      open({ title: 'Minting', steps: ['Approve USDC', 'Confirming...', 'Mint pairs', 'Confirming...'] })

      // First execution progresses through approve step
      setStepInProgress(0) // Approve USDC in_progress
      setStepInProgress(1) // Confirming in_progress, Approve completed
      // At this point, approve succeeded and refetch() updates allowance

      // Verify state before the bug
      expect(useTransactionModal.getState().steps[0].status).toBe('completed')
      expect(useTransactionModal.getState().steps[1].status).toBe('in_progress')

      // NOW: Something triggers a second execution with fewer steps
      // This does NOT call open() again - the modal stays with 4 steps
      // But the execution loop uses indices for 1-step buildSteps result

      // Simulating: execution continues but with steps.length = 1
      // Loop i=0 (only iteration), modalStepBase = 0
      setStepInProgress(0) // FIX 1: completed steps don't go back to in_progress

      // Check: "Approve USDC" stays completed (not spinning)
      const { steps: stepsAfterFix1 } = useTransactionModal.getState()
      expect(stepsAfterFix1[0].status).toBe('completed') // FIX 1 works!
      // Note: step 1 is still in_progress from before

      // setError called with stale index 0
      setError(0, 'User rejected')

      const { steps: finalSteps } = useTransactionModal.getState()
      // FIX 2: Error shows on the in_progress step (step 1), not the stale index
      expect(finalSteps[0].status).toBe('completed') // Approve USDC stays completed
      expect(finalSteps[1].status).toBe('error') // Confirming gets the error (was in_progress)
      expect(finalSteps[1].label).toBe('Confirming...')
    })

    it('setError falls back to clamped index when no step is in_progress', () => {
      const { open, setError } = useTransactionModal.getState()

      // Modal with 2 steps, all completed
      open({ title: 'Test', steps: ['Step A', 'Step B'] })
      useTransactionModal.getState().setStepStatus(0, 'completed')
      useTransactionModal.getState().setStepStatus(1, 'completed')

      // setError with out-of-bounds index 5
      setError(5, 'Late error')

      const { steps, errorMessage } = useTransactionModal.getState()
      expect(errorMessage).toBe('Late error')
      // Falls back to last step (clamped to steps.length - 1)
      expect(steps[1].status).toBe('error')
    })
  })
})
