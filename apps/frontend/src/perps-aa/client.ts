import type {
  SendSponsoredActionInput,
  SponsoredExecutionResult,
} from '@plether/perps-aa-client'
import { sendSponsoredAction } from '@plether/perps-aa-client'
import { findSponsorRequestError } from './errors'

export interface SendSponsoredActionWithRestartInput<
  TOperation,
  TGasEstimate,
  TReceipt,
> extends SendSponsoredActionInput<TOperation, TGasEstimate, TReceipt> {
  maxEstimationRestarts?: number
  onEstimationRestart?: (attempt: number) => void
}

export async function sendSponsoredActionWithRestart<
  TOperation,
  TGasEstimate,
  TReceipt,
>(
  input: SendSponsoredActionWithRestartInput<
    TOperation,
    TGasEstimate,
    TReceipt
  >
): Promise<SponsoredExecutionResult<TReceipt>> {
  const maximumRestarts = input.maxEstimationRestarts ?? 1
  let restartCount = 0

  for (;;) {
    try {
      return await sendSponsoredAction(input)
    } catch (error) {
      const sponsorError = findSponsorRequestError(error)
      if (
        sponsorError?.reason !== 'RESTART_ESTIMATION' ||
        restartCount >= maximumRestarts
      ) {
        throw error
      }

      restartCount += 1
      input.onEstimationRestart?.(restartCount)
    }
  }
}
