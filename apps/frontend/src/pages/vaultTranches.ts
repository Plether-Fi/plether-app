import type { Address } from 'viem'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_SETTLEMENT_TOKEN_LABEL } from '../contracts/perpsAddresses'

export type TrancheId = 'senior' | 'junior'

export interface TrancheDefinition {
  id: TrancheId
  name: string
  token: string
  eyebrow: string
  shortDescription: string
  description: string
  returnModel: string
  lossPriority: string
  withdrawalPriority: string
  upside: string
  primaryRisk: string
  riskLabel: string
  riskVariant: 'info' | 'warning'
  targetReturn: string
  valueClassName: string
  barClassName: string
  featureItems: {
    label: string
    text: string
  }[]
  riskItems: string[]
  address: Address
}

export const VAULT_TRANCHES: Record<TrancheId, TrancheDefinition> = {
  senior: {
    id: 'senior',
    name: 'Senior Vault',
    token: 'psLP',
    eyebrow: 'More protected option',
    shortDescription: 'A targeted return with first priority when withdrawal funds are allocated.',
    description:
      'Senior gives up some upside for greater protection. It receives a targeted return funded by Junior, recovers prior losses before Junior receives new earnings, and takes losses only after Junior is exhausted.',
    returnModel: 'Targeted return funded by Junior',
    lossPriority: 'Second loss, after Junior',
    withdrawalPriority: 'Senior withdrawals are funded before Junior',
    upside: 'Targeted return and recovery priority',
    primaryRisk: 'The targeted return can stop and the vault can still lose value',
    riskLabel: 'Lower relative risk',
    riskVariant: 'info',
    targetReturn: 'Target return',
    valueClassName: 'text-brand-peach',
    barClassName: 'bg-brand-peach',
    featureItems: [
      {
        label: 'Loss order',
        text: 'Junior absorbs realized losses before Senior',
      },
      {
        label: 'Return',
        text: 'Receives its targeted return when funds are available; prior losses are recovered before Junior receives new earnings',
      },
      {
        label: 'Withdrawals',
        text: `Senior withdrawals are funded before Junior whenever ${PERPS_SETTLEMENT_TOKEN_LABEL} is available`,
      },
    ],
    riskItems: [
      'The targeted return is not guaranteed and is limited by available Junior capital.',
      'Losses can reach Senior after Junior is fully exhausted.',
      'Amounts owed to traders are paid before either vault can withdraw.',
      'Positive share value does not guarantee immediate withdrawal liquidity.',
    ],
    address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
  },
  junior: {
    id: 'junior',
    name: 'Junior Vault',
    token: 'pjLP',
    eyebrow: 'Higher-risk option',
    shortDescription: 'Takes losses first in exchange for more variable return potential.',
    description:
      'Junior funds the Senior targeted return and absorbs losses from the shared trading pool first. In exchange, it receives the trading earnings left after Senior is paid, including a share of fees from forced position closures.',
    returnModel: 'Variable return from trading activity',
    lossPriority: 'First loss',
    withdrawalPriority: 'Available after Senior withdrawals and the required safety buffer',
    upside: 'Variable return from trading activity',
    primaryRisk: 'Can lose some or all of its value before Senior begins taking losses',
    riskLabel: 'Higher relative risk',
    riskVariant: 'warning',
    targetReturn: 'Variable return',
    valueClassName: 'text-brand-orange',
    barClassName: 'bg-brand-orange',
    featureItems: [
      {
        label: 'Loss order',
        text: 'Absorbs realized losses first, protecting Senior',
      },
      {
        label: 'Return',
        text: 'Receives remaining trading revenue and the vault share of liquidation charges after Senior obligations',
      },
      {
        label: 'Withdrawals',
        text: 'Junior withdrawals use the cash remaining after Senior withdrawals and the required safety buffer; they may take longer',
      },
    ],
    riskItems: [
      'Junior funds the Senior targeted return from its own capital.',
      'Junior absorbs realized losses from the shared trading pool before Senior is affected.',
      'Junior withdrawals can be unavailable even while its shares still have value.',
      'A sufficiently large loss can wipe out the vault completely.',
    ],
    address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
  },
}
