import type { JuniorReturnAttribution } from '../../api/types'
import type { JuniorReturnPeriod } from '../../utils/juniorReturnAttribution'

/** Illustrative attribution only. Not an audited reproduction of the competition ledger. */
export const juniorReturnExamplePeriod: JuniorReturnPeriod = {
  startTimestamp: 1_789_333_200,
  endTimestamp: 1_789_333_200 + 14 * 86400,
  startDilutedSharePrice: '1000000000000000000',
  endDilutedSharePrice: '1005227311518081856',
}

export const juniorReturnExample: JuniorReturnAttribution = {
  method: 'diluted-share-price-v1',
  ...juniorReturnExamplePeriod,
  components: {
    netVpi: { contributionPp: 0.292, amountUsdc: '3810000000000' },
    liquidationFees: { contributionPp: 0.205, amountUsdc: '2730000000000' },
    realizedCarry: { contributionPp: 0.054, amountUsdc: '720000000000' },
    oracleSpread: { contributionPp: 0.028, amountUsdc: '370000000000' },
    settledTraderPnl: { contributionPp: 0.087, amountUsdc: '1160000000000' },
    openPositionValuation: { contributionPp: -0.097, amountUsdc: '-1200000000000' },
    seniorCoupon: { contributionPp: -0.008, amountUsdc: '-96261950000' },
    maintenanceDilution: { contributionPp: -0.0382688481918144, amountUsdc: '-513738050000' },
  },
  netReturnUsdc: '6980000000000',
}
