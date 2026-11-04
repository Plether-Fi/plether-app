# Junior vault return attribution

The junior vault Activity section contains a share-price return waterfall with
7-day and 30-day reporting periods. It reads the existing, deployment-scoped
`GET /api/perps/vaults/history?range=7d&interval=3600` response (or `range=30d`).
Monetary amounts use the current deployment's six-decimal USDG settlement asset.

## Current data availability

The current backend supplies hourly vault conversion prices, assets and supply.
It **does not yet produce component return attribution**. The UI can show the
observed diluted share-price return and APY, but deliberately shows an unavailable
message instead of inventing component bars. The optional contract below is the
integration point for a future audited accounting producer. Adding this frontend
contract does not implement that producer or backfill historical attribution.

`convertToAssets` includes accrued junior maintenance shares in effective supply;
it excludes withdrawal fees. Both period boundaries must be fresh, covered by
the response, and belong to the configured chain/pool/vault deployment. The UI
also rejects missing, stale or discontinuous history within the period.

## Optional backend field

Publish `junior.returnAttribution` only after a complete audited attribution run:

```typescript
{
  method: 'diluted-share-price-v1',
  startTimestamp: number, // actual sampled block time, seconds UTC
  endTimestamp: number,
  startDilutedSharePrice: string, // same WAD integer as first junior point
  endDilutedSharePrice: string,   // same WAD integer as last junior point
  components: {
    netVpi: { contributionPp: number, amountUsdg: string },
    liquidationFees: { contributionPp: number, amountUsdg: string },
    realizedCarry: { contributionPp: number, amountUsdg: string },
    oracleSpread: { contributionPp: number, amountUsdg: string },
    settledTraderPnl: { contributionPp: number, amountUsdg: string },
    openPositionValuation: { contributionPp: number, amountUsdg: string },
    seniorCoupon: { contributionPp: number, amountUsdg: string },
    maintenanceDilution: { contributionPp: number, amountUsdg: string }
  },
  netReturnUsdg: string
}
```

`contributionPp` is an additive percentage-point contribution over the entire
selected period. `amountUsdg` and `netReturnUsdg` are signed six-decimal raw USDG
integer strings. Do not publish rounded chart labels as source contributions.
Missing fields are unavailable, never zero. Coupon and maintenance are deductions.

The producer must account for the share supply and accrued maintenance shares
at each economic event/valuation boundary. Unitize economic changes using the
contemporaneous effective supply and link each component's unit-price change
to the starting diluted share price. Correctly priced deposits and withdrawals
change ownership and capital, not earnings. A whole-period income total divided
by average capital, starting capital or ending TVL is not an acceptable substitute.

The accounting source must separately measure net collected VPI, junior-owned
liquidation fees, collected carry, realized confidence/frozen-market spread,
settled trader PnL excluding those separately counted costs, open-position
valuation changes, senior coupon and issued/accrued maintenance dilution.
Oracle spread must be removed from settlement-price PnL before classifying
settled trader PnL. Open valuation must be measured independently, never derived
as the residual needed to make the result reconcile. The USDG value of maintenance
dilution is an economic ownership transfer, not a cash expense.

The event ledger needs canonical ordering, reorg handling, deployment isolation,
coverage markers, and boundary prices matching the history response. Hourly
asset/supply samples and current account balances cannot reconstruct this ledger
when liquidity and trading activity change within an hour.

## Reconciliation and annualization

The browser independently computes:

```
returnPp = (ending diluted share price / starting diluted share price - 1) * 100
APY = (exp(log(ending price / starting price) * 31536000 / elapsedSeconds) - 1) * 100
```

The final bar always starts at zero. A difference between the component sum and
the observed return is displayed as an outlined **Unexplained difference** bar
and warning, before the final total. A separate USDG discrepancy is reported in
that bar too. Only floating-point noise below `1e-10` pp is suppressed; USDG
integer differences are exact. No discrepancy is assigned to either PnL category.
Zero starting prices, invalid intervals and nonfinite data cannot define a
return. A zero ending price is a valid -100% return; overflowing APY is unavailable.

The gallery's +0.523% and 14.56% are rounded validation examples. Exactly 0.523%
over exactly fourteen days yields 14.57% APY at two decimals; a full-precision
return of approximately 0.522731% rounds to +0.523% and annualizes to 14.56%.
The Storybook fixture demonstrates those display values with illustrative
components, **not an audited reconstruction of the historical baseline**.

## Verification

`juniorReturnAttribution.test.ts` covers return/APY calculations, neutral liquidity
flows, residual handling, invalid data and dilution deductions.
`JuniorVaultReturnBreakdown.test.tsx` covers the waterfall, hover/focus/tap details,
period controls, accessibility text and unavailable attribution. Storybook includes
baseline-style, negative-return, missing-data, residual and mobile examples.
