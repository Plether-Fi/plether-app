// Reviewed against the pinned release; see ../DIAGRAM_REVIEW.md.
const how = name => `how-plether-works/${name}.md`
const trade = name => `trading-on-plether-perps/${name}.md`
const step = (title, body = '', tone = 'neutral') => ({ title, body, tone })
const flow = (...steps) => ({ type: 'flow', steps })
const note = text => ({ type: 'note', text })
const lanes = (...items) => ({ type: 'lanes', items })
const lane = (title, tone, ...steps) => ({ title, tone, steps })
const section = title => ({ type: 'section', title })
const spec = (id, source, title, category, description, blocks, evidence) => ({ id, source, title, category, description, blocks, evidence })

const sponsor = (id, source, title) => spec(id, source, title, 'SPONSORED TRANSACTIONS',
  'Preparation and sponsorship precede the wallet signature. Successful operation confirmation is separate from delayed-order execution.', [flow(
    step('Prepare', 'Build the operation, estimate gas and obtain sponsorship.'),
    step('Confirm in wallet', 'Sign the exact prepared operation.', 'accent'),
    step('Submit', 'Send the signed operation to the bundler.'),
    step('Pending onchain', 'Wait for an inclusion receipt.', 'warning'),
    step('Confirmed', 'Verify that the operation succeeded onchain.', 'success')),
    note('For an order, confirmed means committed — not filled. Errors or an unknown outcome require receipt reconciliation.')], ['aa'])

const collection = (id, source, title) => spec(id, source, title, 'V2 CLOSE ACCOUNTING',
  'Price losses consume same-account trader claims, then collectible PnL pledge. Action charges use separate sources and cannot consume PnL pledge.', [
    lanes(lane('Price loss', 'danger',
      step('Net own claims', 'Only this account’s trader claims.'),
      step('Consume PnL pledge', 'Within the terminal collectible cap.'),
      step('Write off excess', 'Any price loss beyond that cap.')),
    lane('Action charges', 'accent',
      step('Withhold from gain', 'Offset a positive price gain first.'),
      step('Use eligible funds', 'VPI clawback reserve, spendable action reserve, then free settlement.'),
      step('Full-close fallback', 'Committed-order margin, then waive uncollectible charges.'))),
    note('A partial close cannot waive action charges. PnL pledge and protected execution bounties are not general fee collateral.')], ['close', 'settlement'])

const positionFlow = (id, source, title, setup, result) => spec(id, source, title, 'DELAYED ORDERS',
  'Review and authorize a bounded commitment. A successfully submitted order joins FIFO, then executes, remains pending or fails without the requested position change.', [
    flow(step('Configure and review', setup), step('Authorize commitment', 'Prepare sponsorship, sign, submit and verify onchain success.', 'accent'), step('Wait in FIFO', 'The order is pending; required funds are reserved.', 'warning')),
    section('Execution has separate outcomes'),
    lanes(lane('Executed', 'success', step('Apply the trade', result)), lane('Not executed', 'warning', step('Pending or failed', 'A temporary blocker leaves it pending. A terminal receipt records failure.'))),
    note('Confirmation is not a fill. Read the order receipt before assuming that your position changed.')], ['orders', 'aa'])

const claimCredit = lanes(
  lane('Position still open', 'accent', step('Credit PnL pledge', 'The funded price payout backs the remaining position.')),
  lane('No open position', 'success', step('Credit settlement', 'USDC enters the Margin Account; withdrawal is a separate action.')))

export const diagrams = [
  spec('delayed-order-execution-pipeline', how('how-orders-execute'), 'From review to execution', 'DELAYED ORDERS',
    'A bounded order is committed, queued and assessed using the execution-time oracle regime. Success, temporary deferral and terminal failure are distinct.', [
      flow(step('Review the bounds', 'Check size, acceptable price, expiry and permitted execution modes.'), step('Commit onchain', 'The signed operation creates the pending order.', 'accent'), step('Wait for FIFO eligibility', 'Earlier entries must be handled first.', 'warning'), step('Assess the execution', 'Use an eligible oracle observation; check mode, price, risk and signed bounds.')),
      lanes(lane('Pass', 'success', step('Execute', 'Apply the position and accounting changes.')), lane('Cannot execute', 'warning', step('Defer or fail', 'Temporary blockers stay pending. Terminal reasons end the order.')))], ['orders']),
  sponsor('order-sponsored-submission-lifecycle', how('how-orders-execute'), 'The sponsored submission'),
  spec('preview-commit-finalize', how('how-orders-execute'), 'One review, two onchain stages', 'ORDER LIFECYCLE',
    'Preview is not a transaction. Commitment and later keeper execution are separate onchain stages.', [
      flow(step('Preview', 'Review an estimate. Nothing has been committed.'), step('Commit', 'Authorize an order and reserve its required funds.', 'accent'), step('Execute later', 'A keeper processes the eligible queued order.', 'success')),
      note('The requested trade happens only if execution succeeds.')], ['orders']),
  collection('final-collection-priority', how('how-orders-execute'), 'Two separate collection paths'),
  spec('pnl-and-close-settlement-outcomes', how('how-pnl-is-calculated'), 'Price PnL is not cash settlement', 'PNL & ACCOUNTING',
    'Unrealized price PnL contributes to price-risk equity. A close separates realized price PnL from action costs; positive residual price payouts may be funded or recorded as claims.', [
      section('While the position is open'), flow(step('Unrealized price PnL', 'From side, entry cost, quantity and current mark.'), step('Price-risk equity', 'PnL pledge + nettable own claim + unrealized price PnL.')),
      section('When a close executes'),
      lanes(lane('Price gain', 'success', step('Withhold action costs', 'Any residual price payout is funded in full or recorded as a claim.'), step('Credit the right bucket', 'Open remainder: PnL pledge. No position: settlement.')), lane('Price loss', 'danger', step('Net claim, then pledge', 'The terminal cap limits collection.'), step('Settle costs separately', 'Fees, carry and VPI use their eligible action funds.'))),
      note('A net action rebate is limited by available cash. An unpaid rebate is waived; it does not become a trader claim.')], ['close', 'risk', 'claim']),
  spec('weekly-market-state-schedule', how('market-states-and-oracle-closures'), 'The FX weekend, in New York time', 'MARKET CALENDAR',
    'Regular weekly phases in America/New_York: open until Friday 16:30, close-only with live policy until 17:00, frozen until Sunday 17:00, then close-only until 17:15. UTC offsets change with US daylight saving.', [
      flow(step('Open', 'Sunday 17:15 → Friday 16:30', 'success'), step('Close-only · live oracle', 'Friday 16:30 → 17:00 · 30 minutes', 'warning'), step('Close-only · frozen oracle', 'Friday 17:00 → Sunday 17:00', 'danger'), step('Close-only · live oracle', 'Sunday 17:00 → 17:15 · 15 minutes', 'warning'), step('Open again', 'Sunday 17:15, if no override applies.', 'success')),
      note('17:00 New York = 21:00 UTC in daylight time, 22:00 UTC in standard time. Closure-day overrides may extend restrictions.'),
      note('Sequence is not drawn to a time scale. Oracle data must still satisfy the active policy.')], ['calendar']),
  spec('frozen-close-collection-priority', how('market-states-and-oracle-closures'), 'Where frozen-close spread sits', 'CLOSE ACTION COSTS',
    'Frozen-close spread is an action charge, not a price change. Recovered spread is attributed only after execution fee, carry and positive VPI; unrecovered spread is waived.', [
      flow(step('Execution fee', 'Count recovered action charges against this first.'), step('Carry + positive VPI', 'These prior charges precede spread recovery.'), step('Frozen-close spread', 'Only the remaining recovered amount counts toward spread.', 'warning')),
      note('This is recovery attribution, not a shared price-loss collateral queue. Negative VPI can offset the net action charge; uncollected spread is waived.')], ['settlement', 'close']),
  spec('settlement-liquidity-flow', how('settlement-liquidity-and-trader-claims'), 'How a price payout reaches you', 'SETTLEMENT LIQUIDITY',
    'After action costs are withheld, a residual price payout is paid in full only if free pool cash after existing claims covers it. Otherwise the full residual becomes a claim.', [
      flow(step('Close accounting', 'Separate price PnL, action charges and pledge release.'), step('Residual price payout', 'Positive price gain left after withheld action costs.', 'accent')),
      lanes(lane('Cash covers it', 'success', step('Fund in full', 'Pool cash after existing claims covers the entire payout.')), lane('Cash is insufficient', 'warning', step('Record a claim', 'The entire residual payout waits; no partial price payout.'))),
      section('When funded now or settled later'), claimCredit,
      note('Later claim settlement requires pool cash to cover all claims. A partial close releases only pledge allowed by the remaining terminal cap.')], ['close', 'claim']),
  collection('claim-collateral-collection-order', how('settlement-liquidity-and-trader-claims'), 'Claims, pledge and action costs'),
  spec('liquidity-pool-tranche-waterfall', how('the-liquidity-pool-and-tranche-waterfall'), 'The Senior–Junior waterfall', 'LIQUIDITY PROVIDERS',
    'Reconciliation compares distributable assets after trader liabilities with claimant principal. Losses hit Junior first; revenue restores Senior impairment before reaching Junior.', [
      flow(step('Distributable pool value', 'Reconcile value after trader liabilities.')),
      lanes(lane('Loss', 'danger', step('Junior first', 'Absorb loss up to Junior principal.'), step('Senior second', 'Absorb only the remaining loss.')), lane('Revenue', 'success', step('Restore Senior', 'Fill impairment up to its high-water mark.'), step('Junior receives the rest', 'Residual revenue accrues to Junior.'))),
      note('The Senior coupon is a separate Junior-to-Senior transfer, capped by available Junior principal. Senior is last-loss, not risk-free.')], ['waterfall']),
  spec('trading-price-and-settlement-costs', how('trading-costs-fees-carry-and-vpi'), 'Price and costs are different inputs', 'TRADE ECONOMICS',
    'Oracle price determines price PnL. Fees, carry, VPI and frozen spread are separate action economics, not a synthetic execution price.', [
      lanes(lane('Price PnL', 'accent', step('Oracle price + position', 'Side, exact entry cost and quantity determine price gain or loss.')), lane('Action economics', 'warning', step('Fees + carry + VPI', 'Add frozen-close spread when applicable. VPI can be signed.'))),
      flow(step('Separate settlement paths', 'Costs may offset a gain. Price losses and remaining action charges use different backing.')),
      note('Do not treat a displayed price change as net profit or a guarantee of immediately withdrawable USDC.')], ['close']),
  collection('protocol-close-collection-order', how('trading-costs-fees-carry-and-vpi'), 'How close obligations are funded'),
  spec('margin-reduces-future-carry', how('trading-costs-fees-carry-and-vpi'), 'More pledge, less borrowed backing', 'CARRY',
    'For the same maximum-profit exposure, more assigned PnL pledge lowers max(maximum profit minus pledge, zero), reducing future carry at a given index growth.', [
      flow(step('Add assigned PnL pledge', 'Increase the margin backing this position.', 'accent'), step('Reduce the borrow base', 'Borrow base = max(maximum profit − pledge, 0).'), step('Lower future carry', 'At the same carry-index growth, less borrowed backing accrues less carry.', 'success')),
      note('Holding other conditions equal. Free USDC alone is not assigned pledge; adding margin does not undo carry already accrued.')], ['risk']),
  spec('senior-junior-waterfall-rules', 'liquidity-provider-quickstart.md', 'Three rules for LP returns', 'SENIOR & JUNIOR',
    'Losses go through Junior before Senior; revenue restores Senior impairment before reaching Junior; the coupon transfers available Junior principal to Senior.', [
      section('01 · Loss absorption'), flow(step('Junior → Senior', 'Junior takes the first loss. Senior absorbs the remainder.', 'danger')),
      section('02 · New revenue'), flow(step('Restore Senior → Junior', 'Restore Senior to its high-water mark, then credit residual revenue to Junior.', 'success')),
      section('03 · Target coupon'), flow(step('Junior funds Senior', 'The transfer is capped by available Junior principal.', 'accent')),
      note('A target coupon is not guaranteed yield. Both tranches can lose value.')], ['waterfall']),
  sponsor('quickstart-sponsored-operation-lifecycle', 'trader-quickstart.md', 'From preparation to confirmation'),
  spec('trader-claim-lifecycle', trade('check-and-settle-a-trader-claim'), 'From unpaid payout to funded claim', 'TRADER CLAIMS',
    'An unfunded residual price payout becomes a claim. Claim settlement is all-or-nothing once aggregate cash coverage returns, and the credit bucket depends on whether a position remains open.', [
      flow(step('A price payout cannot be funded', 'Record the full residual payout as a trader claim.', 'warning'), step('Aggregate coverage returns', 'Pool cash must cover all outstanding trader claims.'), step('Authorize Settle Claim', 'The owning Trading Account submits the sponsored action.', 'accent')),
      claimCredit, note('Settlement is not withdrawal. Pledged credit remains backing for an open position; only eligible free funds can be withdrawn.')], ['claim', 'aa']),
  spec('claim-settlement-funding-path', trade('check-and-settle-a-trader-claim'), 'Where settled claim funds go', 'FUNDING PATH',
    'Covered HousePool cash is sent to the Margin Clearinghouse, which credits PnL pledge for a live position or free settlement otherwise.', [
      flow(step('Liquidity pool', 'Cash coverage passes for all outstanding claims.'), step('Margin Clearinghouse', 'Receives USDC for the claim-owning Trading Account.', 'accent')), claimCredit], ['claim']),
  spec('wallet-trading-account-ownership', trade('gas-sponsored-trading-and-your-plether-trading-account'), 'Your wallet controls the account', 'OWNERSHIP',
    'The owner wallet signs for the distinct Trading Account. Protocol positions, orders, protection, margin and claims are keyed to that Trading Account.', [
      flow(step('Owner wallet', 'You review and sign the authorized action.'), step('Trading Account', 'A separate smart-account address you control.', 'accent')),
      { type: 'group', title: 'Protocol state owned by that account', items: ['Positions and TP/SL', 'Orders and execution bounds', 'Margin balances and reserves', 'Trader claims'] },
      note('The owner wallet and Trading Account are different addresses. Verify the correct address before sending funds.')], ['aa', 'accounts']),
  spec('authorization-and-gas-sponsorship', trade('gas-sponsored-trading-and-your-plether-trading-account'), 'Sponsorship before the signature', 'AUTHORIZATION',
    'Prepare the action, request stub data, estimate gas and attach final paymaster data before the owner signs. Then submit the signed operation and verify the result.', [
      flow(step('Review the action', 'Check the account, amounts and execution bounds.'), step('Prepare sponsorship', 'Get stub data, estimate gas and attach final paymaster data.'), step('Sign in your wallet', 'Authorize the operation including its sponsorship data.', 'accent'), step('Submit and verify', 'The sponsor covers eligible network gas. Check onchain success.', 'success')),
      note('Sponsorship does not pay your margin, execution bounties or trading costs.')], ['aa']),
  spec('sponsored-withdrawal-flow', trade('gas-sponsored-trading-and-your-plether-trading-account'), 'Withdraw to your owner wallet', 'MARGIN WITHDRAWAL',
    'Review the free amount and verified owner recipient, prepare sponsorship, sign and confirm. Withdrawal remains subject to onchain account-health and balance checks.', [
      flow(step('Choose an eligible amount', 'Use Withdrawable, not total collateral or unrealized profit.'), step('Verify the recipient', 'Check the Trading Account and displayed owner wallet.'), step('Prepare and sign', 'Authorize the sponsored withdrawal.', 'accent'), step('Confirm receipt', 'After successful execution, check owner-wallet USDC.', 'success')),
      note('The contract rechecks balances and account health. An estimate or wallet signature alone does not complete a withdrawal.')], ['accounts', 'aa']),
  positionFlow('open-increase-position-lifecycle', trade('open-or-increase-a-position'), 'Open or increase a position', 'Choose direction, quantity, leverage and execution protections.', 'Create the position or increase its size and update its backing.'),
  sponsor('open-increase-sponsored-submission', trade('open-or-increase-a-position'), 'Authorize an opening order'),
  spec('account-health-reading-order', trade('read-your-position-and-account-health'), 'Read account health in context', 'ACCOUNT CHECKLIST',
    'Recommended reading order, not a calculation or state transition: market regime, position, equity and margin, liquidation context, pending obligations and spendable balances.', [
      { type: 'checklist', steps: [step('Market state', 'Check oracle availability and close-only restrictions.'), step('Current position', 'Confirm side, size and entry price.'), step('Equity and margin', 'Compare signed position equity with maintenance. Settlement balance is separate.'), step('Liquidation context', 'Treat liquidation price as an estimate, not a guarantee.'), step('Pending obligations', 'Check orders, TP/SL and reserved funds.'), step('Spendable balances', 'Available to Trade and Withdrawable have different checks.')] }], ['risk', 'accounts']),
  positionFlow('reduce-close-position-lifecycle', trade('reduce-or-close-a-position'), 'Reduce or close a position', 'Choose the reduction and review its price and financial bounds.', 'Reduce or remove the position and apply the close accounting.'),
  sponsor('reduce-close-sponsored-submission', trade('reduce-or-close-a-position'), 'Authorize a closing order'),
  spec('confirmed-order-execution-path', trade('trader-troubleshooting'), 'Confirmed does not mean filled', 'ORDER TROUBLESHOOTING',
    'A successful sponsored commitment creates a pending order. Only a successful order execution applies the requested position change.', [
      flow(step('Commitment confirmed', 'The sponsored operation succeeded onchain.', 'accent'), step('Order pending', 'Wait for queue, oracle and policy eligibility.', 'warning')),
      lanes(lane('Execution succeeds', 'success', step('Position changes', 'Verify the executed order and resulting position.')), lane('Execution does not succeed', 'warning', step('Check the outcome', 'It may remain pending or end in terminal failure.')))], ['orders', 'aa']),
  sponsor('troubleshooting-sponsored-submission', trade('trader-troubleshooting'), 'Check the submission stage'),
  spec('sponsorship-vs-order-failure-lifecycles', trade('why-is-my-order-pending-or-failed'), 'Two lifecycles, different failures', 'ORDER TROUBLESHOOTING',
    'Sponsored transaction success and order execution are different lifecycles. V2 transient oracle, gas, engine or receipt failures can remain pending; only terminal reasons fail the order.', [
      section('01 · Sponsored transaction'), flow(step('Prepare → sign → submit', 'An inclusion receipt must confirm successful execution.', 'accent')),
      note('A revert, dropped operation or unknown outcome is not proof that an order exists. Reconcile its receipt and client order ID.'),
      section('02 · Delayed order'),
      lanes(lane('Still pending', 'warning', step('Temporary blockers', 'Queue, oracle timing, insufficient gas, engine or receipt failure.')), { ...lane('Terminal result', 'accent', step('Executed', 'The requested trade was applied.'), step('Failed', 'Expiry, slippage, config or mode mismatch, risk-off, planner or constraint rejection, liquidation.')), alternatives: true }),
      note('An engine panic is not automatically a terminal order failure. Read the stored V2 receipt and pending reason.')], ['orders', 'aa']),
  spec('usdc-account-flow', trade('your-margin-account'), 'USDC has distinct places to live', 'ACCOUNT BALANCES',
    'Trading Account wallet USDC can be deposited into clearinghouse settlement custody. Free funds, PnL pledge and action reserves have different permissions; withdrawal does not require first making a trade.', [
      flow(step('Trading Account wallet', 'Fund with a token transfer or the testnet faucet.'), step('Deposit margin', 'USDC enters the Margin Clearinghouse.', 'accent')),
      { type: 'group', title: 'Internal account balances', items: ['Free settlement — eligible for actions', 'PnL pledge — backs position price risk', 'Action, order and liquidation reserves'] },
      note('Trading moves value between eligible buckets. A deposit does not have to pass through a trade before it can be withdrawn.'),
      flow(step('Withdraw eligible free USDC', 'Subject to reserves, carry and account-health checks.'), step('Owner wallet receives USDC', 'After the sponsored withdrawal succeeds.', 'success'))], ['accounts', 'claim']),
  spec('claim-to-owner-wallet', trade('your-margin-account'), 'A claim is not wallet cash', 'CLAIM SETTLEMENT',
    'Claim coverage and settlement precede account credit. Live-position credit is pledged, so withdrawal requires eligible free settlement and a separate authorized action.', [
      flow(step('Check claim coverage', 'Pool cash must cover all outstanding claims.', 'warning'), step('Authorize settlement', 'Submit Settle Claim from the owning Trading Account.', 'accent')), claimCredit,
      flow(step('Review Withdrawable', 'Only eligible free settlement can leave the account.'), step('Authorize withdrawal', 'A separate operation sends USDC to the owner wallet.', 'success'))], ['claim', 'accounts', 'aa']),
]

export const evidence = {
  calendar: ['packages/perps/src/libraries/MarketCalendarLib.sol:marketStatus,newYorkMarketBoundary'],
  orders: ['packages/perps/src/OrderV2Types.sol:TerminalReason,PendingReason,ExecutionBounds', 'packages/perps/src/OrderRouterV2ExecutionSidecar.sol:executeOrder,executeOrderBatch', 'packages/perps/src/libraries/OrderOraclePolicyLib.sol:getOracleExecutionPolicy'],
  close: ['packages/perps/src/libraries/CfdEnginePlanLib.sol:_planIsolatedCloseSettlement,_planCloseActionSettlement,_maxPricePledgeUnlockPreservingTerminalCap'],
  settlement: ['packages/perps/src/CfdEngineSettlementSidecar.sol:executeClose,_settleCloseActionCharge,_recoveredFrozenSpreadUsdc'],
  claim: ['packages/perps/src/CfdEngine.sol:settleTraderClaim,_settleTraderClaimBalance,_payOrRecordTraderClaim,_availableCashForFreshPoolPayouts'],
  risk: ['packages/perps/src/libraries/PositionRiskAccountingLib.sol:computeBorrowBaseUsdc,computeIndexedCarryUsdc,buildExactPriceRiskState'],
  waterfall: ['packages/perps/src/libraries/HousePoolWaterfallAccountingLib.sol:absorbLoss,distributeRevenue,paySeniorCoupon'],
  accounts: ['packages/perps/src/MarginClearinghouse.sol', 'packages/perps/src/CfdEngineSettlementSidecar.sol:validateWithdraw', 'apps/frontend/vendor/perps-aa-client/dist/actions.js'],
  aa: ['apps/frontend/vendor/perps-aa-client/dist/orchestrator.js:sendSponsoredAction', 'apps/frontend/src/utils/sponsoredOperation.ts:sponsoredOperationStatusLabel', 'apps/frontend/src/perps-aa/SponsoredOperationRecovery.tsx'],
}
