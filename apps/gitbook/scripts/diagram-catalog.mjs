// Reviewed against the pinned Core v2.1.0 release; see ../DIAGRAM_REVIEW.md.
const how = name => `how-plether-works/${name}.md`
const trade = name => `trading-on-plether-perps/${name}.md`
const step = (title, body = '', tone = 'neutral') => ({ title, body, tone })
const flow = (...steps) => ({ type: 'flow', steps })
const note = text => ({ type: 'note', text })
const lanes = (...items) => ({ type: 'lanes', items })
const lane = (title, tone, ...steps) => ({ title, tone, steps })
const section = title => ({ type: 'section', title })
const spec = (id, source, title, category, description, blocks, evidence) => ({ id, source, title, category, description, blocks, evidence })

const sponsor = (id, source, title, noteText = 'For an order, confirmed means committed — not filled. Errors or an unknown outcome require receipt reconciliation.') => spec(id, source, title, 'SPONSORED TRANSACTIONS',
  'Preparation and sponsorship precede the wallet signature. Successful operation confirmation is separate from delayed-order execution.', [flow(
    step('Prepare', 'Build the operation, estimate gas and obtain sponsorship.'),
    step('Confirm in wallet', 'Sign the exact prepared operation.', 'accent'),
    step('Submit', 'Send the signed operation to the bundler.'),
    step('Pending onchain', 'Wait for an inclusion receipt.', 'warning'),
    step('Confirmed', 'Verify that the operation succeeded onchain.', 'success')),
    note(noteText)], ['aa'])

const collection = (id, source, title, focusNote = 'A partial close cannot waive action charges. Remaining position backing and other orders’ execution rewards stay protected after carry. Reduce and close orders reserve no execution reward.') => spec(id, source, title, 'CLOSE ACCOUNTING',
  'Carry consumes active position margin first, then free USDG. Price losses then use claims and remaining collectible pledge; later action charges can use safely released margin, not residual position backing.', [
    flow(step('Collect accrued carry', 'Use active position margin first, then eligible free USDG.'), step('Keep unpaid carry visible', 'Any remainder joins the close action charge; claims and other locked buckets do not fund the carry checkpoint.')),
    lanes(lane('Price loss', 'danger',
      step('Net own claims', 'Only this account’s trader claims.'),
      step('Consume PnL pledge', 'Within the terminal collectible cap.'),
      step('Write off excess', 'Any price loss beyond that cap.')),
    lane('Action charges', 'warning',
      step('Withhold from gain', 'Offset a positive price gain first.'),
      step('Use eligible funds', 'VPI clawback reserve, spendable action reserve, then free USDG.'),
      step('Use safely released margin', 'Only pledge the close can release while preserving remaining-position health.'),
      step('Full-close fallback', 'Committed-order margin, then waive uncollectible charges.'))),
    note(focusNote)], ['close', 'settlement', 'currentCarry', 'currentCloseFunding', 'currentCloseReward'])

const positionFlow = (id, source, title, setup, result) => spec(id, source, title, 'DELAYED ORDERS',
  'Review and authorize a bounded commitment. A successfully submitted order joins FIFO, then executes, remains pending or fails without the requested position change.', [
    flow(step('Configure and review', setup), step('Authorize commitment', 'Prepare sponsorship, sign, submit and verify onchain success.', 'accent'), step('Wait in FIFO', 'The order is pending; required funds are reserved.', 'warning')),
    section('Execution has separate outcomes'),
    lanes(lane('Executed', 'success', step('Apply the trade', result)), lane('Not executed', 'warning', step('Pending or failed', 'A temporary blocker leaves it pending. A terminal receipt records failure.'))),
    note('Confirmation is not a fill. Read the order receipt before assuming that your position changed.')], ['orders', 'aa'])

const claimCredit = lanes(
  lane('Position still open', 'accent', step('Credit PnL pledge', 'The funded price payout backs the remaining position.')),
  lane('No open position', 'success', step('Credit settlement', 'USDG enters the Margin Account; withdrawal is a separate action.')))

export const diagrams = [
  spec('plether-system-overview', 'README.md', 'How Plether Perps fits together', 'OVERVIEW',
    'Pyth FX feeds set the bounded plDXY price. Traders commit LONG USD or SHORT USD orders via gas-sponsored Trading Accounts, while Senior and Junior LP vaults underwrite bounded payouts.', [
      {
        type: 'systemMap',
        oracleTitle: 'Pyth FX Oracle (6 currencies)',
        oracleBody: 'Produces the bounded 0.00 to 2.00 plDXY mark — no order book or AMM.',
        engineTitle: 'Plether Perps Engine',
        engineSub: 'Delayed FIFO · Solvency · VPI & carry',
      },
      lanes(
        lane('Traders', 'success',
          step('LONG or SHORT USD', 'Gas-sponsored Trading Account posts USDG margin.'),
          step('Delayed global FIFO', 'Commit first; fill on the next eligible Pyth tick.')),
        lane('Liquidity pool', 'warning',
          step('Senior & Junior vaults', 'Owner-wallet USDG deposits underwrite bounded payouts.'),
          step('Waterfall & firewall', 'Junior takes first loss; trader reserves come first.'))),
      note('The oracle sets the price. VPI, carry and bounded solvency limits price pool risk without peer-to-peer matching.')], ['index', 'orders', 'waterfall']),
  spec('dollar-index-bounded-payoff', 'welcome/how-plether-works-in-5-minutes.md', 'Inverse basket, bounded 0.00 to 2.00', 'THE DOLLAR INDEX',
    'The raw foreign-currency basket moves opposite to USD strength. The interface displays 2.00 minus the raw basket so LONG USD rises with the dollar and payouts stay bounded.', [
      { type: 'payoffAxis' },
      lanes(
        lane('LONG USD', 'success',
          step('Profits as USD rises', 'Raw basket falls toward 0.00; plDXY rises toward 2.00.'),
          step('Max gross profit', 'Entry raw basket × quantity.')),
        lane('SHORT USD', 'warning',
          step('Profits as USD falls', 'Raw basket rises toward 2.00; plDXY falls toward 0.00.'),
          step('Max gross profit', '(2.00 − entry raw basket) × quantity.'))),
      note('Finite bounds let Plether verify worst-case pool backing before admitting new risk. Bounds are not liquidation protection.')], ['index', 'risk']),
  spec('lp-hourly-epoch-timeline', 'liquidity-provider-quickstart.md', 'Hourly epochs and 4-phase settlement', 'HOURLY LP EPOCHS',
    'Owner-wallet LP requests queue before the five-minute hourly cutoff, settle in strict four-phase order (Senior withdraw, Junior withdraw, Junior deposit, Senior deposit), and age a one-hour cooldown.', [
      flow(step('Queue request (owner wallet)', 'Requires Arbitrum Sepolia ETH. Inclusion within 5 minutes of the hour rolls to the next hour. Cancellable before boundary.')),
      { type: 'epochPhases' },
      lanes(
        lane('Deposit outcome', 'success',
          step('Shares ready (1h)', 'Active shares in custody; Move shares preserves cooldown.'),
          step('Withdrawal ready', 'Cooldown elapsed; Queue direct withdrawal unlocks.')),
        lane('Withdrawal outcome', 'warning',
          step('USDG ready', 'Funded proceeds wait for Move USDG to wallet.'),
          step('Cancel or share return', 'Restarts 1h cooldown for your whole tranche wallet balance.'))),
      note('Trader reserves and Senior backlog always settle ahead of Junior withdrawals. Positive NAV does not guarantee instant cash.')], ['lpEpochs', 'waterfall']),
  spec('delayed-order-execution-pipeline', how('how-orders-execute'), 'From review to execution', 'DELAYED ORDERS',
    'A bounded order is committed, queued and assessed using the execution-time oracle regime. Success, temporary deferral and terminal failure are distinct.', [
      flow(step('Review the bounds', 'Check size, acceptable price, expiry and permitted execution modes.'), step('Commit onchain', 'The signed operation creates the pending order.', 'accent'), step('Wait for FIFO eligibility', 'Earlier entries must be handled first.', 'warning'), step('Assess the execution', 'Use an eligible oracle observation; check mode, price, risk and signed bounds.')),
      lanes(lane('Pass', 'success', step('Execute', 'Apply the position and accounting changes.')), lane('Cannot execute', 'warning', step('Defer or fail', 'Temporary blockers stay pending. Terminal reasons end the order.')))], ['orders']),
  sponsor('order-sponsored-submission-lifecycle', how('how-orders-execute'), 'The sponsored submission', 'Order commitment confirms your place in global FIFO. Keeper execution happens separately on the next eligible oracle tick.'),
  spec('preview-commit-finalize', how('how-orders-execute'), 'One review, two onchain stages', 'ORDER LIFECYCLE',
    'Preview is not a transaction. Commitment and later keeper execution are separate onchain stages.', [
      flow(step('Preview', 'Review an estimate. Nothing has been committed.'), step('Commit', 'Authorize an order and reserve its required funds.', 'accent'), step('Execute later', 'A keeper processes the eligible queued order.', 'success')),
      note('The requested trade happens only if execution succeeds.')], ['orders']),
  collection('final-collection-priority', how('how-orders-execute'), 'Two separate collection paths', 'A partial close cannot waive action charges. Remaining position backing and other orders’ execution rewards stay protected after carry. Reduce and close orders reserve no execution reward.'),
  spec('pnl-and-close-settlement-outcomes', how('how-pnl-is-calculated'), 'Price PnL is not cash settlement', 'PNL & ACCOUNTING',
    'Unrealized price PnL contributes to price-risk equity. A close separates realized price PnL from action costs; positive residual price payouts may be funded or recorded as claims.', [
      section('While the position is open'), flow(step('Unrealized price PnL', 'From side, entry cost, quantity and current mark.'), step('Price-risk equity', 'Position margin after accrued carry + nettable own claim + unrealized price PnL.')),
      section('When a close executes'),
      flow(step('Collect accrued carry', 'Active position margin first, then eligible free USDG. Carry not collected joins the action charge.')),
      lanes(lane('Price gain', 'success', step('Withhold action costs', 'Any residual price payout is funded in full or recorded as a claim.'), step('Credit the right bucket', 'Open remainder: PnL pledge. No position: settlement.')), lane('Price loss', 'danger', step('Net claim, then pledge', 'The terminal cap limits collection.'), step('Settle costs separately', 'Fees, VPI and unpaid carry use eligible action funds, including safely released margin.'))),
      note('A net action rebate is limited by available cash. An unpaid rebate is waived; it does not become a trader claim.')], ['close', 'risk', 'claim', 'currentCarry', 'currentCloseFunding']),
  spec('weekly-market-state-schedule', how('market-states-and-oracle-closures'), 'The FX weekend, in New York time', 'MARKET CALENDAR',
    'Regular weekly phases in America/New_York: open until Friday 16:50, close-only with live policy until 17:00, frozen until Sunday 17:00, then close-only until 17:05. UTC offsets change with US daylight saving.', [
      { type: 'scheduleBar' },
      flow(step('Open', 'Sunday 17:05 → Friday 16:50', 'success'), step('Close-only · live oracle', 'Friday 16:50 → 17:00 · 10 minutes', 'warning'), step('Close-only · frozen oracle', 'Friday 17:00 → Sunday 17:00', 'danger'), step('Close-only · live oracle', 'Sunday 17:00 → 17:05 · 5 minutes', 'warning'), step('Open again', 'Sunday 17:05, if no override applies.', 'success')),
      note('17:00 New York = 21:00 UTC in daylight time, 22:00 UTC in standard time. Sequence is not drawn to a time scale; closure-day overrides and oracle policy still apply.')], ['calendar']),
  spec('frozen-close-collection-priority', how('market-states-and-oracle-closures'), 'Where frozen-close spread sits', 'CLOSE ACTION COSTS',
    'Frozen-close spread is an action charge, not a price change. Recovery follows execution fee, carry left unpaid after initial collection, and positive VPI; unrecovered spread is waived.', [
      flow(step('Execution fee', 'Count recovered action charges against this first.'), step('Unpaid carry + positive VPI', 'Carry already collected from margin or free USDG is not charged again.'), step('Frozen-close spread', 'Only the remaining recovered amount counts toward spread.', 'warning')),
      note('This is recovery attribution, not a shared price-loss collateral queue. Negative VPI can offset the net action charge; uncollected spread is waived.')], ['settlement', 'close', 'currentCarry', 'currentCloseFunding']),
  spec('settlement-liquidity-flow', how('settlement-liquidity-and-trader-claims'), 'How a price payout reaches you', 'SETTLEMENT LIQUIDITY',
    'After action costs are withheld, a residual price payout is paid in full only if free pool cash after existing claims covers it. Otherwise the full residual becomes a claim.', [
      flow(step('Close accounting', 'Separate price PnL, action charges and pledge release.'), step('Residual price payout', 'Positive price gain left after withheld action costs.', 'accent')),
      lanes(lane('Cash covers it', 'success', step('Fund in full', 'Pool cash after existing claims covers the entire payout.')), lane('Cash is insufficient', 'warning', step('Record a claim', 'The entire residual payout waits; no partial price payout.'))),
      section('When funded now or settled later'), claimCredit,
      note('Later claim settlement requires pool cash to cover all claims. A partial close releases only pledge allowed by the remaining terminal cap.')], ['close', 'claim']),
  collection('claim-collateral-collection-order', how('settlement-liquidity-and-trader-claims'), 'Claims, pledge and action costs', 'Same-account trader claims net against price losses before PnL pledge, but claims never pay carry, execution fees, VPI or frozen-close spread.'),
  spec('liquidity-pool-tranche-waterfall', how('the-liquidity-pool-and-tranche-waterfall'), 'The Senior–Junior waterfall', 'LIQUIDITY PROVIDERS',
    'Reconciliation compares distributable assets after trader liabilities with claimant principal. Losses hit Junior first; revenue restores Senior impairment before reaching Junior.', [
      { type: 'waterfallBars' },
      lanes(lane('Loss', 'danger', step('Junior first', 'Absorb loss up to Junior principal.'), step('Senior second', 'Absorb only the remaining loss.')), lane('Revenue', 'success', step('Restore Senior', 'Fill impairment up to its high-water mark.'), step('Junior receives the rest', 'Residual revenue accrues to Junior.'))),
      note('The Senior coupon is a separate Junior-to-Senior transfer, capped by available Junior principal. Senior is last-loss, not risk-free.')], ['waterfall']),
  spec('trading-price-and-settlement-costs', how('trading-costs-fees-carry-and-vpi'), 'Price and costs are different inputs', 'TRADE ECONOMICS',
    'Oracle price determines price PnL. Fees, carry, VPI and frozen spread are separate action economics, not a synthetic execution price.', [
      lanes(lane('Price PnL', 'accent', step('Oracle price + position', 'Side, exact entry cost and quantity determine price gain or loss.')), lane('Action economics', 'warning', step('Fees + carry + VPI', 'Add frozen-close spread when applicable. VPI can be signed.'))),
      flow(step('Collect carry first', 'Active position margin first, then eligible free USDG.'), step('Settle the close', 'Unpaid carry joins fees, VPI and spread. Remaining price losses and action charges use their respective backing.')),
      note('Do not treat a displayed price change as net profit or a guarantee of immediately withdrawable USDG.')], ['close', 'currentCarry', 'currentCloseFunding']),
  collection('protocol-close-collection-order', how('trading-costs-fees-carry-and-vpi'), 'How close obligations are funded', 'Execution fees, unpaid carry, positive VPI and frozen spread draw from action-charge sources only; partial closes cannot waive unpaid charges, while full closes waive uncollectible action charges.'),
  spec('margin-reduces-future-carry', how('trading-costs-fees-carry-and-vpi'), 'Margin protects health, not carry', 'CARRY',
    'Carry accrues at a flat annual rate on full maximum-profit exposure, independently of assigned margin or pool utilization. Adding assigned PnL pledge raises price equity without lowering future carry.', [
      { type: 'borrowBar' },
      flow(step('Add assigned PnL pledge', 'Increase the margin backing this position.', 'accent'), step('Raise price-risk equity', 'Larger margin increases the buffer above maintenance and liquidation.', 'success'), step('Keep carry basis unchanged', 'Flat carry still accrues on full maximum-profit exposure at the configured base rate.')),
      note('Adding margin first realizes accrued carry from active margin, then eligible free USDG. Future carry can consume the new margin too.')], ['risk', 'currentCarry']),
  spec('senior-junior-waterfall-rules', 'liquidity-provider-quickstart.md', 'Three rules for LP returns', 'SENIOR & JUNIOR',
    'Losses go through Junior before Senior; revenue restores Senior impairment before reaching Junior; the coupon transfers available Junior principal to Senior.', [
      { type: 'waterfallBars' },
      flow(
        step('01 · Loss absorption', 'Junior absorbs losses first; Senior takes only excess beyond Junior.', 'danger'),
        step('02 · New revenue', 'Restores impaired Senior to its high-water mark before residual flows to Junior.', 'success'),
        step('03 · Target coupon', 'Junior transfers target return to Senior, capped by available Junior principal.', 'accent')),
      note('A target coupon is not guaranteed yield. Both tranches can lose value.')], ['waterfall']),
  sponsor('quickstart-sponsored-operation-lifecycle', 'trader-quickstart.md', 'From preparation to confirmation', 'First-time Trading Account deployment waits for an Arbitrum Sepolia safe block (up to 30 min). Subsequent sponsored operations confirm normally.'),
  spec('trader-claim-lifecycle', trade('check-and-settle-a-trader-claim'), 'From unpaid payout to funded claim', 'TRADER CLAIMS',
    'An unfunded residual price payout becomes a claim. Claim settlement is all-or-nothing once aggregate cash coverage returns, and the credit bucket depends on whether a position remains open.', [
      flow(step('A price payout cannot be funded', 'Record the full residual payout as a trader claim.', 'warning'), step('Aggregate coverage returns', 'Contract verifies pool cash covers all outstanding claims.'), step('Authorize Settle Claim', 'The owning Trading Account submits the sponsored action.', 'accent')),
      claimCredit, note('Settlement is not withdrawal. Pledged credit remains backing for an open position; only eligible free funds can be withdrawn.')], ['claim', 'aa']),
  spec('claim-settlement-funding-path', trade('check-and-settle-a-trader-claim'), 'Where settled claim funds go', 'FUNDING PATH',
    'Covered liquidity pool cash is sent to the Margin Clearinghouse, which credits PnL pledge for a live position or free USDG otherwise.', [
      flow(step('Liquidity pool', 'Cash coverage passes for all outstanding claims.'), step('Margin Clearinghouse', 'Receives USDG for the claim-owning Trading Account.', 'accent')), claimCredit], ['claim']),
  spec('wallet-trading-account-ownership', trade('gas-sponsored-trading-and-your-plether-trading-account'), 'Your wallet controls the account', 'OWNERSHIP',
    'The owner wallet signs for the distinct Trading Account. Protocol positions, orders, protection, margin and claims are keyed to that Trading Account.', [
      flow(step('Owner wallet', 'You review and sign the authorized action.'), step('Trading Account', 'A separate smart-account address you control.', 'accent')),
      { type: 'group', title: 'Protocol state owned by that account', items: ['Positions and TP/SL', 'Orders and execution bounds', 'Margin balances and reserves', 'Trader claims'] },
      note('The owner wallet and Trading Account are different addresses. Verify the correct address before sending funds.')], ['aa', 'accounts']),
  spec('authorization-and-gas-sponsorship', trade('gas-sponsored-trading-and-your-plether-trading-account'), 'Sponsorship before the signature', 'AUTHORIZATION',
    'Prepare the action, request native paymaster data and gas estimates before the owner signs. Then submit the signed operation and verify the result.', [
      flow(step('Review the action', 'Check the account, amounts and execution bounds.'), step('Prepare sponsorship', 'Request native paymaster data and gas estimate before signing.'), step('Sign in your wallet', 'Authorize the exact prepared Trading Account operation.', 'accent'), step('Submit and verify', 'The paymaster covers eligible network gas. Verify onchain receipt.', 'success')),
      note('First-time Trading Account deployment waits for an Arbitrum Sepolia safe block (up to 30 min). Closes have zero keeper execution reward.')], ['aa', 'currentCloseReward']),
  spec('sponsored-withdrawal-flow', trade('gas-sponsored-trading-and-your-plether-trading-account'), 'Withdraw to your owner wallet', 'MARGIN WITHDRAWAL',
    'Review the free amount and verified owner recipient, prepare sponsorship, sign and confirm. Withdrawal remains subject to onchain account-health and balance checks.', [
      flow(step('Choose an eligible amount', 'Use Withdrawable, not total collateral or unrealized profit.'), step('Verify the recipient', 'Check the Trading Account and displayed owner wallet.'), step('Prepare and sign', 'Authorize the sponsored withdrawal.', 'accent'), step('Confirm receipt', 'After successful execution, check owner-wallet USDG.', 'success')),
      note('The contract rechecks balances and account health. An estimate or wallet signature alone does not complete a withdrawal.')], ['accounts', 'aa']),
  positionFlow('open-increase-position-lifecycle', trade('open-or-increase-a-position'), 'Open or increase a position', 'Choose direction, quantity, leverage and execution protections.', 'Create the position or increase its size and update its backing.'),
  sponsor('open-increase-sponsored-submission', trade('open-or-increase-a-position'), 'Authorize an opening order', 'Opens and increases reserve submitted margin plus a 0.01–0.25 USDG opening execution reward until executed or cleared.'),
  spec('account-health-reading-order', trade('read-your-position-and-account-health'), 'Read account health in context', 'ACCOUNT CHECKLIST',
    'Recommended reading order, not a calculation or state transition: market regime, position, equity and margin, liquidation context, pending obligations and spendable balances.', [
      { type: 'checklist', steps: [step('Market state', 'Check oracle availability and close-only restrictions.'), step('Current position', 'Confirm side, size and entry price.'), step('Equity and margin', 'Compare equity after margin-first carry projection with maintenance. USDG balance is separate.'), step('Liquidation context', 'Treat liquidation price as an estimate, not a guarantee.'), step('Pending obligations', 'Check any uncovered carry, orders, TP/SL and reserved funds.'), step('Spendable balances', 'Available to Trade and Withdrawable have different checks.')] }], ['risk', 'accounts', 'currentCarry']),
  positionFlow('reduce-close-position-lifecycle', trade('reduce-or-close-a-position'), 'Reduce or close a position', 'Choose the reduction and review its price and financial bounds.', 'Reduce or remove the position and apply the close accounting.'),
  sponsor('reduce-close-sponsored-submission', trade('reduce-or-close-a-position'), 'Authorize a closing order', 'Reductions and full closes reserve zero execution reward, checkpoint carry at commit, and leave the position live until executed.'),
  spec('confirmed-order-execution-path', trade('trader-troubleshooting'), 'Confirmed does not mean filled', 'ORDER TROUBLESHOOTING',
    'A successful sponsored commitment creates a pending order. Only a successful order execution applies the requested position change.', [
      flow(step('Commitment confirmed', 'The sponsored operation succeeded onchain.', 'accent'), step('Order pending', 'Wait for queue, oracle and policy eligibility.', 'warning')),
      lanes(lane('Execution succeeds', 'success', step('Position changes', 'Verify the executed order and resulting position.')), lane('Execution does not succeed', 'warning', step('Check the outcome', 'It may remain pending or end in terminal failure.')))], ['orders', 'aa']),
  sponsor('troubleshooting-sponsored-submission', trade('trader-troubleshooting'), 'Check the submission stage', 'If a session drops mid-flight, open Trading Account activity and run Wallet-Verified Recovery before retrying.'),
  spec('sponsorship-vs-order-failure-lifecycles', trade('why-is-my-order-pending-or-failed'), 'Two lifecycles, different failures', 'ORDER TROUBLESHOOTING',
    'Sponsored transaction success and order execution are different lifecycles. Transient oracle, gas, engine or receipt failures can remain pending; only terminal reasons fail the order.', [
      section('01 · Sponsored transaction'), flow(step('Prepare → sign → submit', 'An inclusion receipt must confirm successful execution.', 'accent')),
      note('A revert, dropped operation or unknown outcome is not proof that an order exists. Reconcile its receipt and client order ID.'),
      section('02 · Delayed order'),
      lanes(lane('Still pending', 'warning', step('Temporary blockers', 'Queue, oracle timing, insufficient gas, engine or receipt failure.')), { ...lane('Terminal result', 'danger', step('Executed', 'The requested trade was applied.'), step('Failed', 'Expiry, slippage, config or mode mismatch, risk-off, planner or constraint rejection, liquidation.')), alternatives: true }),
      note('An engine panic is not automatically a terminal order failure. Read the stored order receipt and pending reason.')], ['orders', 'aa']),
  spec('usdg-account-flow', trade('your-margin-account'), 'USDG has distinct places to live', 'ACCOUNT BALANCES',
    'Testnet faucet USDG credits the Margin Account directly; existing wallet USDG deposits through the Trading Account. Free funds, PnL pledge and reserves have distinct permissions.', [
      flow(step('Faucet or owner wallet', 'Get 100,000 USDG credits Margin Account directly; existing wallet USDG uses Transfer & Deposit.'), step('Deposit margin', 'USDG enters the Margin Clearinghouse.', 'accent')),
      { type: 'group', title: 'Internal account balances', items: ['Free USDG — eligible for actions and carry after active margin', 'Position margin — backs price risk and pays carry first', 'Action, order and liquidation reserves'] },
      note('Carry consumes active position margin before free USDG. A deposit need not pass through a trade before withdrawal, but account-health checks still apply.'),
      flow(step('Withdraw eligible free USDG', 'Subject to reserves, carry and account-health checks.'), step('Owner wallet receives USDG', 'After the sponsored withdrawal succeeds.', 'success'))], ['accounts', 'claim', 'currentCarry']),
  spec('claim-to-owner-wallet', trade('your-margin-account'), 'A claim is not wallet cash', 'CLAIM SETTLEMENT',
    'Claim coverage and settlement precede account credit. Live-position credit is pledged, so withdrawal requires eligible free USDG and a separate authorized action.', [
      flow(step('Onchain coverage check', 'Contract verifies pool cash covers all outstanding claims.', 'warning'), step('Authorize settlement', 'Submit Settle Claim from the owning Trading Account.', 'accent')), claimCredit,
      flow(step('Review Withdrawable', 'Only eligible free USDG can leave the account.'), step('Authorize withdrawal', 'A separate operation sends USDG to the owner wallet.', 'success'))], ['claim', 'accounts', 'aa']),
]

export const evidence = {
  index: ['config/perps/arbitrum-sepolia-v2.1.0.json', 'apps/backend/src/Plether/Pyth/Basket.hs:71-164', 'apps/frontend/src/utils/perps.ts:13,110-113'],
  lpEpochs: ['docs/runbooks/lp-settlement-keeper.md:6-10', 'apps/frontend/src/pages/Vaults.tsx:2678-2705,3620,4245-4289'],
  currentCarry: ['https://github.com/Plether-Fi/plether-core/blob/d5ef9725f72f5e131272da27004266d8a3d51e50/packages/perps/src/libraries/MarginClearinghouseAccountingLib.sol', 'https://github.com/Plether-Fi/plether-core/blob/d5ef9725f72f5e131272da27004266d8a3d51e50/packages/perps/src/libraries/PositionRiskAccountingLib.sol#L43-L79'],
  currentCloseFunding: ['https://github.com/Plether-Fi/plether-core/blob/d5ef9725f72f5e131272da27004266d8a3d51e50/packages/perps/src/libraries/CfdEnginePlanLib.sol'],
  currentCloseReward: ['https://github.com/Plether-Fi/plether-core/blob/d5ef9725f72f5e131272da27004266d8a3d51e50/packages/perps/src/CfdEngine.sol', 'https://github.com/Plether-Fi/plether-core/blob/d5ef9725f72f5e131272da27004266d8a3d51e50/packages/perps/src/PositionProtectionBook.sol', 'apps/frontend/src/contracts/preparePerpsOrderV3.ts:closeBounty'],
  calendar: ['packages/perps/src/libraries/MarketCalendarLib.sol:marketStatus,newYorkMarketBoundary'],
  orders: ['packages/perps/src/OrderV3Types.sol:TerminalReason,PendingReason,ExecutionBounds', 'packages/perps/src/OrderRouterExecutionSidecar.sol:executeOrder,executeOrderBatch', 'packages/perps/src/libraries/OrderOraclePolicyLib.sol:getOracleExecutionPolicy'],
  close: ['packages/perps/src/libraries/CfdEnginePlanLib.sol:_planIsolatedCloseSettlement,_planCloseActionSettlement,_maxPricePledgeUnlockPreservingTerminalCap'],
  settlement: ['packages/perps/src/CfdEngineSettlementSidecar.sol:executeClose,_settleCloseActionCharge,_recoveredFrozenSpreadAssets'],
  claim: ['packages/perps/src/CfdEngine.sol:settleTraderClaim,_settleTraderClaimBalance,_payOrRecordTraderClaim,_availableCashForFreshPoolPayouts'],
  risk: ['packages/perps/src/libraries/PositionRiskAccountingLib.sol:computeCurrentCarryIndex,computeIndexedCarryAssets,buildExactPriceRiskState'],
  waterfall: ['packages/perps/src/libraries/HousePoolWaterfallAccountingLib.sol:absorbLoss,distributeRevenue,paySeniorCoupon'],
  accounts: ['packages/perps/src/MarginClearinghouse.sol', 'packages/perps/src/CfdEngineSettlementSidecar.sol:validateWithdraw', 'https://github.com/Plether-Fi/plether-core/blob/e35cf0e0e9b880e07812e36321dee98bd299dcbd/packages/perps-aa-client/src/actions.ts'],
  aa: ['https://github.com/Plether-Fi/plether-core/blob/e35cf0e0e9b880e07812e36321dee98bd299dcbd/packages/perps-aa-client/src/orchestrator.ts', 'apps/frontend/src/utils/sponsoredOperation.ts:sponsoredOperationStatusLabel', 'apps/frontend/src/perps-aa/SponsoredOperationRecovery.tsx'],
}
