import { execFileSync } from 'node:child_process'
import { promises as fs } from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const scriptDirectory = path.dirname(fileURLToPath(import.meta.url))
const gitbookDirectory = path.resolve(scriptDirectory, '..')
const outputDirectory = path.join(gitbookDirectory, '.gitbook/assets/diagrams')
const manifestPath = path.join(outputDirectory, 'diagram-manifest.json')

const COLORS = {
  background: '#250917',
  surface: '#3B212D',
  surfaceDeep: '#2E1220',
  text: '#FFF5F9',
  muted: '#D8CBD0',
  border: '#875465',
  peach: '#FFAB96',
  orange: '#FF572D',
  positive: '#00FF99',
  positiveSurface: '#12372C',
  warning: '#F7D977',
  warningSurface: '#403515',
  dangerSurface: '#471B20',
}

function node(id, label, kind = 'neutral') {
  return { id, label, kind }
}

function edge(from, to, options = {}) {
  return { from, to, ...options }
}

function chainEdges(ids, options = {}) {
  return ids.slice(0, -1).map((id, index) => edge(id, ids[index + 1], options))
}

function linearDiagram({
  sourcePath,
  source,
  filename,
  title,
  alt,
  description,
  labels,
  kinds = [],
  rankdir = labels.length > 5 ? 'TB' : 'LR',
  edgeOptions = {},
  clusters,
}) {
  const nodes = labels.map((label, index) => node(`n${index + 1}`, label, kinds[index] ?? 'neutral'))
  return {
    sourcePath,
    source,
    filename,
    title,
    alt,
    description,
    rankdir,
    nodes,
    edges: chainEdges(nodes.map(({ id }) => id), edgeOptions),
    clusters,
  }
}

const sponsoredLifecycleLabels = [
  'Preparing',
  'Wallet confirmation',
  'Sponsored operation submitted',
  'Pending',
  'Confirmed',
]

function sponsoredLifecycle(meta) {
  return linearDiagram({
    ...meta,
    labels: sponsoredLifecycleLabels,
    kinds: ['warning', 'sponsored', 'sponsored', 'warning', 'success'],
    rankdir: 'TB',
    edgeOptions: { tone: 'sponsored' },
  })
}

const collectionPriorityLabels = [
  'Execution fee',
  'Base close obligation',
  'Frozen-close spread',
]

function collectionPriority(meta) {
  return linearDiagram({
    ...meta,
    labels: collectionPriorityLabels,
    kinds: ['neutral', 'warning', 'danger'],
    rankdir: 'LR',
  })
}

const diagrams = [
  {
    sourcePath: 'how-plether-works/how-orders-execute.md',
    source: `Preview
→ Commit
→ Global FIFO queue
→ Execution-time oracle regime
→ Eligible observation under that regime
→ Price and risk checks
→ Executed or Failed`,
    filename: 'delayed-order-execution-pipeline.svg',
    title: 'Delayed-order execution pipeline',
    alt: 'Flowchart showing a Plether order moving from Preview through Commit, FIFO execution and final execution checks.',
    description: 'A committed order enters the global FIFO queue, uses the execution-time oracle regime and eligible observation, passes price and risk checks, and then executes or fails.',
    rankdir: 'TB',
    nodes: [
      node('preview', 'Preview', 'start'),
      node('commit', 'Commit', 'sponsored'),
      node('fifo', 'Global FIFO queue', 'warning'),
      node('regime', 'Execution-time oracle regime'),
      node('observation', 'Eligible observation under that regime'),
      node('checks', 'Price and risk checks', 'decision'),
      node('executed', 'Executed', 'success'),
      node('failed', 'Failed', 'danger'),
    ],
    edges: [
      edge('preview', 'commit'),
      edge('commit', 'fifo'),
      edge('fifo', 'regime'),
      edge('regime', 'observation'),
      edge('observation', 'checks'),
      edge('checks', 'executed', { label: 'Pass', tone: 'positive' }),
      edge('checks', 'failed', { label: 'Reject', tone: 'danger' }),
    ],
    sameRanks: [['executed', 'failed']],
  },
  sponsoredLifecycle({
    sourcePath: 'how-plether-works/how-orders-execute.md',
    source: `Preparing
→ Wallet confirmation
→ Sponsored operation submitted
→ Pending
→ Confirmed`,
    filename: 'order-sponsored-submission-lifecycle.svg',
    title: 'Sponsored submission lifecycle',
    alt: 'Five-stage sponsored submission lifecycle from Preparing through wallet confirmation to Confirmed.',
    description: 'The application prepares the operation, the owner wallet confirms it, Plether submits the eligible sponsored operation, and inclusion progresses from Pending to Confirmed.',
  }),
  linearDiagram({
    sourcePath: 'how-plether-works/how-orders-execute.md',
    source: 'Preview → Commit → Finalize',
    filename: 'preview-commit-finalize.svg',
    title: 'Two-part order lifecycle',
    alt: 'Three-stage order lifecycle: Preview, Commit and Finalize.',
    description: 'Plether separates reviewing an order, committing it to the queue, and finalizing its execution.',
    labels: ['Preview', 'Commit', 'Finalize'],
    kinds: ['start', 'sponsored', 'success'],
    rankdir: 'LR',
  }),
  collectionPriority({
    sourcePath: 'how-plether-works/how-orders-execute.md',
    source: `Execution fee
→ base close obligation
→ frozen-close spread`,
    filename: 'final-collection-priority.svg',
    title: 'Final collection priority',
    alt: 'Collection priority from execution fee to base close obligation and frozen-close spread.',
    description: 'When reachable collateral is limited, Plether collects the execution fee first, the base close obligation second, and any frozen-close spread last.',
  }),
  {
    sourcePath: 'how-plether-works/how-pnl-is-calculated.md',
    sourceLanguage: 'mermaid',
    source: `flowchart TD
    A["Entry price + current mark + position quantity"] --> B["Unrealized price PnL"]
    B --> C["Account equity and liquidation health"]

    D["Entry price + close execution price + closed quantity"] --> E["Realized price PnL"]
    E --> F["Apply VPI, execution fee, accrued carry and any frozen-close spread"]
    F --> G["Net close settlement"]
    G --> H{"Positive or negative?"}
    H -->|"Positive"| I{"Full payout cash available after existing claims?"}
    I -->|"Available"| J["Credit Margin Account"]
    I -->|"Unavailable"| K["Record trader claim"]
    H -->|"Negative"| L["Collect reachable account collateral"]
    L --> M["For a terminal full-close shortfall, net the same account's trader claim"]
    M --> N["Then record only base-obligation bad debt; waive uncollectible frozen spread"]`,
    filename: 'pnl-and-close-settlement-outcomes.svg',
    title: 'PnL and close-settlement outcomes',
    alt: 'Flowchart separating unrealized PnL and liquidation health from realized PnL and close-settlement outcomes.',
    description: 'Unrealized PnL affects account health. A close realizes PnL, applies trading costs, and routes the result to Margin Account credit, a trader claim, collateral and same-account-claim collection, or base-obligation bad debt.',
    rankdir: 'TB',
    nodes: [
      node('openInputs', 'Entry price + current mark + position quantity', 'start'),
      node('unrealized', 'Unrealized price PnL'),
      node('health', 'Account equity and liquidation health', 'warning'),
      node('closeInputs', 'Entry price + close execution price + closed quantity', 'start'),
      node('realized', 'Realized price PnL'),
      node('costs', 'Apply VPI, execution fee, accrued carry and any frozen-close spread'),
      node('net', 'Net close settlement'),
      node('sign', 'Positive or negative?', 'decision'),
      node('cash', 'Full payout cash available after existing claims?', 'decision'),
      node('credit', 'Credit Margin Account', 'success'),
      node('claim', 'Record trader claim', 'warning'),
      node('collateral', 'Collect reachable account collateral', 'danger'),
      node('claimNetting', "For a terminal full-close shortfall, net the same account's trader claim", 'warning'),
      node('badDebt', 'Then record only base-obligation bad debt; waive uncollectible frozen spread', 'danger'),
    ],
    edges: [
      edge('openInputs', 'unrealized'),
      edge('unrealized', 'health'),
      edge('closeInputs', 'realized'),
      edge('realized', 'costs'),
      edge('costs', 'net'),
      edge('net', 'sign'),
      edge('sign', 'cash', { label: 'Positive', tone: 'positive' }),
      edge('cash', 'credit', { label: 'Available', tone: 'positive' }),
      edge('cash', 'claim', { label: 'Unavailable', tone: 'warning' }),
      edge('sign', 'collateral', { label: 'Negative', tone: 'danger' }),
      edge('collateral', 'claimNetting', { label: 'Terminal shortfall', tone: 'danger' }),
      edge('claimNetting', 'badDebt', { label: 'Shortfall remains', tone: 'danger' }),
      edge('health', 'closeInputs', { style: 'invis' }),
    ],
    clusters: [
      { id: 'open', label: 'OPEN POSITION', nodes: ['openInputs', 'unrealized', 'health'] },
      { id: 'close', label: 'CLOSE SETTLEMENT', nodes: ['closeInputs', 'realized', 'costs', 'net', 'sign', 'cash', 'credit', 'claim', 'collateral', 'claimNetting', 'badDebt'] },
    ],
  },
  linearDiagram({
    sourcePath: 'how-plether-works/market-states-and-oracle-closures.md',
    source: `OPEN
    Sunday 22:00 → Friday 19:00

CLOSE-ONLY · LIVE ORACLE
    Friday 19:00 → Friday 22:00

CLOSE-ONLY · ORACLE FROZEN
    Friday 22:00 → Sunday 21:00

CLOSE-ONLY · LIVE ORACLE
    Sunday 21:00 → Sunday 22:00

OPEN
    From Sunday 22:00`,
    filename: 'weekly-market-state-schedule.svg',
    title: 'Weekly market-state schedule',
    alt: 'Weekly timeline showing open, close-only live-oracle and close-only frozen-oracle periods.',
    description: 'The recurring schedule moves from the open market into live-oracle close-only, frozen-oracle close-only, a final live-oracle hour, and then reopens.',
    labels: [
      'OPEN\nSunday 22:00–Friday 19:00',
      'CLOSE-ONLY · LIVE ORACLE\nFriday 19:00–22:00',
      'CLOSE-ONLY · ORACLE FROZEN\nFriday 22:00–Sunday 21:00',
      'CLOSE-ONLY · LIVE ORACLE\nSunday 21:00–22:00',
      'OPEN\nFrom Sunday 22:00',
    ],
    kinds: ['success', 'warning', 'danger', 'warning', 'success'],
    rankdir: 'TB',
  }),
  collectionPriority({
    sourcePath: 'how-plether-works/market-states-and-oracle-closures.md',
    source: `Execution fee
→ Base close obligation
→ Frozen-close spread`,
    filename: 'frozen-close-collection-priority.svg',
    title: 'Frozen-close collection priority',
    alt: 'Frozen-close collection priority from execution fee through base obligation to frozen-close spread.',
    description: 'The frozen-market voluntary-close path preserves the execution fee first, then the base close obligation, and finally the frozen-close spread.',
  }),
  {
    sourcePath: 'how-plether-works/settlement-liquidity-and-trader-claims.md',
    sourceLanguage: 'mermaid',
    source: `flowchart TD
    A[Close executes] --> B[Release margin assigned to the closed portion]
    A --> C[Calculate net close settlement]

    C -->|Positive| D{Can the liquidity pool fund the full amount?}
    D -->|Yes| E[Credit the full amount to the Margin Account]
    D -->|No| F[Record the full amount as a trader claim]

    F --> G[Settle later when aggregate claims are fully covered]
    G --> E
    E --> H[Withdraw subject to normal account checks]

    C -->|Zero or negative| I[Collect reachable collateral in priority order]
    I --> J[For a terminal full-close shortfall, net any same-account trader claim]
    J --> K[Then record only base-obligation bad debt and waive uncollectible frozen spread]`,
    filename: 'settlement-liquidity-flow.svg',
    title: 'Close settlement and liquidity flow',
    alt: 'Flowchart showing margin release, positive close settlement, liquidity pool funding, trader claims and zero-or-negative settlement.',
    description: 'A close releases assigned margin and calculates settlement. Positive settlement becomes Margin Account credit or a trader claim depending on pool cash after existing claims; zero or negative settlement collects reachable collateral, nets a same-account claim, and records only genuine base-obligation bad debt.',
    rankdir: 'TB',
    nodes: [
      node('close', 'Close executes', 'start'),
      node('release', 'Release margin assigned to the closed portion', 'success'),
      node('calculate', 'Calculate net close settlement'),
      node('funding', 'Can the liquidity pool fund the full amount after existing claims?', 'decision'),
      node('credit', 'Credit the full amount to the Margin Account', 'success'),
      node('claim', 'Record the full amount as a trader claim', 'warning'),
      node('later', 'Settle later when aggregate claims are fully covered', 'sponsored'),
      node('withdraw', 'Withdraw subject to normal account checks', 'success'),
      node('collect', 'Collect reachable collateral in priority order', 'danger'),
      node('claimNetting', 'For a terminal full-close shortfall, net any same-account trader claim', 'warning'),
      node('shortfall', 'Then record only base-obligation bad debt and waive uncollectible frozen spread', 'danger'),
    ],
    edges: [
      edge('close', 'release'),
      edge('close', 'calculate'),
      edge('calculate', 'funding', { label: 'Positive', tone: 'positive' }),
      edge('funding', 'credit', { label: 'Yes', tone: 'positive' }),
      edge('funding', 'claim', { label: 'No', tone: 'warning' }),
      edge('claim', 'later', { style: 'dashed', tone: 'warning' }),
      edge('later', 'credit', { tone: 'positive' }),
      edge('credit', 'withdraw', { tone: 'positive' }),
      edge('calculate', 'collect', { label: 'Zero or negative', tone: 'danger' }),
      edge('collect', 'claimNetting', { label: 'Terminal shortfall', tone: 'danger' }),
      edge('claimNetting', 'shortfall', { label: 'Shortfall remains', tone: 'danger' }),
    ],
    sameRanks: [['release', 'calculate']],
  },
  collectionPriority({
    sourcePath: 'how-plether-works/settlement-liquidity-and-trader-claims.md',
    source: `Execution fee
→ base close obligation
→ frozen-close spread`,
    filename: 'claim-collateral-collection-order.svg',
    title: 'Claim and collateral collection order',
    alt: 'Collection order from execution fee to base close obligation and frozen-close spread.',
    description: 'A trader claim is not collateral. Reachable collateral is applied to the execution fee, base close obligation and frozen-close spread in that order.',
  }),
  {
    sourcePath: 'how-plether-works/the-liquidity-pool-and-tranche-waterfall.md',
    sourceLanguage: 'mermaid',
    source: `flowchart TD
    A["Liquidity pool value after trader liabilities"] --> B{"Revenue or loss?"}

    B -->|"Loss"| C["Junior absorbs first"]
    C --> D["Senior absorbs only the remainder"]

    B -->|"Revenue"| E["Restore impaired Senior to its high-water mark"]
    E --> F["Residual revenue goes to Junior"]`,
    filename: 'liquidity-pool-tranche-waterfall.svg',
    title: 'Liquidity pool tranche waterfall',
    alt: 'Flowchart showing losses flowing through Junior before Senior and revenue restoring Senior before reaching Junior.',
    description: 'After trader liabilities, pool losses are absorbed by Junior and then Senior, while revenue first restores impaired Senior capital and then belongs to Junior.',
    rankdir: 'TB',
    nodes: [
      node('value', 'Liquidity pool value after trader liabilities', 'pool'),
      node('outcome', 'Revenue or loss?', 'decision'),
      node('juniorLoss', 'Junior absorbs first', 'danger'),
      node('seniorLoss', 'Senior absorbs only the remainder', 'danger'),
      node('restore', 'Restore impaired Senior to its high-water mark', 'warning'),
      node('juniorRevenue', 'Residual revenue goes to Junior', 'success'),
    ],
    edges: [
      edge('value', 'outcome'),
      edge('outcome', 'juniorLoss', { label: 'Loss', tone: 'danger' }),
      edge('juniorLoss', 'seniorLoss', { tone: 'danger' }),
      edge('outcome', 'restore', { label: 'Revenue', tone: 'positive' }),
      edge('restore', 'juniorRevenue', { tone: 'positive' }),
    ],
    sameRanks: [['juniorLoss', 'restore'], ['seniorLoss', 'juniorRevenue']],
  },
  {
    sourcePath: 'how-plether-works/trading-costs-fees-carry-and-vpi.md',
    source: `Oracle price → entry, exit and directional PnL

Execution fee + VPI + carry
+ frozen-close spread when applicable
→ account settlement`,
    filename: 'trading-price-and-settlement-costs.svg',
    title: 'Trading price and settlement costs',
    alt: 'Two-input flowchart showing oracle price determining directional PnL while fees, VPI, carry and frozen spread feed account settlement.',
    description: 'Oracle price determines entry, exit and directional PnL. Execution fee, VPI, carry and any frozen-close spread are separate USDC inputs to account settlement.',
    rankdir: 'LR',
    nodes: [
      node('oracle', 'Oracle price', 'start'),
      node('pnl', 'Entry, exit and directional PnL'),
      node('costs', 'Execution fee + VPI + carry + frozen-close spread when applicable', 'warning'),
      node('settlement', 'Account settlement', 'account'),
    ],
    edges: [
      edge('oracle', 'pnl'),
      edge('pnl', 'settlement'),
      edge('costs', 'settlement', { tone: 'warning' }),
    ],
    sameRanks: [['pnl', 'costs']],
  },
  collectionPriority({
    sourcePath: 'how-plether-works/trading-costs-fees-carry-and-vpi.md',
    source: `Protocol execution fee
→ Base close obligation
→ Frozen-close spread`,
    filename: 'protocol-close-collection-order.svg',
    title: 'Protocol close-collection order',
    alt: 'Protocol collection order from execution fee to base close obligation and frozen-close spread.',
    description: 'The protocol applies reachable value to the execution fee first, the base close obligation second, and the frozen-close spread last.',
  }),
  linearDiagram({
    sourcePath: 'how-plether-works/trading-costs-fees-carry-and-vpi.md',
    source: `Higher assigned position margin
→ lower LP-backed borrow base
→ lower future carry`,
    filename: 'margin-reduces-future-carry.svg',
    title: 'How assigned margin reduces carry',
    alt: 'Flow showing higher assigned position margin reducing the LP-backed borrow base and future carry.',
    description: 'Assigning more position margin lowers the LP-backed portion of the position and therefore lowers future carry.',
    labels: ['Higher assigned position margin', 'Lower LP-backed borrow base', 'Lower future carry'],
    kinds: ['start', 'neutral', 'success'],
    rankdir: 'LR',
  }),
  {
    sourcePath: 'liquidity-provider-quickstart.md',
    source: `Losses:       Junior first → Senior second
New revenue:  Restore impaired Senior → Junior receives the residual
Coupon:       Junior NAV → Senior`,
    filename: 'senior-junior-waterfall-rules.svg',
    title: 'Senior–Junior waterfall rules',
    alt: 'Three-lane diagram summarizing loss absorption, new-revenue allocation and Senior coupon funding.',
    description: 'Losses reach Junior before Senior, new revenue restores impaired Senior before reaching Junior, and the Senior coupon is funded from Junior NAV.',
    rankdir: 'TB',
    nodes: [
      node('lossJunior', 'Junior absorbs losses first', 'danger'),
      node('lossSenior', 'Senior absorbs the remainder', 'danger'),
      node('revenueSenior', 'Restore impaired Senior', 'warning'),
      node('revenueJunior', 'Junior receives residual revenue', 'success'),
      node('couponJunior', 'Junior NAV funds coupon', 'warning'),
      node('couponSenior', 'Senior receives coupon', 'success'),
    ],
    edges: [
      edge('lossJunior', 'lossSenior', { label: 'LOSSES', tone: 'danger' }),
      edge('revenueSenior', 'revenueJunior', { label: 'NEW REVENUE', tone: 'positive' }),
      edge('couponJunior', 'couponSenior', { label: 'COUPON', tone: 'warning' }),
      edge('lossJunior', 'revenueSenior', { style: 'invis' }),
      edge('revenueSenior', 'couponJunior', { style: 'invis' }),
    ],
    sameRanks: [
      ['lossJunior', 'lossSenior'],
      ['revenueSenior', 'revenueJunior'],
      ['couponJunior', 'couponSenior'],
    ],
  },
  sponsoredLifecycle({
    sourcePath: 'trader-quickstart.md',
    source: `Preparing
→ Wallet confirmation
→ Sponsored operation submitted
→ Pending
→ Confirmed`,
    filename: 'quickstart-sponsored-operation-lifecycle.svg',
    title: 'Quickstart sponsored-operation lifecycle',
    alt: 'Quickstart lifecycle from Preparing and wallet confirmation through sponsored submission to Confirmed.',
    description: 'The quickstart submission progresses from preparation and wallet authorization through sponsored submission, Pending inclusion and confirmation.',
  }),
  linearDiagram({
    sourcePath: 'trading-on-plether-perps/check-and-settle-a-trader-claim.md',
    source: `Position settles
→ Fresh payout cannot be funded in full
→ Trader claim is recorded
→ Aggregate pool cash coverage returns
→ Owner wallet authorizes settlement
→ Sponsored Trading Account operation confirms
→ USDC is credited to the Margin Account
→ Optional withdrawal to the owner wallet`,
    filename: 'trader-claim-lifecycle.svg',
    title: 'Trader-claim lifecycle',
    alt: 'Trader-claim lifecycle from an underfunded payout through later sponsored settlement and optional withdrawal.',
    description: 'A complete fresh payout that cannot be funded immediately is recorded in full as a claim. Once pool coverage returns, the owner authorizes sponsored settlement to the Margin Account and may then withdraw.',
    labels: [
      'Position settles',
      'Fresh payout cannot be funded in full',
      'Trader claim is recorded',
      'Aggregate pool cash coverage returns',
      'Owner wallet authorizes settlement',
      'Sponsored Trading Account operation confirms',
      'USDC is credited to the Margin Account',
      'Optional withdrawal to the owner wallet',
    ],
    kinds: ['start', 'warning', 'warning', 'success', 'sponsored', 'sponsored', 'success', 'success'],
    rankdir: 'TB',
  }),
  linearDiagram({
    sourcePath: 'trading-on-plether-perps/check-and-settle-a-trader-claim.md',
    source: `Liquidity pool
→ Margin Clearinghouse
→ Your Margin Account`,
    filename: 'claim-settlement-funding-path.svg',
    title: 'Claim-settlement funding path',
    alt: 'Funding path from the liquidity pool through the Margin Clearinghouse to the trader Margin Account.',
    description: 'Claim settlement moves covered pool cash through the Margin Clearinghouse and credits the claim-owning Trading Account’s Margin Account.',
    labels: ['Liquidity pool', 'Margin Clearinghouse', 'Your Margin Account'],
    kinds: ['pool', 'neutral', 'account'],
    rankdir: 'LR',
  }),
  {
    sourcePath: 'trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md',
    source: `Connected wallet
      │ signs and controls
      ▼
Trading Account
      │ owns
      ▼
Positions · Orders · Margin Account · Trader claims`,
    filename: 'wallet-trading-account-ownership.svg',
    title: 'Wallet authorization and Trading Account ownership',
    alt: 'Hierarchy showing the connected wallet signing for and controlling the Trading Account, which owns positions, orders, margin and trader claims.',
    description: 'The connected owner wallet provides signatures and controls the Trading Account. The Trading Account owns the protocol positions, orders, Margin Account and trader claims.',
    rankdir: 'TB',
    nodes: [
      node('wallet', 'Connected wallet', 'start'),
      node('account', 'Trading Account', 'account'),
      node('state', 'Positions · Orders · Margin Account · Trader claims', 'neutral'),
    ],
    edges: [
      edge('wallet', 'account', { label: 'Signs and controls', style: 'dashed', tone: 'sponsored' }),
      edge('account', 'state', { label: 'Owns', tone: 'sponsored' }),
    ],
  },
  linearDiagram({
    sourcePath: 'trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md',
    source: `You review and sign
        ↓
Plether checks sponsorship eligibility
        ↓
The authorized operation is submitted
        ↓
Plether pays the eligible network gas`,
    filename: 'authorization-and-gas-sponsorship.svg',
    title: 'Authorization and gas sponsorship',
    alt: 'Sequence showing the user reviewing and signing, Plether checking eligibility, submitting the authorized operation and paying eligible network gas.',
    description: 'The owner authorizes the exact action. Plether separately evaluates sponsorship eligibility, submits the authorized operation and pays eligible network gas.',
    labels: [
      'You review and sign',
      'Plether checks sponsorship eligibility',
      'The authorized operation is submitted',
      'Plether pays the eligible network gas',
    ],
    kinds: ['start', 'warning', 'sponsored', 'success'],
    rankdir: 'TB',
    edgeOptions: { style: 'dashed', tone: 'sponsored' },
  }),
  linearDiagram({
    sourcePath: 'trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md',
    source: `Choose amount
→ Review Trading Account and recipient
→ Sign withdrawal
→ Sponsored operation confirms
→ USDC reaches the displayed wallet`,
    filename: 'sponsored-withdrawal-flow.svg',
    title: 'Sponsored withdrawal flow',
    alt: 'Sponsored withdrawal sequence from amount selection and recipient review to owner-wallet receipt.',
    description: 'The trader chooses an amount, verifies the Trading Account and recipient, signs the withdrawal, and receives USDC at the verified owner wallet after the sponsored operation confirms.',
    labels: [
      'Choose amount',
      'Review Trading Account and recipient',
      'Sign withdrawal',
      'Sponsored operation confirms',
      'USDC reaches the verified owner wallet',
    ],
    kinds: ['start', 'neutral', 'sponsored', 'sponsored', 'success'],
    rankdir: 'TB',
  }),
  linearDiagram({
    sourcePath: 'trading-on-plether-perps/open-or-increase-a-position.md',
    source: `Configure
→ Review
→ Preparing
→ Wallet confirmation
→ Sponsored operation submitted
→ Pending
→ Confirmed
→ Order Pending in FIFO with funds reserved
→ Executed or failed
→ Position updated`,
    filename: 'open-increase-position-lifecycle.svg',
    title: 'Open or increase position lifecycle',
    alt: 'Complete open-or-increase lifecycle from configuration through sponsored submission, FIFO execution and position update.',
    description: 'The trader configures and reviews the order, authorizes sponsored commitment, waits for confirmation and FIFO execution, and then sees the position update if execution succeeds.',
    labels: [
      'Configure',
      'Review',
      'Preparing',
      'Wallet confirmation',
      'Sponsored operation submitted',
      'Pending',
      'Sponsored operation confirmed',
      'Order Pending in FIFO with funds reserved',
      'Executed or failed',
      'Position updated',
    ],
    kinds: ['start', 'neutral', 'warning', 'sponsored', 'sponsored', 'warning', 'success', 'warning', 'neutral', 'success'],
    rankdir: 'TB',
    clusters: [
      { id: 'setup', label: 'ORDER SETUP', nodes: ['n1', 'n2'], sameRanks: [['n1', 'n2']] },
      {
        id: 'submission',
        label: 'SPONSORED SUBMISSION',
        nodes: ['n3', 'n4', 'n5', 'n6', 'n7'],
        sameRanks: [['n3', 'n4'], ['n5', 'n6']],
      },
      {
        id: 'execution',
        label: 'DELAYED EXECUTION',
        nodes: ['n8', 'n9', 'n10'],
        sameRanks: [['n8', 'n9']],
      },
    ],
  }),
  sponsoredLifecycle({
    sourcePath: 'trading-on-plether-perps/open-or-increase-a-position.md',
    source: `Preparing
→ Wallet confirmation
→ Sponsored operation submitted
→ Pending
→ Confirmed`,
    filename: 'open-increase-sponsored-submission.svg',
    title: 'Open-or-increase sponsored submission',
    alt: 'Open-or-increase sponsored submission states from Preparing to Confirmed.',
    description: 'The opening or increase commitment moves through preparation, wallet authorization, sponsored submission, Pending inclusion and confirmation.',
  }),
  linearDiagram({
    sourcePath: 'trading-on-plether-perps/read-your-position-and-account-health.md',
    source: `Market state
→ Current Position
→ Portfolio value
→ Maintenance margin
→ Liquidation price
→ Pending orders
→ Available to Trade and Withdrawable`,
    filename: 'account-health-reading-order.svg',
    title: 'Reading position and account health',
    alt: 'Recommended reading order from market state through position, risk, pending orders and available balances.',
    description: 'Read market state first, then the current position and equity metrics, followed by pending orders and the balances available for trading or withdrawal.',
    labels: [
      'Market state',
      'Current Position',
      'Portfolio value',
      'Maintenance margin',
      'Liquidation price',
      'Pending orders',
      'Available to Trade and Withdrawable',
    ],
    kinds: ['warning', 'start', 'neutral', 'warning', 'danger', 'warning', 'success'],
    rankdir: 'TB',
  }),
  linearDiagram({
    sourcePath: 'trading-on-plether-perps/reduce-or-close-a-position.md',
    source: `Choose the amount
→ Set an acceptable price
→ Review
→ Preparing
→ Wallet confirmation
→ Sponsored operation submitted
→ Pending
→ Confirmed
→ Join the FIFO queue
→ Execute or fail
→ Update the position and Margin Account`,
    filename: 'reduce-close-position-lifecycle.svg',
    title: 'Reduce or close position lifecycle',
    alt: 'Complete reduce-or-close lifecycle from amount selection through sponsored submission, FIFO execution and account update.',
    description: 'The trader chooses a reduction, sets price protection, authorizes the sponsored commitment, waits for FIFO execution, and then sees the position and Margin Account update.',
    labels: [
      'Choose the amount',
      'Set an acceptable price',
      'Review',
      'Preparing',
      'Wallet confirmation',
      'Sponsored operation submitted',
      'Pending',
      'Sponsored operation confirmed',
      'Join the FIFO queue',
      'Execute or fail',
      'Update the position and Margin Account',
    ],
    kinds: ['start', 'neutral', 'neutral', 'warning', 'sponsored', 'sponsored', 'warning', 'success', 'warning', 'neutral', 'success'],
    rankdir: 'TB',
    clusters: [
      { id: 'setup', label: 'CLOSE SETUP', nodes: ['n1', 'n2', 'n3'], sameRanks: [['n1', 'n2']] },
      {
        id: 'submission',
        label: 'SPONSORED SUBMISSION',
        nodes: ['n4', 'n5', 'n6', 'n7', 'n8'],
        sameRanks: [['n4', 'n5'], ['n6', 'n7']],
      },
      {
        id: 'execution',
        label: 'DELAYED EXECUTION',
        nodes: ['n9', 'n10', 'n11'],
        sameRanks: [['n9', 'n10']],
      },
    ],
  }),
  sponsoredLifecycle({
    sourcePath: 'trading-on-plether-perps/reduce-or-close-a-position.md',
    source: `Preparing
→ Wallet confirmation
→ Sponsored operation submitted
→ Pending
→ Confirmed`,
    filename: 'reduce-close-sponsored-submission.svg',
    title: 'Reduce-or-close sponsored submission',
    alt: 'Reduce-or-close sponsored submission states from Preparing to Confirmed.',
    description: 'The reduction or close commitment moves through preparation, wallet authorization, sponsored submission, Pending inclusion and confirmation.',
  }),
  linearDiagram({
    sourcePath: 'trading-on-plether-perps/trader-troubleshooting.md',
    source: `Sponsored operation confirmed
→ Order becomes Pending
→ Order reaches execution
→ Position changes`,
    filename: 'confirmed-order-execution-path.svg',
    title: 'From sponsored confirmation to position change',
    alt: 'Sequence from sponsored operation confirmation through pending order execution to a changed position.',
    description: 'Confirmation of the sponsored commitment creates a pending delayed order. The position changes only after that order reaches execution.',
    labels: ['Sponsored operation confirmed', 'Order becomes Pending', 'Order reaches execution', 'Position changes'],
    kinds: ['sponsored', 'warning', 'warning', 'success'],
    rankdir: 'TB',
  }),
  sponsoredLifecycle({
    sourcePath: 'trading-on-plether-perps/trader-troubleshooting.md',
    source: `Preparing
→ Wallet confirmation
→ Sponsored operation submitted
→ Pending
→ Confirmed`,
    filename: 'troubleshooting-sponsored-submission.svg',
    title: 'Sponsored-operation troubleshooting states',
    alt: 'Sponsored-operation troubleshooting lifecycle from Preparing through wallet confirmation to Confirmed.',
    description: 'These five states isolate submission problems before the separate delayed-order execution lifecycle begins.',
  }),
  {
    sourcePath: 'trading-on-plether-perps/why-is-my-order-pending-or-failed.md',
    source: `Sponsored submission
Preparing
→ Wallet confirmation
→ Sponsored operation submitted
→ Pending
→ Confirmed

Delayed order
Confirmed commitment
→ Order Pending
   ├─ Executed
   └─ Failed
      ├─ Expired and cleaned up
      ├─ Slippage exceeded
      ├─ Engine rejected
      ├─ Account liquidated
      └─ Engine panic`,
    filename: 'sponsorship-vs-order-failure-lifecycles.svg',
    title: 'Submission failures versus delayed-order outcomes',
    alt: 'Two-lane flowchart separating sponsored submission states from delayed-order execution and failure outcomes.',
    description: 'Sponsored submission ends at a confirmed commitment. The separate delayed-order lifecycle then produces execution or a terminal failure such as expiry, slippage, engine rejection, liquidation or panic.',
    rankdir: 'TB',
    nodes: [
      node('prepare', 'Preparing', 'warning'),
      node('wallet', 'Wallet confirmation', 'sponsored'),
      node('submit', 'Sponsored operation submitted', 'sponsored'),
      node('pendingOp', 'Pending', 'warning'),
      node('confirmed', 'Confirmed commitment', 'success'),
      node('orderPending', 'Order Pending', 'warning'),
      node('executed', 'Executed', 'success'),
      node('failed', 'Failed', 'danger'),
      node(
        'reasons',
        'Terminal failure reasons\nExpired and cleaned up\nSlippage exceeded\nEngine rejected\nAccount liquidated\nEngine panic',
        'danger'
      ),
    ],
    edges: [
      edge('prepare', 'wallet', { tone: 'sponsored' }),
      edge('wallet', 'submit', { tone: 'sponsored' }),
      edge('submit', 'pendingOp', { tone: 'sponsored' }),
      edge('pendingOp', 'confirmed', { tone: 'positive' }),
      edge('confirmed', 'orderPending'),
      edge('orderPending', 'executed', { tone: 'positive' }),
      edge('orderPending', 'failed', { tone: 'danger' }),
      edge('failed', 'reasons', { tone: 'danger' }),
    ],
    clusters: [
      { id: 'submission', label: 'SPONSORED SUBMISSION', nodes: ['prepare', 'wallet', 'submit', 'pendingOp', 'confirmed'] },
      {
        id: 'order',
        label: 'DELAYED ORDER',
        nodes: ['orderPending', 'executed', 'failed', 'reasons'],
        sameRanks: [['executed', 'failed']],
      },
    ],
  },
  {
    sourcePath: 'trading-on-plether-perps/your-margin-account.md',
    source: `Trading Account USDC, funded by the testnet faucet or a direct token transfer
→ Margin Account USDC
→ available, assigned or reserved USDC
→ trade settlement
→ withdrawable USDC
→ owner wallet`,
    filename: 'usdc-account-flow.svg',
    title: 'USDC flow across trader accounts',
    alt: 'USDC lifecycle from the funded Trading Account through Margin Account balances, settlement and withdrawal to the owner wallet.',
    description: 'On the current deployment, funded Trading Account USDC moves into Plether’s Margin Account buckets, through trade settlement and to the verified owner wallet when withdrawable.',
    rankdir: 'TB',
    nodes: [
      node('trading', 'Trading Account USDC\nTestnet faucet or direct token transfer', 'start'),
      node('margin', 'Margin Account USDC', 'account'),
      node('buckets', 'Available, assigned or reserved USDC'),
      node('settlement', 'Trade settlement', 'warning'),
      node('withdrawable', 'Withdrawable USDC', 'success'),
      node('ownerEnd', 'Owner wallet', 'success'),
    ],
    edges: chainEdges(['trading', 'margin', 'buckets', 'settlement', 'withdrawable', 'ownerEnd']),
  },
  linearDiagram({
    sourcePath: 'trading-on-plether-perps/your-margin-account.md',
    source: `Trader claim
→ Owner wallet authorizes Settle Claim
→ Sponsored settlement confirms
→ Margin Account USDC
→ Sponsored withdrawal
→ Owner wallet`,
    filename: 'claim-to-owner-wallet.svg',
    title: 'From trader claim to owner wallet',
    alt: 'Sequence from a trader claim through owner authorization, sponsored settlement, Margin Account credit and sponsored withdrawal.',
    description: 'The claim-owning Trading Account receives Margin Account credit after owner authorization and sponsored settlement; withdrawal to the owner wallet is a separate sponsored action.',
    labels: [
      'Trader claim',
      'Owner wallet authorizes Settle Claim',
      'Sponsored settlement confirms',
      'Margin Account USDC',
      'Sponsored withdrawal',
      'Owner wallet',
    ],
    kinds: ['warning', 'sponsored', 'sponsored', 'account', 'sponsored', 'success'],
    rankdir: 'TB',
  }),
]

const NODE_STYLES = {
  neutral: {
    shape: 'box',
    fillcolor: COLORS.surface,
    color: COLORS.border,
    fontcolor: COLORS.text,
  },
  start: {
    shape: 'box',
    fillcolor: COLORS.peach,
    color: COLORS.peach,
    fontcolor: COLORS.background,
  },
  sponsored: {
    shape: 'box',
    fillcolor: COLORS.surfaceDeep,
    color: COLORS.peach,
    fontcolor: COLORS.text,
  },
  success: {
    shape: 'box',
    fillcolor: COLORS.positiveSurface,
    color: COLORS.positive,
    fontcolor: COLORS.text,
  },
  warning: {
    shape: 'box',
    fillcolor: COLORS.warningSurface,
    color: COLORS.warning,
    fontcolor: COLORS.text,
  },
  danger: {
    shape: 'box',
    fillcolor: COLORS.dangerSurface,
    color: COLORS.orange,
    fontcolor: COLORS.text,
  },
  decision: {
    shape: 'diamond',
    fillcolor: COLORS.surface,
    color: COLORS.warning,
    fontcolor: COLORS.text,
  },
  account: {
    shape: 'box',
    fillcolor: COLORS.surfaceDeep,
    color: COLORS.peach,
    fontcolor: COLORS.text,
    peripheries: 2,
  },
  pool: {
    shape: 'cylinder',
    fillcolor: COLORS.surface,
    color: COLORS.peach,
    fontcolor: COLORS.text,
  },
}

const EDGE_STYLES = {
  neutral: { color: COLORS.muted, fontcolor: COLORS.muted },
  sponsored: { color: COLORS.peach, fontcolor: COLORS.peach },
  positive: { color: COLORS.positive, fontcolor: COLORS.positive },
  warning: { color: COLORS.warning, fontcolor: COLORS.warning },
  danger: { color: COLORS.orange, fontcolor: COLORS.orange },
}

function escapeDot(value) {
  return value.replaceAll('"', '\\"')
}

function escapeXml(value) {
  return value
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&apos;')
}

function wrapText(value, maxCharacters = 30) {
  return value
    .split('\n')
    .flatMap((paragraph) => {
      const words = paragraph.split(/\s+/).filter(Boolean)
      if (words.length === 0) return ['']

      const lines = []
      let current = words[0]
      for (const word of words.slice(1)) {
        if (`${current} ${word}`.length <= maxCharacters) {
          current += ` ${word}`
        } else {
          lines.push(current)
          current = word
        }
      }
      lines.push(current)
      return lines
    })
    .join('\\n')
}

function dotAttributes(attributes) {
  return Object.entries(attributes)
    .filter(([, value]) => value !== undefined)
    .map(([key, value]) => `${key}="${escapeDot(String(value))}"`)
    .join(', ')
}

function buildDot(specification) {
  const lines = [
    'digraph diagram {',
    `  graph [${dotAttributes({
      bgcolor: COLORS.background,
      color: COLORS.border,
      fontcolor: COLORS.text,
      fontname: 'Arial',
      fontsize: 22,
      label: specification.title,
      labeljust: 'l',
      labelloc: 't',
      margin: 0,
      nodesep: 0.48,
      pad: 0.38,
      rankdir: specification.rankdir ?? 'LR',
      ranksep: 0.72,
      splines: 'polyline',
    })}];`,
    `  node [${dotAttributes({
      fontname: 'Arial',
      fontsize: 15,
      height: 0.72,
      margin: '0.20,0.14',
      penwidth: 1.6,
      style: 'filled',
      width: 2.5,
    })}];`,
    `  edge [${dotAttributes({
      arrowsize: 0.72,
      color: COLORS.muted,
      fontcolor: COLORS.muted,
      fontname: 'Arial',
      fontsize: 12,
      penwidth: 1.6,
    })}];`,
  ]

  for (const diagramNode of specification.nodes) {
    const style = NODE_STYLES[diagramNode.kind] ?? NODE_STYLES.neutral
    lines.push(`  ${diagramNode.id} [${dotAttributes({
      ...style,
      label: wrapText(diagramNode.label, diagramNode.kind === 'decision' ? 22 : 30),
      tooltip: diagramNode.label,
    })}];`)
  }

  for (const cluster of specification.clusters ?? []) {
    lines.push(`  subgraph cluster_${cluster.id} {`)
    lines.push(`    graph [${dotAttributes({
      bgcolor: COLORS.surfaceDeep,
      color: COLORS.border,
      fontcolor: COLORS.peach,
      fontname: 'Arial',
      fontsize: 11,
      label: cluster.label,
      labeljust: 'l',
      margin: 20,
      penwidth: 1.2,
    })}];`)
    for (const nodeId of cluster.nodes) lines.push(`    ${nodeId};`)
    for (const sameRank of cluster.sameRanks ?? []) {
      lines.push(`    { rank=same; ${sameRank.join('; ')}; }`)
    }
    lines.push('  }')
  }

  for (const sameRank of specification.sameRanks ?? []) {
    lines.push(`  { rank=same; ${sameRank.join('; ')}; }`)
  }

  for (const diagramEdge of specification.edges) {
    const tone = EDGE_STYLES[diagramEdge.tone ?? 'neutral']
    lines.push(`  ${diagramEdge.from} -> ${diagramEdge.to} [${dotAttributes({
      ...tone,
      label: diagramEdge.label ? wrapText(diagramEdge.label, 24) : undefined,
      style: diagramEdge.style ?? 'solid',
      tooltip: diagramEdge.label,
    })}];`)
  }

  lines.push('}')
  return `${lines.join('\n')}\n`
}

function finalizeSvg(svg, specification) {
  const titleId = `${specification.filename.replace(/\.svg$/, '')}-title`
  const descriptionId = `${specification.filename.replace(/\.svg$/, '')}-description`
  let output = svg
    .replace(/<\?xml[^>]*>\s*/i, '')
    .replace(/<!DOCTYPE[\s\S]*?>\s*/i, '')
    .replace(
      /<svg\b/,
      `<svg role="img" aria-labelledby="${titleId} ${descriptionId}" focusable="false"`
    )

  const svgStart = output.indexOf('<svg')
  const openingTagEnd = output.indexOf('>', svgStart)
  const accessibleText = [
    `<title id="${titleId}">${escapeXml(specification.title)}</title>`,
    `<desc id="${descriptionId}">${escapeXml(specification.description)}</desc>`,
    '<style>text { font-family: "Uncut Sans", Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; }</style>',
  ].join('\n')

  output = `${output.slice(0, openingTagEnd + 1)}\n${accessibleText}${output.slice(openingTagEnd + 1)}`
  return output.replace(/\n{3,}/g, '\n\n')
}

async function generateSvgAssets() {
  await fs.mkdir(outputDirectory, { recursive: true })

  for (const specification of diagrams) {
    const dot = buildDot(specification)
    const rawSvg = execFileSync('dot', ['-Tsvg'], {
      encoding: 'utf8',
      input: dot,
      maxBuffer: 10 * 1024 * 1024,
    })
    const svg = finalizeSvg(rawSvg, specification)
    await fs.writeFile(path.join(outputDirectory, specification.filename), svg)
  }
}

async function syncDocumentation() {
  let replacements = 0
  const byFile = new Map()

  for (const specification of diagrams) {
    const records = byFile.get(specification.sourcePath) ?? []
    records.push(specification)
    byFile.set(specification.sourcePath, records)
  }

  for (const [relativePath, specifications] of byFile) {
    const documentationPath = path.join(gitbookDirectory, relativePath)
    let markdown = await fs.readFile(documentationPath, 'utf8')

    for (const specification of specifications) {
      const language = specification.sourceLanguage ?? ''
      const sourceBlock = `\`\`\`${language}\n${specification.source}\n\`\`\``
      const relativeAssetPath = path.relative(
        path.dirname(documentationPath),
        path.join(outputDirectory, specification.filename)
      ).split(path.sep).join('/')
      const imageReference = `![${specification.alt}](${relativeAssetPath})`

      if (markdown.includes(sourceBlock)) {
        markdown = markdown.replace(sourceBlock, imageReference)
        replacements += 1
      } else if (!markdown.includes(imageReference)) {
        throw new Error(`Could not find diagram source or generated reference in ${relativePath}: ${specification.title}`)
      }
    }

    await fs.writeFile(documentationPath, markdown)
  }

  return replacements
}

async function writeManifest() {
  const manifest = {
    diagramCount: diagrams.length,
    generator: path.relative(gitbookDirectory, fileURLToPath(import.meta.url)).split(path.sep).join('/'),
    diagrams: diagrams.map((specification) => ({
      title: specification.title,
      source: specification.sourcePath,
      asset: path.relative(
        gitbookDirectory,
        path.join(outputDirectory, specification.filename)
      ).split(path.sep).join('/'),
      alt: specification.alt,
    })),
  }

  await fs.writeFile(manifestPath, `${JSON.stringify(manifest, null, 2)}\n`)
}

async function main() {
  if (diagrams.length !== 30) {
    throw new Error(`Expected 30 diagrams, found ${diagrams.length.toString()}`)
  }

  await generateSvgAssets()
  const replacements = await syncDocumentation()
  await writeManifest()

  process.stdout.write(
    `Generated ${diagrams.length.toString()} SVG diagrams and replaced ${replacements.toString()} Markdown diagram blocks.\n`
  )
}

await main()
