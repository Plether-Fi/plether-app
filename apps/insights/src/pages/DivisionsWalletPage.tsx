import { Link, useParams } from 'react-router-dom'
import { formatPlDxyQuantity } from '../api/wallets'
import { ArbiscanButton, CopyButton } from '../components/ActionButtons'
import { AddressAvatar } from '../components/AddressAvatar'
import { ShareCardModal, type ShareCardOption } from '../components/ShareCardModal'
import { StatusPill } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl } from '../components/ui'
import { WalletMetric as Metric } from '../components/WalletMetric'
import { CurrentBadge } from '../divisions/CurrentBadge'
import { formatReturn } from '../divisions/engine'
import { useDivisions, useLiveDivisions } from '../divisions/query'
import {
  computeLiveTierProjection,
  useDivisionsWalletContext,
  type DivisionsWeekContext,
} from '../divisions/walletContext'
import {
  formatAssets,
  formatCompactAssets,
  formatPrice,
  formatRoi,
  formatUtc,
  isWalletAddress,
  shortAddress,
} from '../utils/format'

const WEEKDAY_LABELS = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri'] as const

function formatWeekAssets(raw: string, isMockUsdc: boolean): string {
  const formatted = formatAssets(raw)
  return isMockUsdc ? formatted.replace(/USDG$/, 'mock USDC') : formatted
}

function formatWeekCompactAssets(raw: string, isMockUsdc: boolean): string {
  const formatted = formatCompactAssets(raw)
  return isMockUsdc ? formatted.replace(/USDG$/, 'mock USDC') : formatted
}

function FxSessionStrip({
  weekdays,
  activeDays,
}: {
  weekdays?: [boolean, boolean, boolean, boolean, boolean]
  activeDays: number
}) {
  if (!weekdays) {
    return (
      <span className="font-mono text-sm font-semibold text-content-primary">
        {String(activeDays)} / 5
      </span>
    )
  }
  return (
    <div className="space-y-1.5">
      <div className="flex flex-wrap items-center gap-1" aria-label={`Active FX sessions: ${String(activeDays)} of 5`}>
        {WEEKDAY_LABELS.map((day, index) => {
          const active = weekdays[index]
          return (
            <span
              key={day}
              className={`border px-1.5 py-0.5 font-mono text-[10px] font-semibold uppercase ${
                active
                  ? 'border-positive/50 bg-positive/15 text-positive'
                  : 'border-brand-border/25 bg-app-bg text-content-tertiary'
              }`}
            >
              {day}
            </span>
          )
        })}
      </div>
      <p className="font-mono text-[11px] text-content-tertiary">{String(activeDays)} / 5 sessions</p>
    </div>
  )
}

function CutoffContextBlock({
  context,
  mode,
}: {
  context: DivisionsWeekContext
  mode: 'live' | 'published'
}) {
  const isMockUsdc = context.assetSymbol === 'USDC'
  const assetKind = isMockUsdc ? 'mock' : undefined
  const bridge = context.equityBridge
  const snap = context.cutoffSnapshot
  const pos = snap?.hasPosition ? snap.position : null
  const labelPrefix = mode === 'live' ? 'Snapshot' : 'Cutoff'
  const netFlow =
    bridge != null
      ? BigInt(bridge.depositsAssets) - BigInt(bridge.withdrawalsAssets)
      : BigInt(context.activity.depositsAssets) - BigInt(context.activity.withdrawalsAssets)

  return (
    <div className="grid gap-4 lg:grid-cols-2">
      {/* Equity Accounting Bridge & Week Activity */}
      <Panel>
        <div className="flex flex-wrap items-center justify-between gap-2 border-b border-brand-border/15 px-5 py-3">
          <span className="text-xs font-semibold uppercase tracking-wider text-content-tertiary">
            {mode === 'live' ? 'Live equity accounting bridge' : 'Audited cutoff equity bridge'}
          </span>
          <span className="font-mono text-[11px] text-content-tertiary">
            #{Number(context.baselineBlock).toLocaleString('en-US')} → #
            {Number(context.cutoffBlock).toLocaleString('en-US')}
          </span>
        </div>

        {bridge ? (
          <dl className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 sm:divide-y-0">
            <Metric label="Starting equity (baseline)">
              {formatWeekAssets(bridge.startingEquityAssets, isMockUsdc)}
            </Metric>
            <Metric label="Net funding (dep − wd)">
              <Pnl value={netFlow.toString()} assetKind={assetKind} />
            </Metric>
            <Metric label="Adjusted equity base">
              {formatWeekAssets(bridge.adjustedEquityAssets, isMockUsdc)}
            </Metric>
            <Metric label={`${labelPrefix} economic equity`}>
              {formatWeekAssets(bridge.cutoffEquityAssets, isMockUsdc)}
            </Metric>
          </dl>
        ) : (
          <div className="px-5 py-3 text-xs text-content-secondary">
            Historical on-chain boundary read unavailable for release{' '}
            <span className="font-mono text-content-primary">{context.releaseVersion}</span>.
          </div>
        )}

        <div className="border-t border-brand-border/15 bg-app-bg/40 px-5 py-3">
          <div className="flex flex-wrap items-center justify-between gap-x-4 gap-y-1 font-mono text-xs">
            <span className="text-content-tertiary">
              Week activity:{' '}
              <strong className="text-content-primary">
                {context.activity.tradesCount.toLocaleString('en-US')}{' '}
                {context.activity.tradesCount === 1 ? 'fill' : 'fills'}
              </strong>{' '}
              ({String(context.activity.opensCount)} open / {String(context.activity.closesCount)}{' '}
              close
              {context.activity.liquidationsCount > 0
                ? ` / ${String(context.activity.liquidationsCount)} liq`
                : ''}
              )
            </span>
            <span className="text-content-secondary">
              Vol{' '}
              <strong className="text-content-primary">
                {formatWeekCompactAssets(context.activity.volumeAssets, isMockUsdc)}
              </strong>
              {' · '}Realized{' '}
              <Pnl value={context.activity.realizedPnlAssets} assetKind={assetKind} />
            </span>
          </div>
        </div>
      </Panel>

      {/* Exact Wallet Balance & Position at Cutoff Moment */}
      <Panel>
        <div className="flex flex-wrap items-center justify-between gap-2 border-b border-brand-border/15 px-5 py-3">
          <span className="text-xs font-semibold uppercase tracking-wider text-content-tertiary">
            Position &amp; balance at {labelPrefix.toLowerCase()}
          </span>
          {snap ? (
            pos ? (
              <StatusPill
                tone={pos.liquidatable ? 'blocked' : 'live'}
                label={`${pos.side.toUpperCase()} open at ${labelPrefix.toLowerCase()}`}
              />
            ) : (
              <StatusPill tone="ready" label={`Flat at ${labelPrefix.toLowerCase()}`} />
            )
          ) : (
            <StatusPill tone="unknown" label="Snapshot unavailable" />
          )}
        </div>

        {snap ? (
          <>
            <dl className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 sm:divide-y-0">
              <Metric label="Settlement balance">
                {formatWeekAssets(snap.settlementBalanceAssets, isMockUsdc)}
              </Metric>
              <Metric label="Free collateral">
                {formatWeekAssets(snap.freeSettlementAssets, isMockUsdc)}
              </Metric>
              {pos ? (
                <>
                  <Metric label="Position size">
                    <span className={pos.side === 'short' ? 'text-brand-orange' : 'text-positive'}>
                      {pos.side.toUpperCase()}
                    </span>{' '}
                    · {formatPlDxyQuantity(pos.size)}
                  </Metric>
                  <Metric label="Entry price">{formatPrice(pos.entryPrice)}</Metric>
                  <Metric label="Locked position margin">
                    {formatWeekAssets(pos.marginAssets, isMockUsdc)}
                  </Metric>
                  <Metric label="Unrealized PnL at cutoff">
                    <Pnl value={pos.unrealizedPnlAssets} assetKind={assetKind} />
                  </Metric>
                </>
              ) : (
                <>
                  <Metric label="Active position margin">
                    {formatWeekAssets(snap.activePositionMarginAssets, isMockUsdc)}
                  </Metric>
                  <Metric label="Trader claims / pending carry">
                    {formatWeekAssets(snap.traderClaimBalanceAssets, isMockUsdc)}
                    {snap.pendingCarryAssets !== '0'
                      ? ` · carry −${formatWeekCompactAssets(snap.pendingCarryAssets, isMockUsdc)}`
                      : ''}
                  </Metric>
                </>
              )}
            </dl>
            <div className="border-t border-brand-border/15 bg-app-bg/40 px-5 py-2.5 font-mono text-[11px] text-content-tertiary">
              Exact on-chain CfdEngineAccountLens state at block #
              {Number(context.cutoffBlock).toLocaleString('en-US')} ({formatUtc(context.cutoffAt)})
            </div>
          </>
        ) : (
          <div className="p-5 text-xs text-content-secondary">
            On-chain ledger snapshot at block #{Number(context.cutoffBlock).toLocaleString('en-US')}{' '}
            is not available for this release.
          </div>
        )}
      </Panel>
    </div>
  )
}

function TraderDetails({ address }: { address: string }) {
  const liveQuery = useLiveDivisions()
  const publicationQuery = useDivisions()
  const contextQuery = useDivisionsWalletContext(address)

  const live = liveQuery.data?.status === 'ready' ? liveQuery.data : null
  const publication = publicationQuery.data
  const walletContext = contextQuery.data
  const lowerAddress = address.toLowerCase()

  const current = live?.standings?.find((row) => row.address.toLowerCase() === lowerAddress)
  const awarded = publication?.standings.find((row) => row.address.toLowerCase() === lowerAddress)
  const handle =
    current?.discordUsername ??
    awarded?.displayName ??
    walletContext?.identity.discordUsername ??
    null
  const traderLabel = handle ? `@${handle.replace(/^@/, '')}` : shortAddress(address)
  const notFound =
    live &&
    !current &&
    !awarded &&
    !liveQuery.isError &&
    !publicationQuery.isLoading &&
    !publicationQuery.isError

  const liveProjection = computeLiveTierProjection(live?.standings, address)
  const isTraderOfTheWeek = Boolean(
    publication?.announcement.tradersOfTheWeek.some((a) => a.toLowerCase() === lowerAddress),
  )
  const isDiamondRoster = Boolean(
    publication?.announcement.diamondRoster.some((a) => a.toLowerCase() === lowerAddress),
  )

  const publishedRankedCount = publication
    ? publication.standings.filter((s) => s.rank != null).length
    : null

  const shareCards: ShareCardOption[] = []
  if (current && live) {
    shareCards.push({
      id: 'division-live',
      label: 'Live Divisions Rank',
      data: {
        kind: 'division',
        traderLabel,
        account: address,
        networkName: 'Arbitrum Sepolia',
        tier: awarded?.tier ?? null,
        rank: current.rank,
        weeklyReturnLabel: formatRoi(current.weeklyReturnBps),
        activeDays: current.activeDays,
        weekLabel: `Week of ${formatUtc(live.weekStartsAt).slice(0, 11)}`,
        provisional: true,
      },
    })
  }
  if (awarded && publication) {
    shareCards.push({
      id: 'division-awarded',
      label: 'Published Divisions Badge',
      data: {
        kind: 'division',
        traderLabel,
        account: address,
        networkName: 'Arbitrum Sepolia',
        tier: awarded.tier,
        rank: awarded.rank,
        weeklyReturnLabel: formatReturn(awarded.weeklyPnlAssets, awarded.adjustedEquityAssets),
        weeklyPnlAssets: awarded.weeklyPnlAssets,
        activeDays: awarded.activeDays,
        movement: awarded.movement,
        weekLabel: `Ended ${formatUtc(publication.week.closesAt).slice(0, 11)}`,
        provisional: false,
      },
    })
  }

  const history = walletContext?.history ?? []

  return (
    <div className="space-y-6 sm:space-y-8">
      <div className="flex flex-wrap items-center justify-between gap-3">
        <Link to="/divisions" className="text-sm font-semibold text-brand-peach hover:underline">
          ← Back to leaderboard
        </Link>
        <Link
          to="/results"
          className="text-xs font-semibold text-content-secondary hover:text-content-primary hover:underline"
        >
          Browse weekly results archive →
        </Link>
      </div>

      {/* Prominent Trader Header with Unmistakable Trading Account CTAs */}
      <header className="flex flex-col items-start justify-between gap-5 border border-brand-border/25 bg-surface-panel p-5 sm:flex-row sm:p-6">
        <div className="min-w-0 space-y-3">
          <div className="flex flex-wrap items-center gap-2">
            <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">
              Plether Divisions · Trader
            </p>
            {isTraderOfTheWeek ? (
              <StatusPill tone="live" label="Trader of the Week" />
            ) : null}
            {isDiamondRoster ? (
              <StatusPill tone="ready" label="Diamond Roster" />
            ) : null}
          </div>

          <div className="flex items-center gap-3.5">
            <AddressAvatar address={address} size={44} />
            <h1 className="break-all text-3xl font-semibold sm:text-4xl">{traderLabel}</h1>
          </div>

          <div className="flex flex-wrap items-center gap-1.5">
            <p className="break-all font-mono text-xs text-content-secondary sm:text-sm">
              {address}
            </p>
            <CopyButton value={address} />
            <ArbiscanButton kind="address" value={address} />
          </div>

          <div className="flex flex-wrap items-center gap-2.5 pt-1">
            <Link
              to={`/wallets/${address}`}
              className="inline-flex items-center gap-1.5 border border-brand-orange bg-brand-orange px-4 py-2 text-xs font-semibold text-content-primary transition-colors hover:bg-brand-peach hover:text-app-bg"
            >
              Open Trading Account →
            </Link>
            <Link
              to={`/wallets/${address}?tab=exposure`}
              className="inline-flex items-center gap-1.5 border border-brand-border/45 bg-app-bg px-3.5 py-2 text-xs font-semibold text-brand-peach transition-colors hover:border-brand-peach hover:bg-brand-peach/10"
            >
              Market Exposure &amp; Lifecycles →
            </Link>
          </div>
        </div>

        <div className="flex flex-col items-start gap-2.5 sm:items-end">
          <div className="space-y-1 sm:text-right">
            <p className="text-xs text-content-tertiary">Current badge</p>
            <CurrentBadge
              tier={awarded?.tier}
              loading={publicationQuery.isLoading}
              unavailable={publicationQuery.isError && !publication}
            />
          </div>
          {shareCards.length > 0 ? (
            <ShareCardModal cards={shareCards} triggerLabel="Share Divisions card" />
          ) : null}
        </div>
      </header>

      <p className="text-sm text-content-secondary">
        Badges are awarded on Monday after the trading week ends (Friday 21:00 UTC cutoff). Live
        returns are provisional and do not change your current badge.
      </p>

      {notFound ? (
        <Panel>
          <EmptyState
            title="Trader not found"
            message="This wallet has no current-release trading results yet."
          />
        </Panel>
      ) : null}

      {/* Live Weekly Performance */}
      <section aria-labelledby="trader-live-title" className="space-y-3">
        <div className="flex flex-wrap items-baseline justify-between gap-2">
          <h2 id="trader-live-title" className="text-xl font-semibold">
            Live weekly performance
          </h2>
          {live?.snapshotBlock ? (
            <span className="font-mono text-xs text-content-tertiary">
              Snapshot block #{Number(live.snapshotBlock).toLocaleString('en-US')}
            </span>
          ) : null}
        </div>

        {liveQuery.isLoading ? (
          <Panel>
            <LoadingState />
          </Panel>
        ) : liveQuery.isError && !live ? (
          <ErrorState
            title="Live performance is unavailable"
            message={liveQuery.error.message}
            onRetry={() => void liveQuery.refetch()}
          />
        ) : current && live ? (
          <div className="space-y-4">
            <p className="text-xs text-content-tertiary">
              Provisional · Week starting {formatUtc(live.weekStartsAt)} · Snapshot{' '}
              {formatUtc(live.snapshotAt ?? '')}
            </p>
            {liveQuery.isError ? (
              <p role="status" className="text-sm text-brand-yellow">
                Could not refresh live performance. Showing the last available snapshot.
              </p>
            ) : null}
            <Panel>
              <dl className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 lg:grid-cols-4 lg:divide-y-0">
                <Metric label="Weekly rank">
                  {current.rank != null ? (
                    <>
                      #{current.rank}
                      {liveProjection ? (
                        <span className="ml-1.5 font-mono text-xs font-normal text-content-tertiary">
                          of {liveProjection.totalRanked}
                        </span>
                      ) : null}
                    </>
                  ) : (
                    '—'
                  )}
                </Metric>
                <Metric label="Weekly return">{formatRoi(current.weeklyReturnBps)}</Metric>
                <Metric label="Provisional pace">
                  {liveProjection ? (
                    current.weeklyReturnBps != null && current.weeklyReturnBps > 0 ? (
                      <div className="space-y-0.5">
                        <span className="text-brand-peach">
                          Tracking {liveProjection.projectedTier}
                        </span>
                        <p className="font-mono text-[11px] font-normal text-content-tertiary">
                          {liveProjection.percentileAmongProfitable != null
                            ? `Top ${liveProjection.percentileAmongProfitable.toFixed(1)}% of ${String(liveProjection.profitableCount)} profitable`
                            : null}
                          {liveProjection.nextTier && liveProjection.bpsBehindNextTier != null
                            ? ` · +${(liveProjection.bpsBehindNextTier / 100).toFixed(2)}% to ${liveProjection.nextTier}`
                            : ''}
                        </p>
                      </div>
                    ) : (
                      <div className="space-y-0.5">
                        <span className="text-content-secondary">Below profit cutoff</span>
                        {liveProjection.bpsBehindNextTier != null ? (
                          <p className="font-mono text-[11px] font-normal text-content-tertiary">
                            +{(liveProjection.bpsBehindNextTier / 100).toFixed(2)}% to reach profit
                          </p>
                        ) : null}
                      </div>
                    )
                  ) : (
                    '—'
                  )}
                </Metric>
                <Metric label="Active FX sessions">
                  <FxSessionStrip
                    weekdays={walletContext?.liveWeek?.activeWeekdays}
                    activeDays={current.activeDays}
                  />
                </Metric>
              </dl>
            </Panel>

            {walletContext?.liveWeek ? (
              <CutoffContextBlock context={walletContext.liveWeek} mode="live" />
            ) : null}
          </div>
        ) : (
          <Panel>
            <EmptyState
              title={live ? 'No live weekly results' : 'Preparing live rankings'}
              message={
                live
                  ? 'No current-week snapshot is available for this wallet.'
                  : 'The backend is preparing the current-week snapshot. Check back shortly.'
              }
            />
          </Panel>
        )}
      </section>

      {/* Last Published Week */}
      <section aria-labelledby="trader-awarded-title" className="space-y-3">
        <div className="flex flex-wrap items-baseline justify-between gap-2">
          <h2 id="trader-awarded-title" className="text-xl font-semibold">
            Last published week
          </h2>
          {publication ? (
            <span className="font-mono text-xs text-content-tertiary">
              Cutoff block #{Number(publication.source.closingBlock.number).toLocaleString('en-US')}
              {publication.scoringRelease ? ` · ${publication.scoringRelease.version}` : ''}
            </span>
          ) : null}
        </div>

        {publicationQuery.isLoading ? (
          <Panel>
            <LoadingState />
          </Panel>
        ) : publicationQuery.isError && !publication ? (
          <ErrorState
            title="Published results are unavailable"
            message={publicationQuery.error.message}
            onRetry={() => void publicationQuery.refetch()}
          />
        ) : awarded && publication ? (
          <div className="space-y-4">
            <p className="text-xs text-content-tertiary">
              Week ending {formatUtc(publication.week.closesAt)} · Published{' '}
              {formatUtc(publication.publishedAt)}
            </p>
            {publicationQuery.isError ? (
              <p role="status" className="text-sm text-brand-yellow">
                Could not refresh published results. Showing the last published week.
              </p>
            ) : null}
            <Panel>
              <dl className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 lg:grid-cols-5 lg:divide-y-0">
                <Metric label="Final rank">
                  {awarded.rank != null ? (
                    <>
                      #{awarded.rank}
                      {publishedRankedCount != null ? (
                        <span className="ml-1.5 font-mono text-xs font-normal text-content-tertiary">
                          of {publishedRankedCount}
                        </span>
                      ) : null}
                    </>
                  ) : (
                    '—'
                  )}
                </Metric>
                <Metric label="Weekly return">
                  {formatReturn(awarded.weeklyPnlAssets, awarded.adjustedEquityAssets)}
                </Metric>
                <Metric label="Net P&L">
                  <Pnl
                    value={awarded.weeklyPnlAssets}
                    assetKind={publication.scoringRelease?.assetSymbol === 'USDC' ? 'mock' : undefined}
                  />
                </Metric>
                <Metric label="Adjusted equity base">
                  {formatWeekAssets(
                    awarded.adjustedEquityAssets,
                    publication.scoringRelease?.assetSymbol === 'USDC',
                  )}
                </Metric>
                <Metric label="Active FX sessions">
                  <FxSessionStrip
                    weekdays={walletContext?.publishedWeek?.activeWeekdays}
                    activeDays={awarded.activeDays}
                  />
                </Metric>
              </dl>
            </Panel>

            <p className="text-sm text-content-secondary">
              {awarded.movement === 'promoted'
                ? `Promoted from ${awarded.previousTier} to ${awarded.tier}.`
                : awarded.movement === 'demoted'
                  ? `Demoted from ${awarded.previousTier} to ${awarded.tier}.`
                  : awarded.qualifying
                    ? 'Qualifying week · tier held.'
                    : 'Non-qualifying week.'}
            </p>

            {walletContext?.publishedWeek ? (
              <CutoffContextBlock context={walletContext.publishedWeek} mode="published" />
            ) : null}
          </div>
        ) : (
          <Panel>
            <EmptyState
              title="No published award yet"
              message="This trader awaits their first completed, published weekly result."
            />
          </Panel>
        )}
      </section>

      {/* Multi-Week Divisions History */}
      {history.length > 0 ? (
        <section aria-labelledby="trader-history-title" className="space-y-3">
          <div>
            <h2 id="trader-history-title" className="text-xl font-semibold">
              Multi-week Divisions trajectory
            </h2>
            <p className="mt-1 text-xs text-content-tertiary">
              Published weekly outcomes and tier progression across audited scoring windows
            </p>
          </div>
          <Panel className="overflow-x-auto">
            <table className="w-full min-w-[680px] text-left text-sm">
              <caption className="sr-only">Historical published Divisions weekly results</caption>
              <thead className="border-b border-brand-border/25 bg-app-bg/40 text-xs font-medium text-content-secondary">
                <tr>
                  <th scope="col" className="px-4 py-3">Week</th>
                  <th scope="col" className="px-4 py-3">Rank</th>
                  <th scope="col" className="px-4 py-3">Tier</th>
                  <th scope="col" className="px-4 py-3">Movement</th>
                  <th scope="col" className="px-4 py-3 text-right">Return</th>
                  <th scope="col" className="px-4 py-3 text-right">Net P&amp;L</th>
                  <th scope="col" className="px-4 py-3 text-right">Active days</th>
                </tr>
              </thead>
              <tbody>
                {history.map((week) => (
                  <tr
                    key={week.weekStart}
                    className="border-b border-brand-border/15 last:border-0 hover:bg-app-bg/35"
                  >
                    <td className="whitespace-nowrap px-4 py-3 font-mono text-xs text-content-secondary">
                      <Link
                        to={`/results?week=${String(week.weekStart)}`}
                        className="text-brand-peach hover:underline"
                      >
                        {week.weekClosesAt ? formatUtc(week.weekClosesAt).slice(0, 11) : `Week ${String(week.weekStart)}`}
                      </Link>
                    </td>
                    <td className="whitespace-nowrap px-4 py-3 font-mono text-xs font-semibold">
                      {week.rank != null ? `#${String(week.rank)}` : '—'}
                    </td>
                    <td className="whitespace-nowrap px-4 py-3 font-semibold capitalize">
                      {week.tier ?? '—'}
                    </td>
                    <td className="whitespace-nowrap px-4 py-3 text-xs capitalize text-content-secondary">
                      {week.movement ?? (week.qualifying ? 'Held' : 'Non-qualifying')}
                    </td>
                    <td className="whitespace-nowrap px-4 py-3 text-right font-mono text-xs font-semibold tabular-nums">
                      {week.weeklyPnlAssets && week.adjustedEquityAssets
                        ? formatReturn(week.weeklyPnlAssets, week.adjustedEquityAssets)
                        : '—'}
                    </td>
                    <td className="whitespace-nowrap px-4 py-3 text-right font-mono text-xs font-semibold tabular-nums">
                      {week.weeklyPnlAssets ? <Pnl value={week.weeklyPnlAssets} /> : '—'}
                    </td>
                    <td className="whitespace-nowrap px-4 py-3 text-right font-mono text-xs tabular-nums text-content-secondary">
                      {week.activeDays != null ? `${String(week.activeDays)} / 5` : '—'}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </Panel>
        </section>
      ) : null}
    </div>
  )
}

export function DivisionsWalletPage() {
  const { address = '' } = useParams()
  if (!isWalletAddress(address)) {
    return (
      <ErrorState
        title="Invalid wallet address"
        message="Enter a complete 0x-prefixed Ethereum wallet address."
      />
    )
  }
  return <TraderDetails address={address} />
}
