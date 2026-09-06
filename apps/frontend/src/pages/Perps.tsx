import { useCallback, useEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import { DxyBasketPanel } from '../components/DxyBasketPanel'
import { DxyBasketComponentsRail } from '../components/DxyBasketComponentsRail'
import { PerpsAccountPanel, type PerpsAccountTab } from '../components/PerpsAccountPanel'
import { PerpsInstrumentPanel, type PerpsInstrumentStat } from '../components/PerpsInstrumentPanel'
import { PerpsPoolLiquidityDetails } from '../components/PerpsPoolLiquidityDetails'
import { PerpsMarketStatePanel } from '../components/PerpsMarketStatePanel'
import { getPerpsMarketSchedule } from '../utils/perpsMarketSchedule'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import { INFO_TOOLTIP_PANEL_CLASS_NAME, TokenAmount } from '../components/ui'
import { useProtocolConfig } from '../api'
import { usePerpsAccount, usePerpsHistory, usePerpsMarket } from '../hooks'
import { dxyExposureFromContractNotional, formatPerpsUsdc } from '../utils/perps'
import { trackPerpsPageViewed } from '../analytics/perps'
import { usePerpsIdentity } from '../perps-aa'
import { DOCS_LINKS } from '../config/docs'

function displayValue(value: string | undefined, isLoading: boolean): string {
  if (value) return value
  return isLoading ? '...' : '--'
}

function usdcValue(value: string | undefined, isLoading: boolean): ReactNode {
  if (value) return <TokenAmount amount={value} />
  return isLoading ? '...' : '--'
}

function capacityTooltipValue(value: bigint | undefined, markPrice: bigint | undefined): string {
  if (value === undefined) return '--'
  return formatPerpsUsdc(dxyExposureFromContractNotional(value, markPrice) ?? value)
}

function formatMarkAge(ageSeconds: number): string {
  if (!Number.isFinite(ageSeconds) || ageSeconds < 0) return 'unknown age'
  if (ageSeconds < 60) return `${ageSeconds.toString()}s ago`

  const minutes = Math.floor(ageSeconds / 60)
  const seconds = ageSeconds % 60
  if (minutes < 60) return seconds > 0 ? `${minutes.toString()}m ${seconds.toString()}s ago` : `${minutes.toString()}m ago`

  const hours = Math.floor(minutes / 60)
  const remainingMinutes = minutes % 60
  if (hours < 24) return remainingMinutes > 0 ? `${hours.toString()}h ${remainingMinutes.toString()}m ago` : `${hours.toString()}h ago`

  const days = Math.floor(hours / 24)
  const remainingHours = hours % 24
  return remainingHours > 0 ? `${days.toString()}d ${remainingHours.toString()}h ago` : `${days.toString()}d ago`
}

export function Perps() {
  const perpsIdentity = usePerpsIdentity()
  const perpsMarket = usePerpsMarket()
  const protocolConfig = useProtocolConfig()
  const perpsAccount = usePerpsAccount(perpsMarket.raw.markPrice)
  const [isTransactionHistoryActive, setIsTransactionHistoryActive] = useState(false)
  const perpsHistory = usePerpsHistory({
    activityEnabled: isTransactionHistoryActive,
  })
  const [nowSeconds, setNowSeconds] = useState(() => Math.floor(Date.now() / 1000))
  const [closePositionRequestId, setClosePositionRequestId] = useState(0)
  const trackedPageViewRef = useRef(false)

  const handleAccountTabChange = useCallback((activeTab: PerpsAccountTab) => {
    setIsTransactionHistoryActive(activeTab === 'tradeHistory')
  }, [])

  const handleAccountRefresh = useCallback(async () => {
    const accountRefresh = perpsAccount.refetchDynamic()
    void perpsMarket.refetchDynamic()
    void perpsHistory.refetch()
    await accountRefresh
  }, [perpsAccount, perpsHistory, perpsMarket])

  useEffect(() => {
    if (trackedPageViewRef.current) return
    trackedPageViewRef.current = true
    trackPerpsPageViewed()
  }, [])

  useEffect(() => {
    const interval = window.setInterval(() => {
      setNowSeconds(Math.floor(Date.now() / 1000))
    }, 5_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [])

  const dxyFreshnessTooltip = useMemo(() => {
    if (perpsMarket.oracleFreshness === 'checking') return 'checking backend for a fresh update'

    if (!perpsMarket.oracleFreshnessTime) return undefined

    const ageSeconds = Math.max(0, nowSeconds - perpsMarket.oracleFreshnessTime)
    return `updated ${formatMarkAge(ageSeconds)}`
  }, [
    nowSeconds,
    perpsMarket.oracleFreshness,
    perpsMarket.oracleFreshnessTime,
  ])

  const marketSchedule = useMemo(
    () => getPerpsMarketSchedule(new Date(nowSeconds * 1000), perpsMarket.marketPhase),
    [nowSeconds, perpsMarket.marketPhase]
  )

  const instrumentStats = useMemo<PerpsInstrumentStat[]>(
    () => {
      const costOfCarryTooltip = (
        <div className="w-full space-y-3 text-left leading-5">
          <p>
            Annualized max carry paid by traders to LPs for the part of a position&apos;s worst-case
            payout backed by pool capital.
          </p>
          <p>
            This is not a funding rate; both sides can pay carry at the same time. The actual
            USDC amount depends on borrow base, side utilization, and elapsed time.
          </p>
        </div>
      )

      return [
        {
          label: 'plDXY Perp price',
          value: displayValue(perpsMarket.oraclePrice, perpsMarket.isLoading),
          freshness: perpsMarket.oracleFreshness,
          freshnessTooltip: dxyFreshnessTooltip,
          tooltip: 'The oracle-derived dollar-oriented mark, not a guaranteed execution price. Final execution may differ because of oracle confidence; VPI, fees, and execution rewards are separate adjustments.',
          tooltipDocsLink: DOCS_LINKS.perpsPrice,
          tooltipClassName: INFO_TOOLTIP_PANEL_CLASS_NAME,
          tooltipPosition: 'bottom',
          hoverDetails: (
            <DxyBasketComponentsRail
              components={perpsMarket.latestBasket?.components}
              priceChanges={perpsMarket.basketComponentPriceChanges}
              isLoading={perpsMarket.isBasketComponentsLoading}
              isError={perpsMarket.isBasketComponentsError}
              nowSeconds={nowSeconds}
              docsLink={DOCS_LINKS.direction}
            />
          ),
        },
        {
          label: '24h change',
          value: displayValue(perpsMarket.priceChange24h, perpsMarket.isStatsLoading),
          tone: perpsMarket.priceChange24hTone,
        },
        {
          label: '24h volume',
          value: usdcValue(perpsMarket.volume24h, perpsMarket.isStatsLoading),
        },
        {
          label: 'Directional limit used',
          directionalLimit: {
            usagePercent: perpsMarket.directionalLimit?.usagePercent,
            side: perpsMarket.directionalLimit?.side,
            totalExposure: usdcValue(perpsMarket.directionalLimit?.totalExposure, perpsMarket.isLoading),
            netExposure: usdcValue(perpsMarket.directionalLimit?.netExposure, perpsMarket.isLoading),
            limit: usdcValue(perpsMarket.directionalLimit?.limit, perpsMarket.isLoading),
            isLoading: perpsMarket.isLoading,
          },
        },
        {
          label: 'Pool liquidity',
          value: usdcValue(perpsMarket.availableLiquidity, perpsMarket.isLoading),
          hoverDetailsType: 'pool-liquidity',
          hoverDetailsLabel: 'Pool liquidity details',
          hoverDetails: (
            <PerpsPoolLiquidityDetails
              longCapacity={(
                <TokenAmount amount={capacityTooltipValue(
                  perpsMarket.raw.longOpenCapacityUsdc,
                  perpsMarket.raw.markPrice
                )} />
              )}
              shortCapacity={(
                <TokenAmount amount={capacityTooltipValue(
                  perpsMarket.raw.shortOpenCapacityUsdc,
                  perpsMarket.raw.markPrice
                )} />
              )}
              juniorPrincipal={usdcValue(perpsMarket.poolCapital?.juniorPrincipal, perpsMarket.isLoading)}
              seniorPrincipal={usdcValue(perpsMarket.poolCapital?.seniorPrincipal, perpsMarket.isLoading)}
              juniorSharePercent={perpsMarket.poolCapital?.juniorSharePercent}
              seniorSharePercent={perpsMarket.poolCapital?.seniorSharePercent}
              seniorStatus={perpsMarket.poolCapital?.seniorStatus}
              seniorImpairment={usdcValue(perpsMarket.poolCapital?.seniorImpairment, perpsMarket.isLoading)}
              isJuniorExhausted={perpsMarket.poolCapital?.isJuniorExhausted}
              isEmpty={perpsMarket.poolCapital?.isEmpty}
              isLoading={perpsMarket.isLoading}
              docsLink={DOCS_LINKS.poolLiquidity}
            />
          ),
        },
        {
          label: 'Cost of carry',
          value: displayValue(perpsMarket.costOfCarry, perpsMarket.isLoading),
          tooltip: costOfCarryTooltip,
          tooltipDocsLink: DOCS_LINKS.marketCostOfCarry,
          tooltipClassName: INFO_TOOLTIP_PANEL_CLASS_NAME,
          tooltipPosition: 'left',
        },
      ]
    },
    [
      perpsMarket.availableLiquidity,
      perpsMarket.basketComponentPriceChanges,
      perpsMarket.costOfCarry,
      perpsMarket.directionalLimit,
      dxyFreshnessTooltip,
      perpsMarket.isLoading,
      perpsMarket.isBasketComponentsError,
      perpsMarket.isBasketComponentsLoading,
      perpsMarket.isStatsLoading,
      perpsMarket.latestBasket,
      perpsMarket.oracleFreshness,
      perpsMarket.oraclePrice,
      perpsMarket.poolCapital,
      perpsMarket.priceChange24h,
      perpsMarket.priceChange24hTone,
      perpsMarket.raw.longOpenCapacityUsdc,
      perpsMarket.raw.markPrice,
      perpsMarket.raw.shortOpenCapacityUsdc,
      perpsMarket.volume24h,
      nowSeconds,
    ]
  )

  return (
    <div className="space-y-3 sm:space-y-4">
      {perpsIdentity.isAaManifestConfigured && (
        perpsIdentity.status === 'selection-required' ||
        perpsIdentity.status === 'continuity-required' ||
        perpsIdentity.status === 'blocked'
      ) ? (
        <div className="border border-brand-orange/40 bg-brand-orange/10 p-4 text-sm leading-5 text-content-primary">
          <div className="font-semibold text-brand-orange">Trading Account action required</div>
          <p className="mt-1 text-content-secondary">
            {perpsIdentity.error?.message ??
              (perpsIdentity.status === 'selection-required'
                ? 'Your connected wallet will remain the owner and signature surface for a Plether Trading Account. Positions, margin, orders, and claims will belong to that Trading Account.'
                : 'Plether updated its testnet deployment. Confirm the updated Trading Account configuration before continuing. The app will not fall back to the owner wallet.')}
          </p>
          {(perpsIdentity.status === 'selection-required' ||
            perpsIdentity.status === 'continuity-required') &&
          perpsIdentity.proposedIdentity ? (
            <button
              type="button"
              className="mt-3 border border-[#FFAB96] bg-[#FFAB96] px-4 py-2 text-sm font-semibold text-[#250917] hover:bg-[#FF572D] hover:text-[#FFF5F9]"
              onClick={() => {
                perpsIdentity.confirmIdentityAfterContinuityCheck()
              }}
            >
              {perpsIdentity.status === 'selection-required'
                ? 'Use Plether Trading Account'
                : 'Confirm updated Trading Account'}
            </button>
          ) : null}
        </div>
      ) : null}
      <div className="flex flex-col gap-3 sm:gap-6 xl:flow-root">
        <div className="min-w-0 xl:float-left xl:mb-6 xl:w-[calc(100%_-_clamp(340px,28vw,380px)_-_1.5rem)]">
          <PerpsInstrumentPanel stats={instrumentStats} />
        </div>

        <div className="min-w-0 xl:float-right xl:w-[clamp(340px,28vw,380px)]">
          <div className="-mb-px">
            <PerpsMarketStatePanel
              currentPhase={perpsMarket.marketPhase}
              currentDuration={marketSchedule.currentDuration}
              nextPhase={marketSchedule.nextPhase}
              nextDuration={marketSchedule.nextDuration}
            />
          </div>
          <PerpsTradeTicket
            enableLiveTrading
            oraclePriceRaw={perpsMarket.raw.markPrice}
            oraclePublishTime={perpsMarket.oracleFreshnessTime}
            oraclePriceDisplay={perpsMarket.oraclePrice}
            latestBasket={perpsMarket.latestBasket}
            adverseConfidenceMultiplierBps={protocolConfig.data?.data.constants.adverseConfidenceMultiplierBps}
            oracleFrozen={perpsMarket.oracleFrozen}
            oracleFreshness={perpsMarket.oracleFreshness}
            oracleFreshnessTooltip={dxyFreshnessTooltip}
            oracleBasketComponents={perpsMarket.raw.basketComponents}
            availableToTradeRaw={perpsAccount.freeBuyingPowerUsdc ?? perpsAccount.withdrawableUsdc}
            availableToTradeAmount={perpsAccount.display.availableToTrade}
            portfolioValueRaw={perpsAccount.equityUsdc}
            withdrawableUsdcRaw={perpsAccount.withdrawableUsdc}
            walletUsdcRaw={perpsAccount.walletUsdc}
            ownerWalletUsdcRaw={perpsAccount.ownerWalletUsdc}
            tradingAccountUsdcRaw={perpsAccount.tradingAccountUsdc}
            marginAllowanceUsdc={perpsAccount.marginAllowanceUsdc}
            currentPosition={perpsAccount.position}
            currentPositionSide={perpsAccount.position?.direction}
            currentPositionAmount={perpsAccount.display.positionNotional}
            closePositionRequestId={closePositionRequestId}
            pendingOrders={perpsAccount.pendingOrders}
            orderHistory={perpsHistory.orderHistory}
            ordersIndexedThroughBlockRaw={perpsHistory.ordersIndexedThroughBlockRaw}
            pendingOrderCount={perpsAccount.pendingOrders.length}
            activePositionProtectionId={perpsAccount.activePositionProtectionId}
            maxPendingOrders={perpsAccount.maxPendingOrders}
            firstPendingOrderId={perpsAccount.firstPendingOrderId}
            firstPendingOrderExpiryTime={perpsAccount.firstPendingOrderExpiryTime}
            longOpenCapacityUsdc={perpsMarket.raw.longOpenCapacityUsdc}
            shortOpenCapacityUsdc={perpsMarket.raw.shortOpenCapacityUsdc}
            minOpenNotionalUsdc={perpsMarket.raw.minOpenNotionalUsdc}
            minNewPositionNotionalUsdc={perpsMarket.raw.minNewPositionNotionalUsdc}
            maintenanceMarginBps={perpsMarket.raw.maintenanceMarginBps}
            initialMarginBps={perpsMarket.raw.initialMarginBps}
            executionFeeBps={perpsMarket.raw.executionFeeBps}
            marketPhase={perpsMarket.marketPhase}
            marketCurrentDuration={marketSchedule.currentDuration}
            onAccountRefresh={handleAccountRefresh}
          />
        </div>

        <div className="min-w-0 xl:clear-left xl:float-left xl:mb-6 xl:w-[calc(100%_-_clamp(340px,28vw,380px)_-_1.5rem)]">
          <DxyBasketPanel
            liquidationPriceRaw={perpsAccount.position?.liquidationPrice}
            marketPhase={perpsMarket.marketPhase}
            marketCurrentDuration={marketSchedule.currentDuration}
          />
        </div>

        <div className="min-w-0 xl:clear-left xl:float-left xl:w-[calc(100%_-_clamp(340px,28vw,380px)_-_1.5rem)]">
          <PerpsAccountPanel
            position={perpsAccount.position}
            equityUsdc={perpsAccount.equityUsdc}
            freeBuyingPowerUsdc={perpsAccount.freeBuyingPowerUsdc}
            traderClaimBalanceUsdc={perpsAccount.traderClaimBalanceUsdc}
            pendingOrders={perpsAccount.pendingOrders}
            orderHistory={perpsHistory.orderHistory}
            tradeHistory={perpsHistory.tradeHistory}
            isConnected={perpsAccount.isConnected}
            isLoading={perpsAccount.isLoading}
            isOrderHistoryLoading={perpsHistory.isOrderHistoryLoading}
            isTradeHistoryLoading={perpsHistory.isTradeHistoryLoading}
            orderHistoryError={perpsHistory.orderHistoryError}
            tradeHistoryError={perpsHistory.tradeHistoryError}
            onActiveTabChange={handleAccountTabChange}
            onAccountRefresh={() => {
              void handleAccountRefresh()
            }}
            onClosePosition={() => {
              setClosePositionRequestId((requestId) => requestId + 1)
            }}
          />
        </div>
      </div>
    </div>
  )
}

export default Perps
