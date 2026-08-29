import { type ReactNode, useEffect } from 'react'
import { useLocation } from 'react-router-dom'
import { Header } from './Header'
import { Footer } from './Footer'
import { MobileNav } from './MobileNav'
import { SepoliaNoticeBar } from './SepoliaNoticeBar'
import { WrongNetworkBanner } from '../wallet/WrongNetworkBanner'
import { useAccount } from 'wagmi'
import { useTransactionStore } from '../../stores/transactionStore'
import { isPrimaryAppDeployment, isSepoliaDeployment } from '../../utils/deployment'

interface LayoutProps {
  children: ReactNode
}

export function Layout({ children }: LayoutProps) {
  const { isConnected } = useAccount()
  const { pathname } = useLocation()
  const fullWidth = pathname === '/'
  const hasMobileNav = !isPrimaryAppDeployment()
  const cleanupOldTransactions = useTransactionStore((s) => s.cleanupOldTransactions)

  useEffect(() => {
    cleanupOldTransactions()
  }, [cleanupOldTransactions])

  return (
    <div className="flex min-h-screen min-h-dvh min-w-0 flex-col bg-app-bg text-content-primary">
      {isConnected && <WrongNetworkBanner />}
      <div data-app-sticky-header className="sticky top-0 z-50">
        {isSepoliaDeployment() ? <SepoliaNoticeBar /> : null}
        <Header />
      </div>
      <main className={`w-full min-w-0 flex-grow px-4 py-6 sm:px-6 sm:py-8 lg:px-8 lg:py-10 ${hasMobileNav ? 'pb-[calc(5rem+env(safe-area-inset-bottom))] lg:pb-10' : 'pb-8 lg:pb-10'} ${fullWidth ? '' : 'mx-auto max-w-7xl'}`}>
        {children}
      </main>
      <Footer />
      <MobileNav />
    </div>
  )
}
