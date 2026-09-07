import { type ReactNode, useEffect } from 'react'
import { useLocation } from 'react-router-dom'
import { Header } from './Header'
import { Footer } from './Footer'
import { MobileNav } from './MobileNav'
import { SepoliaNoticeBar } from './SepoliaNoticeBar'
import { WrongNetworkBanner } from '../wallet/WrongNetworkBanner'
import { useAccount } from 'wagmi'
import { useTransactionStore } from '../../stores/transactionStore'
import { isSepoliaDeployment } from '../../utils/deployment'

interface LayoutProps {
  children: ReactNode
}

export function Layout({ children }: LayoutProps) {
  const { isConnected } = useAccount()
  const { pathname } = useLocation()
  const fullWidth = pathname === '/'
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
      <main className={`w-full min-w-0 flex-grow ${fullWidth ? 'p-4 sm:p-6' : 'page-gutter py-6 sm:py-8 mx-auto max-w-7xl'}`}>
        {children}
      </main>
      <Footer />
      <MobileNav />
    </div>
  )
}
