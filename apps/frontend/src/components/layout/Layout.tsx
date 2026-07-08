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
    <div className="min-h-screen flex flex-col bg-app-bg text-content-primary">
      {isConnected && <WrongNetworkBanner />}
      <div className="sticky top-0 z-50">
        {isSepoliaDeployment() ? <SepoliaNoticeBar /> : null}
        <Header />
      </div>
      <main className={`flex-grow px-6 lg:px-8 py-10 w-full pb-24 lg:pb-10 ${fullWidth ? '' : 'max-w-7xl mx-auto'}`}>
        {children}
      </main>
      <Footer />
      <MobileNav />
    </div>
  )
}
