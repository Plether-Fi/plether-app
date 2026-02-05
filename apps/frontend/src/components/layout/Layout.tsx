import { type ReactNode, useEffect } from 'react'
import { Header } from './Header'
import { MobileNav } from './MobileNav'
import { WrongNetworkBanner } from '../wallet/WrongNetworkBanner'
import { useAccount } from 'wagmi'
import { useTransactionStore } from '../../stores/transactionStore'

interface LayoutProps {
  children: ReactNode
}

export function Layout({ children }: LayoutProps) {
  const { isConnected } = useAccount()
  const cleanupOldTransactions = useTransactionStore((s) => s.cleanupOldTransactions)

  useEffect(() => {
    cleanupOldTransactions()
  }, [cleanupOldTransactions])

  return (
    <div className="min-h-screen flex flex-col bg-cyber-bg text-cyber-text-primary">
      {isConnected && <WrongNetworkBanner />}
      <Header />
      <main className="flex-grow max-w-7xl mx-auto px-6 lg:px-8 py-10 w-full pb-24 lg:pb-10">
        {children}
      </main>
      <MobileNav />
    </div>
  )
}
