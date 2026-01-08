import { type ReactNode } from 'react'
import { Header } from './Header'
import { MobileNav } from './MobileNav'
import { WrongNetworkBanner } from '../wallet/NetworkSwitcher'
import { useAccount } from 'wagmi'

interface LayoutProps {
  children: ReactNode
}

export function Layout({ children }: LayoutProps) {
  const { isConnected } = useAccount()

  return (
    <div className="min-h-screen flex flex-col">
      {/* Wrong network warning */}
      {isConnected && <WrongNetworkBanner />}

      {/* Header */}
      <Header />

      {/* Main content */}
      <main className="flex-1 w-full max-w-7xl mx-auto px-4 py-6 pb-24 lg:pb-6">
        {children}
      </main>

      {/* Mobile bottom navigation */}
      <MobileNav />
    </div>
  )
}
