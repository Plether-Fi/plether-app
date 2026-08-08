import { Link } from 'react-router-dom'
import { Skeleton } from './ui'
import { formatUsd } from '../utils/formatters'

function SkeletonCard() {
  return (
    <div className="h-full min-w-0 border border-brand-border/30 bg-surface-panel p-4 sm:p-5">
      <Skeleton className="mb-3 h-3 w-24 max-w-full" />
      <Skeleton className="mb-2 h-8 w-32 max-w-full" />
      <Skeleton className="h-3 w-40 max-w-full" />
    </div>
  )
}

export interface PortfolioCardProps {
  title: string
  value: bigint
  description: string
  link: string
  isLoading: boolean
  colorClass: string
}

export function PortfolioCard({ title, value, description, link, isLoading, colorClass }: PortfolioCardProps) {
  if (isLoading) {
    return <SkeletonCard />
  }

  return (
    <Link to={link} className="block h-full min-w-0">
      <div className="h-full min-w-0 cursor-pointer border border-brand-border/30 bg-surface-panel p-4 transition-colors hover:border-[#FFAB96]/50 sm:p-5">
        <p className="mb-2 break-words text-xs font-medium uppercase tracking-wider text-content-secondary">{title}</p>
        <div className={`mb-1 break-words text-xl font-bold [overflow-wrap:anywhere] sm:text-2xl ${colorClass}`}>{formatUsd(value)} USDC</div>
        <p className="break-words text-xs text-content-secondary">{description}</p>
      </div>
    </Link>
  )
}
