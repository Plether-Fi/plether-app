import { Link } from 'react-router-dom'
import { Skeleton } from './ui'
import { formatUsd } from '../utils/formatters'

function SkeletonCard() {
  return (
    <div className="bg-surface-panel p-5 border border-brand-border/30 h-full">
      <Skeleton className="h-3 w-24 mb-3" />
      <Skeleton className="h-8 w-32 mb-2" />
      <Skeleton className="h-3 w-40" />
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
    <Link to={link}>
      <div className="bg-surface-panel p-5 border border-brand-border/30 hover:border-[#FFAB96]/50 transition-colors cursor-pointer h-full">
        <p className="text-xs text-content-secondary uppercase tracking-wider font-medium mb-2">{title}</p>
        <div className={`text-2xl font-bold mb-1 ${colorClass}`}>{formatUsd(value)} USDC</div>
        <p className="text-xs text-content-secondary truncate">{description}</p>
      </div>
    </Link>
  )
}
