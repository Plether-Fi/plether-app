import type { ReactNode } from 'react'
import { Link, useInRouterContext } from 'react-router-dom'
import { oracleUpdatePath } from '../api'

export interface OraclePriceLinkProps {
  /** Optional specific oracle publication ID; defaults to `/oracle/updates/latest`. */
  updateId?: string | null
  children: ReactNode
  className?: string
  /** Keep the caller's text color (e.g. green/orange hero mark) instead of default peach link styling. */
  preserveColor?: boolean
  title?: string
}

const BASE_LINK_CLASS =
  'underline decoration-brand-peach/55 underline-offset-2 transition-colors hover:text-brand-peach hover:decoration-brand-peach'

const DEFAULT_COLOR_CLASS = 'text-brand-peach'

/**
 * Links any displayed plDXY oracle price to its dedicated Oracle Publication
 * detail page (`/oracle/updates/:id` or `/oracle/updates/latest`).
 */
export function OraclePriceLink({
  updateId,
  children,
  className,
  preserveColor = false,
  title,
}: OraclePriceLinkProps) {
  const inRouter = useInRouterContext()
  const to = oracleUpdatePath(updateId)
  const trimmedId = updateId?.trim()
  const resolvedTitle =
    title ??
    (trimmedId && trimmedId.toLowerCase() !== 'latest'
      ? `Inspect oracle publication #${trimmedId}`
      : 'Inspect latest oracle publication')
  const mergedClass = [
    BASE_LINK_CLASS,
    preserveColor ? '' : DEFAULT_COLOR_CLASS,
    className ?? '',
  ]
    .filter(Boolean)
    .join(' ')

  if (!inRouter) {
    return (
      <a href={to} className={mergedClass} title={resolvedTitle}>
        {children}
      </a>
    )
  }

  return (
    <Link to={to} className={mergedClass} title={resolvedTitle}>
      {children}
    </Link>
  )
}
