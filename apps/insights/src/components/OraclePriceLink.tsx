import type { ReactNode } from 'react'
import { Link, useInRouterContext } from 'react-router-dom'
import { oracleUpdatePath } from '../api'

export interface OraclePriceLinkProps {
  /** Optional specific oracle publication ID; defaults to `/oracle/updates/latest`. */
  updateId?: string | null
  children: ReactNode
  className?: string
  title?: string
}

const DEFAULT_CLASS =
  'underline decoration-brand-peach/45 decoration-dotted underline-offset-2 transition-colors hover:text-brand-peach hover:decoration-brand-peach hover:decoration-solid'

/**
 * Links any displayed plDXY oracle price to its dedicated Oracle Publication
 * detail page (`/oracle/updates/:id` or `/oracle/updates/latest`).
 */
export function OraclePriceLink({
  updateId,
  children,
  className,
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
  const mergedClass = className ? `${DEFAULT_CLASS} ${className}` : DEFAULT_CLASS

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
