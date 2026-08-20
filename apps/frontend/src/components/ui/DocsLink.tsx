import type { ReactNode } from 'react'

interface DocsLinkProps {
  href: string
  title: string
  children: ReactNode
  className?: string
}

export function DocsLink({ href, title, children, className = '' }: DocsLinkProps) {
  return (
    <a
      href={href}
      aria-label={`Read: ${title}`}
      target="_blank"
      rel="noopener noreferrer"
      className={`cursor-pointer border-b border-transparent font-medium text-[#FFAB96] transition-colors hover:border-current hover:text-content-primary focus-visible:border-current focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-[#FFAB96] ${className}`}
    >
      {children}
    </a>
  )
}
