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
      className={`cursor-pointer font-medium text-[#FFAB96] underline decoration-2 underline-offset-4 transition-colors hover:text-content-primary focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-[#FFAB96] ${className}`}
    >
      {children}
    </a>
  )
}
