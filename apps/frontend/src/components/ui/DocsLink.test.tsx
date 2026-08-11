import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { DocsLink } from './DocsLink'

describe('DocsLink', () => {
  it('uses a reliable single-pixel underline treatment on interaction', () => {
    render(
      <DocsLink href="https://docs.plether.com/example" title="Helpful context">
        Learn more
      </DocsLink>
    )

    const link = screen.getByRole('link', { name: 'Read: Helpful context' })

    expect(link).toHaveClass(
      'border-b',
      'border-transparent',
      'hover:border-current',
      'focus-visible:border-current'
    )
    expect(link).not.toHaveClass('border-b-2')
    expect(link).toHaveAttribute('target', '_blank')
    expect(link).toHaveAttribute('rel', 'noopener noreferrer')
  })
})
