import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { DocsLink } from './DocsLink'

describe('DocsLink', () => {
  it('uses a visible, consistent underline treatment', () => {
    render(
      <DocsLink href="https://docs.plether.com/example" title="Helpful context">
        Learn more
      </DocsLink>
    )

    const link = screen.getByRole('link', { name: 'Read: Helpful context' })

    expect(link).toHaveClass('underline', 'decoration-2', 'underline-offset-4')
    expect(link).toHaveAttribute('target', '_blank')
    expect(link).toHaveAttribute('rel', 'noopener noreferrer')
  })
})
