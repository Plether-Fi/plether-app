import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom'
import { describe, expect, it } from 'vitest'
import { GlobalSearch } from './GlobalSearch'

function RouteEcho() {
  const loc = useLocation()
  return <div data-testid="route-echo">{`${loc.pathname}${loc.search}`}</div>
}

function renderWithRouter() {
  return render(
    <MemoryRouter initialEntries={['/']}>
      <GlobalSearch />
      <Routes>
        <Route path="*" element={<RouteEcho />} />
      </Routes>
    </MemoryRouter>,
  )
}

describe('GlobalSearch', () => {
  it('routes 0x wallet addresses to /wallets/:address, tx hashes to /tx/:hash, and handles to /accounts?q=', () => {
    renderWithRouter()
    const input = screen.getByRole('searchbox')
    const form = screen.getByRole('search')

    const addr = `0x${'a'.repeat(40)}`
    fireEvent.change(input, { target: { value: addr } })
    fireEvent.submit(form)
    expect(screen.getByTestId('route-echo')).toHaveTextContent(`/wallets/${addr}`)

    const tx = `0x${'B'.repeat(64)}`
    fireEvent.change(input, { target: { value: tx } })
    fireEvent.submit(form)
    expect(screen.getByTestId('route-echo')).toHaveTextContent(`/tx/${tx.toLowerCase()}`)

    fireEvent.change(input, { target: { value: '@macro_fox' } })
    fireEvent.submit(form)
    expect(screen.getByTestId('route-echo')).toHaveTextContent('/divisions?q=macro_fox')
  })

  it('focuses search input when pressing / outside an editable control', () => {
    renderWithRouter()
    const input = screen.getByRole('searchbox')
    expect(document.activeElement).not.toBe(input)
    fireEvent.keyDown(document, { key: '/' })
    expect(document.activeElement).toBe(input)
  })
})
