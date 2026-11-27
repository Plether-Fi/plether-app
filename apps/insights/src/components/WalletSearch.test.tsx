import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter, Route, Routes, useParams } from 'react-router-dom'
import { describe, expect, it } from 'vitest'
import { WalletSearch } from './WalletSearch'

function ResolvedWallet() {
  const { address } = useParams()
  return <div data-testid="resolved-wallet">{address}</div>
}

function ResolvedTx() {
  const { txHash } = useParams()
  return <div data-testid="resolved-tx">{txHash}</div>
}

function renderSearch() {
  render(
    <MemoryRouter initialEntries={['/']}>
      <Routes>
        <Route path="/" element={<WalletSearch />} />
        <Route path="/wallets/:address" element={<ResolvedWallet />} />
        <Route path="/tx/:txHash" element={<ResolvedTx />} />
      </Routes>
    </MemoryRouter>,
  )
}

describe('WalletSearch', () => {
  it('rejects incomplete or non-EVM addresses with an accessible alert', () => {
    renderSearch()
    fireEvent.change(screen.getByRole('searchbox'), { target: { value: '0x1234' } })
    fireEvent.click(screen.getByRole('button', { name: 'Inspect' }))
    expect(screen.getByRole('alert')).toHaveTextContent(/Trading Account address/i)
    expect(screen.queryByTestId('resolved-wallet')).not.toBeInTheDocument()
  })

  it('navigates directly to the Trading Account route for a valid 42-character address', () => {
    const address = `0x${'a'.repeat(40)}`
    renderSearch()
    fireEvent.change(screen.getByRole('searchbox'), { target: { value: `  ${address}  ` } })
    fireEvent.click(screen.getByRole('button', { name: 'Inspect' }))
    expect(screen.getByTestId('resolved-wallet')).toHaveTextContent(address)
  })

  it('navigates directly to the Transaction page for a valid 66-character tx hash', () => {
    const txHash = `0x${'B'.repeat(64)}`
    renderSearch()
    fireEvent.change(screen.getByRole('searchbox'), { target: { value: `  ${txHash}  ` } })
    fireEvent.click(screen.getByRole('button', { name: 'Inspect' }))
    expect(screen.getByTestId('resolved-tx')).toHaveTextContent(`0x${'b'.repeat(64)}`)
  })
})
