import { describe, expect, it } from 'vitest'
import { isPrimaryAppDeployment, isSepoliaDeployment } from '../deployment'

describe('isSepoliaDeployment', () => {
  it('matches known Sepolia frontend hosts', () => {
    expect(isSepoliaDeployment('app.sepolia.plether.com', 1)).toBe(true)
    expect(isSepoliaDeployment('sepolia.plether.com', 1)).toBe(true)
    expect(isSepoliaDeployment('plether-testnet.pages.dev', 1)).toBe(true)
  })

  it('does not match mainnet hosts', () => {
    expect(isSepoliaDeployment('app.plether.com', 1)).toBe(false)
    expect(isSepoliaDeployment('plether.fi', 1)).toBe(false)
  })

  it('matches local hosts only when the default chain is testnet', () => {
    expect(isSepoliaDeployment('localhost', 11155111)).toBe(true)
    expect(isSepoliaDeployment('127.0.0.1', 421614)).toBe(true)
    expect(isSepoliaDeployment('localhost', 1)).toBe(false)
  })
})

describe('isPrimaryAppDeployment', () => {
  it('matches the production app host', () => {
    expect(isPrimaryAppDeployment('app.plether.com')).toBe(true)
    expect(isPrimaryAppDeployment('APP.PLETHER.COM')).toBe(true)
  })

  it('does not match landing, testnet, or local hosts', () => {
    expect(isPrimaryAppDeployment('plether.fi')).toBe(false)
    expect(isPrimaryAppDeployment('app.sepolia.plether.com')).toBe(false)
    expect(isPrimaryAppDeployment('localhost')).toBe(false)
  })
})
