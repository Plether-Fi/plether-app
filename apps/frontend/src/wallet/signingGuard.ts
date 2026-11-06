import type { EIP1193Provider } from '@privy-io/react-auth'
import type { WalletSelection } from './selection'

interface ProviderWallet {
  address: string
  walletClientType?: string
  meta?: { id: string }
  getEthereumProvider: () => Promise<EIP1193Provider>
}

function requiresSelectedOwner(method: string): boolean {
  return method === 'personal_sign'
    || method.startsWith('eth_sign')
    || method.startsWith('eth_send')
    || method.startsWith('wallet_sign')
    || method.startsWith('wallet_send')
    || method === 'wallet_grantPermissions'
    || method === 'wallet_revokePermissions'
    || method === 'wallet_switchEthereumChain'
    || method === 'wallet_addEthereumChain'
}

// A bridge connector can finish initializing after its owner was deselected.
// Check the selected owner on every write, outside the asynchronous connector
// lifecycle. A cached provider becomes usable again only if its owner is
// explicitly selected again and is available through the initialized SDK.
export function createSigningGuard(initialOwner: WalletSelection) {
  let owner = initialOwner?.toLowerCase() ?? initialOwner
  let ready = false
  let revision = 0

  const assertSelected = (address: string) => {
    if (!ready || owner !== address.toLowerCase()) {
      throw new Error('The selected wallet changed or is not ready. Reconnect or select an owner wallet before signing.')
    }
  }

  return {
    get revision() { return revision },
    get selection() { return owner },
    setReady(value: boolean) {
      ready = value
    },
    select(address: string | null) {
      const nextOwner = address?.toLowerCase() ?? null
      if (owner === nextOwner) return
      owner = nextOwner
      revision += 1
    },
    wrapWallet<T extends ProviderWallet>(wallet: T): T {
      const address = wallet.address.toLowerCase()
      // Privy's bridge otherwise identifies every account in an external
      // extension by the same connector ID, reusing the previous owner's
      // guarded provider. Scope our bridge's identity to the chosen owner.
      const connectorMeta = wallet.meta && wallet.walletClientType !== 'privy'
        ? { ...wallet.meta, id: `${wallet.meta.id}.${address}` }
        : wallet.meta
      return new Proxy(wallet, {
        get(target, property) {
          if (property === 'meta') return connectorMeta
          if (property === 'getEthereumProvider') {
            return async () => {
              const provider = await target.getEthereumProvider()
              return new Proxy(provider, {
                get(providerTarget, providerProperty) {
                  if (providerProperty === 'request') {
                    return async (args: Parameters<EIP1193Provider['request']>[0]) => {
                      if (requiresSelectedOwner(args.method)) assertSelected(address)
                      const result: unknown = await providerTarget.request(args)
                      return result
                    }
                  }
                  return boundMember(providerTarget, providerProperty)
                },
              })
            }
          }
          return boundMember(target, property)
        },
      })
    },
  }
}

function boundMember(target: object, property: PropertyKey): unknown {
  const value: unknown = Reflect.get(target, property, target)
  if (typeof value !== 'function') return value
  return (...args: unknown[]) => {
    const result: unknown = Reflect.apply(value, target, args)
    return result
  }
}
