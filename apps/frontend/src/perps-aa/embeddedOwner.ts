import { isAddress, isAddressEqual, type Address } from 'viem'

interface EmbeddedAccountSnapshot {
  address?: string
  embeddedWalletInfo?: { accountType?: string }
}

export class UnsupportedEmbeddedOwnerError extends Error {
  constructor() {
    super('Your email/social wallet is using Smart Account mode, which cannot authorize this Trading Account. Open wallet settings, choose Smart Account Settings, then “Switch to your EOA”. This selects a different owner and Trading Account; existing balances and pending activity stay with the original account.')
    this.name = 'UnsupportedEmbeddedOwnerError'
  }
}

/** AppKit remembers account choices, so the EOA default alone cannot protect
 * returning users from deriving and funding an unsupported nested account. */
export function assertEmbeddedPerpsOwner(
  account: EmbeddedAccountSnapshot | undefined,
  ownerAddress: Address,
): void {
  if (!account?.address || !isAddress(account.address) || !isAddressEqual(account.address, ownerAddress)) {
    throw new Error('The embedded wallet account is still reconnecting')
  }
  if (account.embeddedWalletInfo?.accountType === 'smartAccount') {
    throw new UnsupportedEmbeddedOwnerError()
  }
  if (account.embeddedWalletInfo?.accountType !== 'eoa') {
    throw new Error('The embedded wallet account type is not available yet')
  }
}
