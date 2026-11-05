export function Privacy() {
  return (
    <div className="page-stack max-w-3xl mx-auto text-sm text-content-secondary">
      <div>
        <h1 className="text-2xl font-bold text-content-primary mb-2">Privacy Policy</h1>
        <p className="text-xs text-content-secondary/70">Last updated: November 2026</p>
      </div>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">1. Information We Collect</h2>
        <p>
          The Plether Interface is designed to minimize data collection. The following
          data may be processed when you use the Interface:
        </p>
        <ul className="list-disc list-inside space-y-1 ml-2">
          <li>Public wallet addresses used to interact with the protocol</li>
          <li>Email addresses or social account identifiers when you choose those login methods</li>
          <li>Authentication session information used by Privy to provide login and embedded wallets</li>
          <li>On-chain transaction data (publicly available on the Ethereum blockchain)</li>
          <li>Browser type, device information, and IP address for security and analytics</li>
        </ul>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">2. How We Use Information</h2>
        <p>We use the information described above to:</p>
        <ul className="list-disc list-inside space-y-1 ml-2">
          <li>Display your positions, balances, and transaction history</li>
          <li>Authenticate your chosen login method and provide access to your embedded wallet</li>
          <li>Provide protocol analytics and price data</li>
          <li>Maintain security and prevent abuse</li>
          <li>Improve the Interface and user experience</li>
        </ul>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">3. Third-Party Services</h2>
        <p>The Interface integrates with the following third-party services:</p>
        <ul className="list-disc list-inside space-y-1 ml-2">
          <li>
            <span className="text-content-primary">Privy</span> — for email and social login,
            authentication sessions, and embedded wallets.
          </li>
          <li>
            <span className="text-content-primary">WalletConnect</span> — for connecting
            supported external wallets through Privy.
          </li>
          <li>
            <span className="text-content-primary">RPC Providers (Alchemy)</span> — to read
            from and submit transactions to the Ethereum network.
          </li>
          <li>
            <span className="text-content-primary">PostHog EU Cloud</span> — for anonymous
            product analytics and sampled, masked session replay.
          </li>
        </ul>
        <p>
          These services may independently collect data according to their own privacy policies.
          We encourage you to review them.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">4. Data Retention</h2>
        <p>
          On-chain data is permanent and publicly accessible on the Ethereum blockchain. We do
          not store wallet addresses or transaction data beyond what is necessary for the
          Interface to function. Local settings (such as slippage preferences) are stored in
          your browser's localStorage and are never transmitted to our servers. Privy manages
          authentication and linked-account data under its own retention policy.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">5. Cookies and Local Storage</h2>
        <p>
          The Interface uses browser localStorage to persist your preferences (slippage settings,
          risk acknowledgment). We do not use tracking cookies. PostHog analytics is configured
          with browser persistence disabled. Third-party services integrated into the Interface may
          set their own cookies or store authentication sessions locally, including Privy.
        </p>
        <p>
          Sampled session replay is masked and is not intended to capture wallet addresses,
          transaction hashes, order IDs, input values, balances, signatures, or permit data.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">6. Your Rights</h2>
        <p>
          You can log out and clear your local preferences by clearing your browser's
          storage. For requests about personal information associated with login, contact
          us using the details below; Privy also provides information about its processing
          in its own privacy policy. On-chain data cannot be deleted as it is part of the
          public blockchain.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">7. Changes to This Policy</h2>
        <p>
          We may update this Privacy Policy from time to time. Changes will be posted on this
          page with an updated revision date.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">8. Contact</h2>
        <p>
          For questions about this Privacy Policy, please contact Plether Labs Limited through
          the channels listed on our website.
        </p>
      </section>
    </div>
  )
}

export default Privacy
