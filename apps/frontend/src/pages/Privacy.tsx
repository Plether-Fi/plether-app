export function Privacy() {
  return (
    <div className="max-w-3xl mx-auto space-y-8 text-sm text-cyber-text-secondary">
      <div>
        <h1 className="text-2xl font-bold text-cyber-text-primary mb-2">Privacy Policy</h1>
        <p className="text-xs text-cyber-text-secondary/70">Last updated: March 2026</p>
      </div>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-cyber-text-primary">1. Information We Collect</h2>
        <p>
          The Plether Interface is designed to minimize data collection. We do not collect
          personally identifiable information (PII). The following data may be processed:
        </p>
        <ul className="list-disc list-inside space-y-1 ml-2">
          <li>Public wallet addresses used to interact with the protocol</li>
          <li>On-chain transaction data (publicly available on the Ethereum blockchain)</li>
          <li>Browser type, device information, and IP address for security and analytics</li>
        </ul>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-cyber-text-primary">2. How We Use Information</h2>
        <p>We use the information described above to:</p>
        <ul className="list-disc list-inside space-y-1 ml-2">
          <li>Display your positions, balances, and transaction history</li>
          <li>Provide protocol analytics and price data</li>
          <li>Maintain security and prevent abuse</li>
          <li>Improve the Interface and user experience</li>
        </ul>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-cyber-text-primary">3. Third-Party Services</h2>
        <p>The Interface integrates with the following third-party services:</p>
        <ul className="list-disc list-inside space-y-1 ml-2">
          <li>
            <span className="text-cyber-text-primary">WalletConnect / Reown</span> — for wallet
            connectivity. Subject to their own privacy policies.
          </li>
          <li>
            <span className="text-cyber-text-primary">RPC Providers (Alchemy)</span> — to read
            from and submit transactions to the Ethereum network.
          </li>
          <li>
            <span className="text-cyber-text-primary">PostHog EU Cloud</span> — for anonymous
            product analytics and sampled, masked session replay.
          </li>
        </ul>
        <p>
          These services may independently collect data according to their own privacy policies.
          We encourage you to review them.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-cyber-text-primary">4. Data Retention</h2>
        <p>
          On-chain data is permanent and publicly accessible on the Ethereum blockchain. We do
          not store wallet addresses or transaction data beyond what is necessary for the
          Interface to function. Local settings (such as slippage preferences) are stored in
          your browser's localStorage and are never transmitted to our servers.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-cyber-text-primary">5. Cookies and Local Storage</h2>
        <p>
          The Interface uses browser localStorage to persist your preferences (slippage settings,
          risk acknowledgment). We do not use tracking cookies. PostHog analytics is configured
          with browser persistence disabled. Third-party services integrated into the Interface may
          set their own cookies.
        </p>
        <p>
          Sampled session replay is masked and is not intended to capture wallet addresses,
          transaction hashes, order IDs, input values, balances, signatures, or permit data.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-cyber-text-primary">6. Your Rights</h2>
        <p>
          Since we do not collect PII, most data subject rights (access, deletion, portability)
          do not apply. You can clear your local preferences at any time by clearing your
          browser's localStorage. On-chain data cannot be deleted as it is part of the public
          blockchain.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-cyber-text-primary">7. Changes to This Policy</h2>
        <p>
          We may update this Privacy Policy from time to time. Changes will be posted on this
          page with an updated revision date.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-cyber-text-primary">8. Contact</h2>
        <p>
          For questions about this Privacy Policy, please contact Plether Labs Limited through
          the channels listed on our website.
        </p>
      </section>
    </div>
  )
}

export default Privacy
