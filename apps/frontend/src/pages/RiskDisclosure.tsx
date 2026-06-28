export function RiskDisclosure() {
  return (
    <div className="max-w-3xl mx-auto space-y-8 text-sm text-content-secondary">
      <div>
        <h1 className="text-2xl font-bold text-content-primary mb-2">Risk Disclosure</h1>
        <p className="text-xs text-content-secondary/70">Last updated: March 2026</p>
      </div>

      <p>
        Using the Plether protocol involves significant risks. You should not interact with the
        protocol unless you fully understand these risks and are prepared to bear potential
        losses. This disclosure does not cover all possible risks.
      </p>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">1. Smart Contract Risk</h2>
        <p>
          The Plether protocol is governed by smart contracts deployed on the Ethereum
          blockchain. While these contracts have been designed with care, they may contain
          bugs, vulnerabilities, or logic errors that could result in the loss of funds.
          Smart contract interactions are irreversible — once a transaction is confirmed on
          the blockchain, it cannot be undone.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">2. Market Risk</h2>
        <p>
          The value of plDXY-BEAR and plDXY-BULL tokens is derived from the U.S. Dollar Index
          (DXY). These tokens are leveraged instruments whose value can fluctuate significantly.
          You may lose a substantial portion or all of your invested capital. Past performance
          is not indicative of future results.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">3. Oracle Risk</h2>
        <p>
          The protocol relies on price oracles to determine the value of the DXY index.
          Oracles may experience delays, provide inaccurate data, or become temporarily
          unavailable. Oracle failures can result in incorrect pricing, liquidations, or
          inability to execute transactions.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">4. Jurisdictional Risk</h2>
        <p>
          Decentralized finance protocols may be restricted or prohibited in certain
          jurisdictions. Changes in laws or regulations may adversely affect the use, transfer,
          or value of digital assets. You are solely responsible for determining whether your
          use of this protocol complies with all applicable laws and regulations in your
          jurisdiction.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">5. Liquidity Risk</h2>
        <p>
          There may be insufficient liquidity to execute trades at desired prices or to exit
          positions. Low liquidity can result in significant slippage, unfavorable execution
          prices, or the inability to close positions entirely.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">6. Counterparty Risk</h2>
        <p>
          The protocol uses USDC as its base asset. USDC is issued by a centralized entity and
          is subject to its own risks, including potential depegging, regulatory action against
          the issuer, or blacklisting of addresses.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">7. Technology Risk</h2>
        <p>
          The Ethereum network itself may experience congestion, forks, or other disruptions
          that could affect the protocol. Wallet software, browser extensions, and other tools
          you use to interact with the protocol may also contain vulnerabilities.
        </p>
      </section>

      <section className="space-y-3">
        <h2 className="text-lg font-semibold text-content-primary">8. No Guarantee of Returns</h2>
        <p>
          Plether Labs Limited makes no guarantees regarding the performance of the protocol,
          the value of any tokens, or the availability of the Interface. The protocol is
          provided "as is" without any warranties of any kind. You use the protocol entirely
          at your own risk.
        </p>
      </section>
    </div>
  )
}

export default RiskDisclosure
