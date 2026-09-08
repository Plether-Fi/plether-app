# Trader quickstart screenshots

Captured from Firefox and its installed MetaMask extension on 2026-09-08, using
[Plether on Arbitrum Sepolia](https://app.sepolia.plether.com/) (application build
`cedad1b34a7d`). These are live UI captures, cropped to the relevant dialog or
panel. Interface text, addresses and values have not been redrawn or replaced.
Page zoom was adjusted for legibility and MetaMask was temporarily set to English.

All seven PNGs are embedded in `apps/gitbook/trader-quickstart.md`. They are
maintained separately from the generated Storybook assets in `../screenshots/`.

| File | Quickstart step | Captured state |
| --- | --- | --- |
| `metamask-connect.png` | 1 | English connection review for the Sepolia site and Account 3 |
| `test-funds.png` | 1 | Faucet targeting the account's derived Trading Account |
| `deposit.png` | 2 | 10,000 MockUSDC deposit from a 100,000 MockUSDC Trading Account balance; owner balance is zero |
| `metamask-deposit.png` | 2 | English sponsored deposit signature request on Arbitrum Sepolia |
| `order-controls.png` | 3 | 10,000 USDC available to trade, long direction, 2,000 plDXY and 2x leverage |
| `commit-preview.png` | 4 | Fully loaded, valid review of the same order |
| `metamask-order.png` | 4 | English signature request for the same sponsored order |

The faucet sent 100,000 valueless MockUSDC to Trading Account
`0xBE988FA5c3c23157DA01807eB6b9afF6FA7cA9e0`. A sponsored 10,000 MockUSDC deposit
was signed and confirmed, producing the displayed 10,000 USDC available balance.
No real funds or owner-wallet gas were used. The order signature request was
cancelled after capture; the illustrated order was not submitted or executed.
The article's later execution, closing and withdrawal steps are instructions,
not a captured transaction history. Live prices and estimates vary slightly
between captures.

To refresh, capture the actual Sepolia interface and MetaMask prompts with a
consistent account and example amount. Crop out unrelated tabs and browser
chrome, preserve real values, and update captions when labels change. Verify
every PNG opens, every article image link resolves, and no pending wallet request
is left behind. Restore temporary language, zoom and account-selection changes.
