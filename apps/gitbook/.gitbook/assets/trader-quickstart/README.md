# Trader quickstart screenshots

The quickstart now uses current USDG component captures from Storybook, stored in `../screenshots/`. The screenshot map records their stories and article references.

The former legacy testnet and wallet captures were removed because they showed an earlier token and funding flow. Wallet confirmation steps remain in the guide as text; Storybook screenshots use deterministic illustrative USDG values.

Rebuild Storybook and run `node apps/frontend/scripts/capture-gitbook-screenshots.mjs` from the repository root to refresh the mapped images.
