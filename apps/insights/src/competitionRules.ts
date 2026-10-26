const ASSET_SCALE = 1_000_000n

interface CompetitionRules {
  minimumProfitAssets: string
}

const COMPETITION_RULES: Readonly<Partial<Record<string, CompetitionRules>>> = {
  'testnet-trading-2026-09': {
    minimumProfitAssets: (1n * ASSET_SCALE).toString(),
  },
}

export function minimumProfitAssetsForSlug(slug: string): string | undefined {
  return COMPETITION_RULES[slug]?.minimumProfitAssets
}
