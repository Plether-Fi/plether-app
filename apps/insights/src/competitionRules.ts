const USDC_SCALE = 1_000_000n

interface CompetitionRules {
  minimumProfitUsdc: string
}

const COMPETITION_RULES: Readonly<Partial<Record<string, CompetitionRules>>> = {
  'testnet-trading-2026-09': {
    minimumProfitUsdc: (1n * USDC_SCALE).toString(),
  },
}

export function minimumProfitUsdcForSlug(slug: string): string | undefined {
  return COMPETITION_RULES[slug]?.minimumProfitUsdc
}
