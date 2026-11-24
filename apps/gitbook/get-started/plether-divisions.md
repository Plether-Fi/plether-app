# Plether Divisions (testnet leaderboard)

> **Climb the weekly ranks on Arbitrum Sepolia. Earn persistent tiers. Show them off on Discord.**

**Plether Divisions** is Plether's ongoing weekly testnet ranking at [insights.plether.com](https://insights.plether.com). Every active Trading Account on Arbitrum Sepolia participates automatically—there is no registration gate and no cash prize pool.

### Weekly schedule

Each weekly epoch tracks the global FX trading week:

* **Epoch window:** Sunday **21:00 UTC** (inclusive) to Friday **21:00 UTC** (exclusive).
* **Results publication:** Every Monday at or after **00:00 UTC**.

### How weekly return is scored

Rankings are ordered by exact weekly percentage return on adjusted equity:

```text
Adjusted equity = opening equity + external deposits − external withdrawals
Weekly return   = (closing equity − adjusted equity) ÷ adjusted equity
```

Depositing or withdrawing test USDG mid-week adjusts the denominator rather than inflating your score.

### Qualification rules

To earn a promotion for the week, an account must satisfy **both** conditions:

1. **Strictly positive weekly PnL** after fees, carry and VPI.
2. **At least 2 distinct active FX-session days** (Monday–Friday, grouped by `UTC date(execution_timestamp + 3 hours)`) with voluntary executed position changes.

### Persistent tiers

Tiers carry forward across weeks (including across the Core v2.0.0 MockUSDG upgrade):

| Tier | How you qualify |
| --- | --- |
| **Iron** | At least 1 executed trade in an epoch (awarded on Monday publication) |
| **Bronze** | Qualifying profitable week |
| **Silver** | Top 50% of qualifying profitable traders |
| **Gold** | Top 20% of qualifying profitable traders |
| **Platinum** | Top 10% of qualifying profitable traders |
| **Diamond** | Top 5% of qualifying profitable traders |

**Progression and demotion:**

* You can advance **at most one tier per week** (reaching Diamond from Iron requires 5 qualifying weeks).
* **Two consecutive non-qualifying weeks** demote your account by one tier, down to a floor of **Iron**.

### Link your wallet and Discord (optional)

Your Trading Account ranks automatically even without linking Discord. To display your Discord handle on the public leaderboard:

1. Open [insights.plether.com/register](https://insights.plether.com/register) (or select **Link your wallet** in the testnet top banner).
2. Connect the same owner wallet or Privy account you use on [app.sepolia.plether.com](https://app.sepolia.plether.com).
3. Complete Discord OAuth (`identify` scope), the human-verification check, and the wallet ownership signature.

Each Discord account links to one owner wallet and its deterministic Trading Account. Linking publishes your Discord handle alongside your leaderboard stats. Automated Discord role syncing and social auto-posting are not currently enabled.

### Continue from here

* New to trading? Start with the [Trader quickstart](../trader-quickstart.md).
* Check live standings and past epochs at [insights.plether.com](https://insights.plether.com).
