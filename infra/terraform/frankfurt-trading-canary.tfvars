# Explicit extension of the approved single-wallet AA canary, not a Core deploy.
frankfurt_activation_stage = "trading-canary"
workers_desired_count = 1
liquidation_worker_desired_count = 1
protection_worker_desired_count = 1
protection_worker_execution_enabled = true
aa_protection_commits_enabled = true
lp_settlement_mode = "off"
# Set funding confirmation only after the dedicated signer passes preflight.
lp_settlement_max_tx_cost_wei = "250000000000000"
lp_settlement_max_drain_transactions = 4
faucet_client_requests_per_hour = 5
faucet_global_requests_per_hour = 10
# Keep the stored mark inside the frontend's 60-second freshness window.
perps_oracle_updater_poll_seconds = "30"
