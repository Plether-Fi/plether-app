# Preparation overlay only: not a complete environment variable file.
# Nothing is provisioned or activated by this profile. Before any plan/apply,
# supply approved owner/RPC inputs and follow self-hosted-aa-rollout.md.
environment             = "sepolia"
aws_region              = "ap-southeast-1"
expected_aws_account_id = "932542905614"
perps_chain_id          = "421614"

provision_self_hosted_aa         = false
configure_native_aa_backend      = false
enable_native_aa_sponsorship     = false
enable_native_aa_submission      = false
aa_native_global_rollout_enabled = false
aa_native_canary_owners          = "0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B" # User-approved test owner; issuance remains disabled.
alto_desired_count               = 0
aa_reconciler_desired_count      = 0

# User-approved reuse of the backend RPC; no duplicated credential to rotate.
alto_rpc_url_ssm_parameter_name                    = "/plether/sepolia/perps-rpc-url"
alto_executor_private_keys_ssm_parameter_name      = "/plether/sepolia/alto-executor-private-keys"
alto_utility_private_key_ssm_parameter_name        = "/plether/sepolia/alto-utility-private-key"
alto_send_transaction_rpc_url_ssm_parameter_name   = ""
alto_secrets_kms_key_arn                           = ""
aa_rpc_mode                                        = "single-provider-sepolia"
aa_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia/perps-rpc-url"
aa_reconciler_secondary_rpc_url_kms_key_arn        = ""

# User-approved 10x preparation ceilings, subject to live estimation qualification.
# These are sponsorship limits, not transfers or a total wallet funding budget.
aa_paymaster_max_cost_wei                = "10000000000000000"  # 0.01 ETH per operation
aa_paymaster_account_outstanding_wei     = "20000000000000000"  # 0.02 ETH
aa_paymaster_client_outstanding_wei      = "20000000000000000"  # 0.02 ETH
aa_paymaster_global_outstanding_wei      = "20000000000000000"  # 0.02 ETH
aa_paymaster_account_hourly_wei          = "50000000000000000"  # 0.05 ETH
aa_paymaster_global_hourly_wei           = "50000000000000000"  # 0.05 ETH
aa_paymaster_global_daily_wei            = "100000000000000000" # 0.1 ETH
aa_paymaster_min_deposit_wei             = "50000000000000000"  # Pause below 0.05 ETH
aa_sponsored_gas_alert_wei_per_hour      = "20000000000000000"  # Alert at 0.02 ETH/hour
aa_paymaster_final_rate_limit_per_minute = "20"

# Keep the existing reviewed refill floor pending measured bundle costs.
alto_min_executor_balance_wei = "5000000000000000"
