# Dormant resources only. Not an activation or funding authorization.
deployment_id           = "sepolia-aa-temp"
environment             = "sepolia"
aws_region              = "eu-central-1"
expected_aws_account_id = "932542905614"
perps_chain_id          = "421614"

api_desired_count                   = 0
consolidate_workers                 = true
workers_desired_count               = 0
liquidation_worker_desired_count    = 0
protection_worker_desired_count     = 0
protection_worker_execution_enabled = false
aa_protection_commits_enabled       = false
lp_settlement_mode                  = "off"
provision_aa_proxy                  = false
provision_self_hosted_aa            = true
configure_native_aa_backend         = false
enable_native_aa_sponsorship        = false
enable_native_aa_submission         = false
aa_native_global_rollout_enabled    = false
aa_native_canary_owners             = "0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B"
alto_desired_count                  = 0
aa_reconciler_desired_count         = 0

aa_rpc_mode                                        = "single-provider-sepolia"
alto_rpc_url_ssm_parameter_name                    = "/plether/sepolia-aa-temp/perps-rpc-url"
aa_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia-aa-temp/perps-rpc-url"
alto_executor_private_keys_ssm_parameter_name      = "/plether/sepolia-aa-temp/alto-executor-private-keys"
alto_utility_private_key_ssm_parameter_name        = "/plether/sepolia-aa-temp/alto-utility-private-key"
pyth_api_key_ssm_parameter_name                    = "/plether/sepolia-aa-temp/pyth-api-key"

aa_paymaster_max_cost_wei                = "10000000000000000"
aa_paymaster_account_outstanding_wei     = "20000000000000000"
aa_paymaster_client_outstanding_wei      = "20000000000000000"
aa_paymaster_global_outstanding_wei      = "20000000000000000"
aa_paymaster_account_hourly_wei          = "50000000000000000"
aa_paymaster_global_hourly_wei           = "50000000000000000"
aa_paymaster_global_daily_wei            = "100000000000000000"
aa_paymaster_min_deposit_wei             = "50000000000000000"
aa_sponsored_gas_alert_wei_per_hour      = "20000000000000000"
aa_paymaster_final_rate_limit_per_minute = "20"
alto_min_executor_balance_wei            = "5000000000000000"

perps_order_lifecycle_book   = "0x753eb48305ffb88bb70869ade2c4efa941879221"
cors_origins                 = "http://127.0.0.1:5173 http://localhost:5173"
db_allocated_storage         = 20
db_storage_type              = "gp3"
db_snapshot_identifier       = null
db_deletion_protection       = true
db_skip_final_snapshot       = false
db_final_snapshot_identifier = "plether-sepolia-aa-temp-final-20260910"

# No custom DNS, public API listener or certificate. Access through Session Manager.
api_hostname            = ""
alb_certificate_arn     = ""
frankfurt_tunnel_ami_id = "ami-08295554222e9a438"

# Real credentials are injected from the operator-only bootstrap SecureString.
# An alarm destination and funding allocations are still not configured.
