# Temporary, explicitly approved Sepolia-only provider policy.
# Apply after the current environment/release overlays; preserve their rollout
# cohort, sponsorship budgets, contract bindings, and safe-block checks.
# Both RPC slots reuse the existing Alchemy SecureString without copying it.
aa_rpc_mode                                        = "single-provider-sepolia"
aa_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia/perps-rpc-url"
aa_reconciler_secondary_rpc_url_kms_key_arn        = ""

# Return to sepolia-close-assistance.tfvars' dual-independent settings only
# after a second provider qualifies for the required historical safe-state reads.
