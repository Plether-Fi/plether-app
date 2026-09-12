# Staging overlay, not a complete deployment file or release approval.
# Supply verified signer/anchor/simulation inputs and the Core v1.2.3 overlay.
# Public policy is explicit; new issuance and preparation remain disabled.
provision_self_hosted_aa = true
# Approved testnet tracing exception; does not skip ordinary validation.
alto_sepolia_safe_mode_exception = true
configure_native_aa_backend      = true
alto_desired_count               = 1
aa_reconciler_desired_count      = 1
aa_native_global_rollout_enabled = true
aa_native_canary_owners          = ""
enable_native_aa_sponsorship     = false
enable_native_aa_preparation     = false
enable_native_aa_submission      = false
enable_aa_readiness_enforcement  = false
