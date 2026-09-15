# Overlay only; preserve the current environment's other settings.
# Reviewed all-trader rollout state; apply only after the runbook canary gate.
# For a disabled bootstrap, override both issuance flags to false.
aa_rpc_mode                                        = "dual-independent"
aa_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia/aa-reconciler-secondary-rpc-url"
aa_reconciler_secondary_rpc_url_kms_key_arn        = ""
perps_close_assistance_enabled                     = true
perps_close_assistance_global_enabled              = true
perps_close_assistance_lens                        = "0xC8Ad43019D371DEe7784C06dFa1A2F1538E0D7cf"
perps_close_assistance_lens_code_hash              = "0x7cb66d1cb8f7c6748bd34150207ad1b8ead01ce8e77002dde384160ba3a1333e"
