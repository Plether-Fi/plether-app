# Issuance-disabled qualification only; no owner unpause or sponsored submissions.
frankfurt_activation_stage             = "aa-qualification"
operations_alarm_sns_topic_arn          = "arn:aws:sns:eu-central-1:932542905614:plether-sepolia-aa-temp-operations"
api_desired_count                      = 1
alto_desired_count                     = 1
aa_reconciler_desired_count            = 1
# Explicitly approved temporary canary allowance; the shared default stays 600.
aa_reconciler_max_safe_lag_seconds      = "1800"
configure_native_aa_backend            = true
enable_native_aa_submission            = false
enable_native_aa_sponsorship           = false
aa_paymaster_address                   = "0x9761091045616A388f5fE1433721B272c78fe31b"
aa_paymaster_signer_address            = "0x015736E1F47E37938236e481F7a3B7c57F922b80"
aa_paymaster_code_hash                 = "0xb8ae276b01850fdbb8d9d7fd32ec7b9b1c7ab7af20f5d62179a76f6b4912c528"
aa_reconciler_start_block              = "307684600"
aa_reconciler_start_block_hash         = "0x64210ad75de20ddd2ccf494e7c4d042447f4ea48119419945637f17c271904fe"
alto_entrypoint_simulation_contract_v8 = "0x9c3c25a084AE8B1df3B2e82bb07Dac4E115C9Ae1"
alto_pimlico_simulation_contract       = "0x95CC02A7B69dD46c6DD6Bd56132A24a235D58948"
