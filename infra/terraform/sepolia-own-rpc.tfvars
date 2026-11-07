# Apply after the deployment/release overlays and the existing explicit AA mode.
# The routing service retains the original Alchemy origin in its own dedicated
# parameter. The original perps-rpc-url and the existing Alto service stay unchanged.
application_rpc_url_ssm_parameter_name             = "/plether/sepolia/app-rpc-url"
perps_rpc_auth_token_ssm_parameter_name            = "/plether/sepolia/rpc-gateway-token"
aa_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia/app-rpc-url"

# Keep the bundler's separately qualified provider while application reads and
# workers move to a separate application parameter. Do not redirect the
# original perps-rpc-url on a later Terraform apply.
alto_rpc_url_ssm_parameter_name = "/plether/sepolia/perps-rpc-url"
