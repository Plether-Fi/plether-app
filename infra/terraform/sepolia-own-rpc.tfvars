# Apply after the deployment/release overlays and the existing explicit AA mode.
# The routing service retains the original Alchemy origin in its own dedicated
# parameter, so replacing this ordinary URL cannot create a fallback loop.
perps_rpc_url                           = "https://rpc-internal.sepolia.plether.com/"
perps_rpc_auth_token_ssm_parameter_name = "/plether/sepolia/rpc-gateway-token"

# Keep the bundler's separately qualified provider while application reads and
# workers move to Nitro. Its existing resolved endpoint must not follow the
# ordinary PERPS_RPC_URL alias on a later task restart.
alto_rpc_url_ssm_parameter_name = "/plether/sepolia/alto-rpc-url"
