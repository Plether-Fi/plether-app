# Apply after the deployment/release overlays and the existing explicit AA mode.
# The gateway retains Alchemy as fallback. Preserve the original perps-rpc-url
# parameter so a later Terraform apply keeps the independent backup origin.
application_rpc_url_ssm_parameter_name             = "/plether/sepolia/app-rpc-url"
perps_rpc_auth_token_ssm_parameter_name            = "/plether/sepolia/rpc-gateway-token"
aa_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia/app-rpc-url"

# Alto reads and bundle submissions share the authenticated application gateway.
# Match the immutable image qualified by the protected Alto RPC rollout before
# applying this overlay; the gateway provides Alchemy fallback for both paths.
alto_rpc_url_ssm_parameter_name                  = "/plether/sepolia/app-rpc-url"
alto_rpc_auth_token_ssm_parameter_name           = "/plether/sepolia/rpc-gateway-token"
alto_rpc_auth_image                              = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-alto-sepolia@sha256:1a6c3e820409d3f215da8497acc6f12e9ab930abb4f30fa07ea2116f95e11a1a"
alto_send_transaction_rpc_url_ssm_parameter_name = ""

# Post-cutover scan cadence only. Confirmation depths, batching, gas budgets,
# oracle publication and LP settlement policy keep their existing settings.
keeper_poll_seconds             = "1"
keeper_idle_poll_seconds        = "1"
liquidation_worker_poll_seconds = "2"
protection_worker_poll_seconds  = "2"
perps_indexer_poll_seconds      = "2"
aa_reconciler_poll_seconds      = "2"

# Hints prepare inputs and wake the existing keeper; canonical execution gates
# and the one-second reconciliation fallback remain authoritative.
perps_commitment_wakeups_mode = "active"
