# Apply after the deployment/release overlays and the existing explicit AA mode.
# The gateway retains Alchemy in its dedicated backup parameter. Preserve the
# original perps-rpc-url so the recorded pre-cutover Alto revision can roll back.
application_rpc_url_ssm_parameter_name             = "/plether/sepolia/app-rpc-url"
perps_rpc_auth_token_ssm_parameter_name            = "/plether/sepolia/rpc-gateway-token"
aa_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia/app-rpc-url"

# Alto uses the same authenticated primary/backup gateway as application reads.
# Select this immutable preload image only after the protected RPC qualification;
# a separate send RPC would bypass the gateway's primary/backup routing.
alto_rpc_url_ssm_parameter_name                  = "/plether/sepolia/app-rpc-url"
alto_rpc_auth_token_ssm_parameter_name           = "/plether/sepolia/rpc-gateway-token"
alto_rpc_auth_image                              = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-alto-sepolia@sha256:1231d00c0a6b42cf2e1bad9044a8c284068a5721e44bb4008419fddaa8bc9904"
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
