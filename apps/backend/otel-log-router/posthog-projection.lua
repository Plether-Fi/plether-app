-- This filter runs ONLY on the PostHog copy. CloudWatch retains its original
-- record and metric-filter fields. Never forward arbitrary bodies or nested data.
local categories = {
  event=true, component=true, stage=true, reason_code=true, outcome=true,
  action_kind=true, sponsorship_status=true, terminal_outcome=true,
  deployment_name=true, rpc_role=true, wallet_family=true,
}
local function set(words)
  local values = {}
  for value in words:gmatch("%S+") do values[value] = true end
  return values
end
-- Values as well as keys are allowlisted. An arbitrary alphanumeric exception
-- or provider credential must not become an event, stage or reason label.
local events = set([[
aa_attempt_prepared aa_request_failed aa_recovery_outcome aa_order_committed
aa_preparation_gas_headroom aa_execution_diagnosed
worker_funding_observation worker_funding_monitor_failed
aa_order_execution_attempt_failed aa_diagnostic_export_dropped aa_diagnostic_queue_full
aa_diagnostic_link_unavailable aa_preparation_timing aa_native_signer_failure
aa_native_reconciler_stale aa_native_issuance_unavailable aa_native_database_failure
aa_reconciler_heartbeat aa_reconciler_rpc_unavailable aa_reconciler_provider_disagreement
aa_reconciler_failure_threshold_exceeded aa_reconciler_paymaster_low_deposit
aa_reconciler_paymaster_unstaked aa_reconciler_unknown_operation
aa_reconciler_cost_exceeds_reservation aa_reconciler_cursor_discontinuity
aa_reconciler_safe_block_advanced aa_reconciler_timestamp_invalid
keeper_transaction_failed keeper_transaction_mined keeper_transaction_deferred
keeper_order_failed keeper_order_finalized_failed keeper_readiness_unavailable keeper_funding_low
keeper_queue_head_refresh_failed keeper_queue_context_fetch_failed keeper_batch_refresh_failed
keeper_cached_payload_decode_failed keeper_cached_payload_invalid keeper_order_logs_fetch_failed
keeper_frozen_close_payload_failed keeper_chain_head_fetch_failed keeper_commit_metadata_fetch_failed
order_keeper_worker_restarting order_keeper_worker_stopped
lp_settlement_heartbeat lp_settlement_low_balance lp_settlement_transaction_unaffordable
lp_settlement_transaction_unavailable lp_settlement_pending_stuck lp_settlement_reconciliation_failed
lp_settlement_broadcast_uncertain lp_settlement_receipt_reorged lp_settlement_invariant_failure
lp_settlement_dependency_unknown lp_settlement_operationally_blocked lp_settlement_ready_backlog
lp_settlement_confirmed lp_settlement_broadcast lp_settlement_worker_restarting lp_settlement_worker_stopped
protection_signer_low_balance protection_worker_heartbeat protection_worker_failed
protection_worker_startup_failed protection_reorg liquidation_worker_heartbeat
liquidation_worker_failed liquidation_worker_startup_failed oracle_worker_failed
api_started rpc_request_failed rpc_request_completed
]])
local values = {
  component=set('keeper funding oracle readiness sponsorship reconciliation bundler paymaster liquidation protection lp_settlement alto'),
  stage=set('prepared preparation authorization estimation signing persistence gateway submitted submitting included committed execution execution_attempt_failed user_operation_confirmed user_operation_reverted authorization_expired recovery'),
  outcome=set('ready blocked unknown rejected failure success confirmed expired pending'),
  action_kind=set('deposit withdraw place-order open close protection'),
  sponsorship_status=set('building requesting-stub estimating requesting-sponsorship awaiting-signature journaling submitting confirming confirmed failed cancelled execution-reverted dropped replaced expired receipt-timeout'),
  terminal_outcome=set('confirmed failed cancelled execution-reverted dropped replaced expired preflight_failed'),
  deployment_name=set('sepolia mainnet'),
  rpc_role=set('api-core api-perps keeper oracle liquidation protection aa-reconciler'),
  reason_code=set([[
READY FUNDING_LOW FUNDING_UNVERIFIED KEEPER_INSUFFICIENT_FUNDS KEEPER_RPC_TIMEOUT
WORKER_INSUFFICIENT_FUNDS
KEEPER_EXECUTION_FAILED READINESS_UNAVAILABLE WORKER_HEARTBEAT_STALE ORACLE_UNAVAILABLE
OPEN_EXECUTION_UNAVAILABLE EXIT_MODE_REQUIRES_VALIDATION BUNDLER_UNAVAILABLE
PROTECTION_TRIGGER_UNAVAILABLE
SPONSORSHIP_DISABLED PAYMASTER_PAUSED RECONCILIATION_STALE RECONCILER_STALE
POLICY_DENIED ACCOUNT_NOT_TRUSTED RATE_LIMITED INVALID_REQUEST PROXY_AUTH_FAILED
SPONSOR_BUDGET_EXCEEDED PER_OPERATION_BUDGET_EXCEEDED OUTSTANDING_BUDGET_EXCEEDED
ACCOUNT_BUDGET_EXCEEDED HOURLY_BUDGET_EXCEEDED DAILY_BUDGET_EXCEEDED
PREPARATION_DISABLED PREPARATION_EXPIRED PREPARATION_BUSY PREPARATION_CONFLICT PREPARATION_LEASE_LOST
SIGNER_UNAVAILABLE DATABASE_UNAVAILABLE SPONSOR_UNAVAILABLE SIMULATION_FAILED
AUTHORIZATION_EXPIRED USER_OPERATION_REVERTED SECURITY_ATTESTATION_UNAVAILABLE DEADLINE_TOO_CLOSE
USER_OPERATION_OUT_OF_GAS
]]),
}
local numbers = {
  duration_ms=true, retry_count=true, occurrence_count=true, suppressed_count=true,
  http_status=true, request_count=true, failure_count=true,
  gas_headroom_bps=true, gas_utilization_bps=true,
}
local resources = {
  ["service.name"]=true, ["service.version"]=true,
  ["deployment.environment.name"]=true, ["cloud.region"]=true,
  ["cloud.provider"]=true, ["cloud.platform"]=true,
}
local function category(value)
  return type(value) == "string" and #value <= 96
    and value:match("^[A-Za-z_][A-Za-z0-9_.%-]*$")
    and not value:find("0x[%da-fA-F]+") and not value:match("^ph[ctx]_")
end
local function uuid(value)
  return type(value) == "string" and #value == 36
    and value:match("^[%da-fA-F]+%-%x+%-4%x+%-[89aAbB]%x+%-%x+$")
end
local function projection(record)
  if type(record) ~= "table" then return {event="unclassified_service_log",message="unclassified_service_log",SeverityText="WARN",SeverityNumber=13} end
  local output = {}
  local event = type(record.event) == 'string' and events[record.event] and record.event or "unclassified_service_log"
  output.event = event
  -- Developer-owned event names are the only allowed body source; no raw Pino
  -- msg/error/err/stack, exception message, RPC URL or calldata is exported.
  output.message = event
  local severity = record.SeverityText
  if severity ~= "TRACE" and severity ~= "DEBUG" and severity ~= "INFO"
    and severity ~= "WARN" and severity ~= "ERROR" and severity ~= "FATAL" then severity = "INFO" end
  output.SeverityText = severity
  output.SeverityNumber = ({TRACE=1, DEBUG=5, INFO=9, WARN=13, ERROR=17, FATAL=21})[severity]
  for key,_ in pairs(categories) do
    if values[key] and type(record[key]) == 'string' and values[key][record[key]] then output[key] = record[key] end
  end
  for key,_ in pairs(numbers) do
    local value = record[key]
    if type(value) == "number" and value == value and value >= 0 and value < 1e12 then output[key] = value end
  end
  if uuid(record.attempt_id) then output.attempt_id = record.attempt_id end
  local attrs = type(record.resource) == "table" and record.resource.attributes or nil
  if type(attrs) == "table" then
    local safe = {}
    for key,_ in pairs(resources) do if category(attrs[key]) then safe[key] = attrs[key] end end
    output.resource = { attributes = safe }
  end
  return output
end

function project_posthog(tag, timestamp, record)
  -- Fluent Bit's protected_mode normally preserves the original record after
  -- a Lua error. Catch errors here so even malformed input cannot take that
  -- fail-open path and send its unsanitized payload to PostHog.
  local ok, output = pcall(projection, record)
  if not ok then
    output = {event="diagnostic_projection_failed",message="diagnostic_projection_failed",SeverityText="ERROR",SeverityNumber=17}
  end
  return 2, timestamp, output
end
