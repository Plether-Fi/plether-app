dofile('posthog-projection.lua')
for _,reason in ipairs({'INSUFFICIENT_FREE_EQUITY','INVALID_ORDER_DEADLINE'}) do
  local _,_,safe=project_posthog('test',0,{event='aa_request_failed',reason_code=reason,error='private calldata'})
  assert(safe.reason_code==reason and safe.error==nil)
end
local _,_,funding=project_posthog('test',0,{event='worker_funding_observation',diagnostic_code='JOURNAL_DECODE_FAILED',raw_transaction='private'})
assert(funding.diagnostic_code=='JOURNAL_DECODE_FAILED' and funding.raw_transaction==nil)
local _,_,unsafe=project_posthog('test',0,{event='worker_funding_observation',diagnostic_code='private credential'})
assert(unsafe.diagnostic_code==nil)
local _,_,pending = project_posthog('test',0,{event='aa_request_failed',reason_code='ACCOUNT_DEPLOYMENT_PENDING',
  outcome='rejected',sender='0xsecret',error='provider payload'})
assert(pending.reason_code=='ACCOUNT_DEPLOYMENT_PENDING' and pending.outcome=='rejected')
assert(pending.sender==nil and pending.error==nil)
local _,_,recovery = project_posthog('test',0,{event='aa_receipt_recovery',recovery_source='finalized_record',
  reason_code='RECOVERY_EVIDENCE_UNAVAILABLE',attempt_id='12345678-1234-4123-8123-123456789abc',
  transaction_hash='0xsecret',sender='0xsecret',receipt={data='secret'}})
assert(recovery.recovery_source=='finalized_record' and recovery.reason_code=='RECOVERY_EVIDENCE_UNAVAILABLE')
assert(recovery.transaction_hash==nil and recovery.sender==nil and recovery.receipt==nil)
local _,_,gas = project_posthog('test',0,{event='aa_preparation_gas_headroom',gas_headroom_bps=5000,estimated_call_gas=419155,calldata='secret'})
assert(gas.event=='aa_preparation_gas_headroom' and gas.gas_headroom_bps==5000 and gas.estimated_call_gas==nil and gas.calldata==nil)
local _,_,oog = project_posthog('test',0,{event='aa_execution_diagnosed',reason_code='USER_OPERATION_OUT_OF_GAS',trace={input='secret'}})
assert(oog.reason_code=='USER_OPERATION_OUT_OF_GAS' and oog.trace==nil)
local raw = {
  event='keeper_transaction_failed', SeverityText='ERROR', error='secret raw payload',
  order_ids={8}, signer_balance_wei=1234, transaction_hash='0xdead',
  message='insufficient funds 0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
  attempt_id='12345678-1234-4123-8123-123456789abc', duration_ms=12,
  reason_code='KEEPER_INSUFFICIENT_FUNDS',
  resource={attributes={['service.name']='plether-keeper', ['user.address']='secret'}}
}
local code,ts,safe=project_posthog('plether-keeper-firelens-test',123,raw)
assert(code==2 and ts==123 and safe.message=='keeper_transaction_failed')
assert(safe.error==nil and safe.order_ids==nil and safe.signer_balance_wei==nil and safe.transaction_hash==nil)
assert(safe.attempt_id==raw.attempt_id and safe.duration_ms==12 and safe.SeverityNumber==17)
assert(safe.resource.attributes['user.address']==nil)
assert(raw.message:find('0x') and raw.error=='secret raw payload') -- CloudWatch copy unchanged
local _,_,unknown=project_posthog('plether-alto-firelens-test',0,{msg='secret',err={stack='secret'},attempt_id='0xdead',reason_code='phc_secret'})
assert(unknown.message=='unclassified_service_log' and unknown.attempt_id==nil and unknown.reason_code==nil)
for _, malformed in ipairs({'raw secret payload', 123, false, {resource='secret',event={},duration_ms=0/0,reason_code='https://secret.invalid'}}) do
  local _,_,projected = project_posthog('plether-alto-firelens-test',0,malformed)
  assert(type(projected)=='table' and projected.message=='unclassified_service_log')
  assert(projected.reason_code==nil and projected.duration_ms==nil)
end
local broken = setmetatable({}, {__index=function() error('credential payload') end})
local _,_,failure = project_posthog('plether-alto-firelens-test',0,broken)
assert(failure.message=='diagnostic_projection_failed')
assert(failure.error==nil and failure.log==nil)
local _,_,credential = project_posthog('plether-alto-firelens-test',0,{event='AKIAEXAMPLECREDENTIAL',stage='private_alphanumeric_secret',reason_code='MY_PRIVATE_SECRET'})
assert(credential.message=='unclassified_service_log' and credential.stage==nil and credential.reason_code==nil)
print('PostHog projection privacy fixtures passed')
for _,reason in ipairs({'KEEPER_SAME_BLOCK','KEEPER_ENGINE_FAILURE','KEEPER_INSUFFICIENT_GAS'}) do
  local original = { event='keeper_order_deferral_summary', reason_code=reason,
    occurrence_count=4, duration_ms=21000, remaining_deadline_seconds=7,
    attempt_id='12345678-1234-4123-8123-123456789abc', order_id=40,
    order_router='0xsecret', gas_limit=3500000, payload_publish_time=123456789,
    valid_until=123456799, first_observed_at=123456780, last_observed_at=123456788,
    error='private calldata' }
  local _,_,projected=project_posthog('test',0,original)
  assert(projected.event=='keeper_order_deferral_summary' and projected.reason_code==reason)
  assert(projected.occurrence_count==4 and projected.duration_ms==21000 and projected.remaining_deadline_seconds==7)
  assert(projected.attempt_id==original.attempt_id)
  for _,key in ipairs({'order_id','order_router','gas_limit','payload_publish_time','valid_until','first_observed_at','last_observed_at','error'}) do assert(projected[key]==nil) end
  assert(original.order_id==40 and original.error=='private calldata')
end
for _,component in ipairs({'alto','keeper','oracle','liquidation','protection','lp_settlement'}) do
  local operational = {event='worker_funding_observation',component=component,outcome='blocked',reason_code='WORKER_INSUFFICIENT_FUNDS',occurrence_count=2,
    balance_wei='12345',liability_wei='567',reserve_wei='89',signer_address='0xdead',raw_transaction='0xsecret'}
  local _,_,exported = project_posthog('plether-funding-monitor-firelens-test',0,operational)
  assert(exported.event=='worker_funding_observation' and exported.component==component and exported.reason_code=='WORKER_INSUFFICIENT_FUNDS')
  assert(exported.balance_wei==nil and exported.liability_wei==nil and exported.reserve_wei==nil and exported.signer_address==nil and exported.raw_transaction==nil)
  assert(operational.balance_wei=='12345' and operational.signer_address=='0xdead')
end

for _,event in ipairs({'oracle_sync_lag','oracle_sync_pending','oracle_update_mined','oracle_update_not_needed',
  'oracle_update_payload_stale','oracle_update_dry_run','oracle_worker_started','oracle_worker_iteration_failed','oracle_worker_fatal'}) do
  local original = {event=event, lag_seconds=5, previous_lag_seconds=10, poll_seconds=30, health_poll_seconds=5,
    repair=true, synchronized=false, transaction_hash='0xsecret', update_fee_wei='123', error='raw provider secret',
    updateData={'signed payload'}, updater_address='0xsecret'}
  local _,_,projected = project_posthog('test',0,original)
  assert(projected.event==event and projected.message==event)
  assert(projected.lag_seconds==5 and projected.previous_lag_seconds==10)
  assert(projected.poll_seconds==30 and projected.health_poll_seconds==5)
  assert(projected.repair==true and projected.synchronized==false)
  assert(projected.transaction_hash==nil and projected.error==nil and projected.updateData==nil and projected.updater_address==nil)
  assert(original.error=='raw provider secret')
end
local _,_,badLag = project_posthog('test',0,{event='oracle_sync_lag',lag_seconds='secret',repair='secret',synchronized={private='secret'}})
assert(badLag.lag_seconds==nil and badLag.repair==nil and badLag.synchronized==nil)
