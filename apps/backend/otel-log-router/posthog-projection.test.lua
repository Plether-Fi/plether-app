dofile('posthog-projection.lua')
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
for _,component in ipairs({'alto','keeper','oracle','liquidation','protection','lp_settlement'}) do
  local operational = {event='worker_funding_observation',component=component,outcome='blocked',reason_code='WORKER_INSUFFICIENT_FUNDS',occurrence_count=2,
    balance_wei='12345',liability_wei='567',reserve_wei='89',signer_address='0xdead',raw_transaction='0xsecret'}
  local _,_,exported = project_posthog('plether-funding-monitor-firelens-test',0,operational)
  assert(exported.event=='worker_funding_observation' and exported.component==component and exported.reason_code=='WORKER_INSUFFICIENT_FUNDS')
  assert(exported.balance_wei==nil and exported.liability_wei==nil and exported.reserve_wei==nil and exported.signer_address==nil and exported.raw_transaction==nil)
  assert(operational.balance_wei=='12345' and operational.signer_address=='0xdead')
end
