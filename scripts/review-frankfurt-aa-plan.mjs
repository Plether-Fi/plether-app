#!/usr/bin/env node
// Reads a private plan in memory and emits only non-secret review metadata.
import { execFileSync } from 'node:child_process'
import { readFileSync, statSync } from 'node:fs'
import { createHash } from 'node:crypto'
import { resolve, join } from 'node:path'
import assert from 'node:assert/strict'

const staging = process.argv[2]
if (!staging || process.argv.length !== 3) throw new Error('Provide the private plan workspace')
const directory = join(resolve(staging), 'infra/terraform')
const file = join(directory, 'frankfurt.tfplan')
assert.equal(statSync(file).mode & 0o077, 0, 'Plan permissions must be operator-only')
const plan = JSON.parse(execFileSync(process.env.TERRAFORM_BIN || 'terraform', ['show', '-json', file], {
  cwd: directory, env: { ...process.env, AWS_PROFILE: 'plether', TF_DATA_DIR: join(resolve(staging), 'terraform-data') },
  encoding: 'utf8', maxBuffer: 32 * 1024 * 1024,
}))
assert.equal(plan.errored, false, 'Reject incomplete/error plans')
const target = plan.planned_values.outputs.deployment_target.value
assert.deepEqual(target, { id: 'sepolia-aa-temp', region: 'eu-central-1', account_id: '932542905614',
  chain_id: '421614', preparation_only: true })
const managed = plan.resource_changes.filter(change => change.mode === 'managed')
for (const change of managed) {
  assert.deepEqual(change.change.actions, ['create'], `Not a creation: ${change.address}`)
  assert(change.change.before === null, `Existing resource: ${change.address}`)
  const after = JSON.stringify(change.change.after)
  assert(!after.includes(':ap-southeast-1:932542905614:') && !after.includes('/plether/sepolia/'),
    `Singapore resource reference: ${change.address}`)
  assert(!['aws_iam_openid_connect_provider', 'aws_route53_record', 'aws_route53_zone'].includes(change.type), `Shared DNS/OIDC mutation: ${change.address}`)
  if (['aws_iam_role', 'aws_kms_alias', 'aws_ecr_repository', 'aws_ssm_parameter', 'aws_ecs_cluster', 'aws_lb'].includes(change.type)) {
    assert(change.change.after.name.includes('sepolia-aa-temp'), `Wrong namespace: ${change.address}`)
  }
}
const services = managed.filter(change => change.type === 'aws_ecs_service').map(change => {
  assert.equal(change.change.after.desired_count, 0, `Service activation: ${change.address}`)
  return { name: change.change.after.name, desiredCount: 0 }
})
assert.equal(services.length, 10, 'Expected exact full dormant backend and AA service set')
const vars = plan.variables
assert.equal(vars.api_hostname.value, '', 'Superseded custom-hostname plan')
assert.equal(vars.alb_certificate_arn.value, '', 'Superseded TLS certificate plan')
const resource = address => {
  const found = managed.find(change => change.address === address)
  assert(found, `Missing resource: ${address}`)
  return found.change.after
}
assert.equal(resource('aws_lb.api').internal, true, 'API must not be public')
assert.equal(managed.filter(change => change.type === 'aws_lb_listener' && change.change.after.protocol === 'HTTPS').length, 0)
assert.equal(managed.filter(change => change.type.startsWith('aws_acm_')).length, 0)
const apiIngress = resource('aws_security_group.alb').ingress
assert.equal(apiIngress.length, 1)
assert.equal(apiIngress[0].from_port, 80)
assert.equal(apiIngress[0].to_port, 80)
assert.equal(apiIngress[0].protocol, 'tcp')
for (const key of ['cidr_blocks', 'ipv6_cidr_blocks', 'prefix_list_ids']) assert.equal(apiIngress[0][key]?.length ?? 0, 0)
assert.equal(apiIngress[0].self, false)
assert.deepEqual(resource('aws_security_group.frankfurt_tunnel[0]').ingress, [])
assert.equal(managed.filter(change => change.type === 'aws_instance').length, 1)
const relay = resource('aws_instance.frankfurt_tunnel[0]')
assert.equal(relay.instance_type, 't4g.nano')
assert.equal(relay.key_name ?? null, null)
assert.equal(relay.metadata_options[0].http_tokens, 'required')
assert.equal(relay.metadata_options[0].http_put_response_hop_limit, 1)
assert.equal(relay.root_block_device[0].encrypted, true)
assert.equal(relay.root_block_device[0].volume_size, 8)
assert.deepEqual(JSON.parse(resource('aws_iam_role_policy.frankfurt_tunnel[0]').policy).Statement[0].Action.sort(),
  ['ssm:UpdateInstanceInformation', 'ssmmessages:CreateControlChannel', 'ssmmessages:CreateDataChannel',
    'ssmmessages:OpenControlChannel', 'ssmmessages:OpenDataChannel'].sort())
assert.equal(resource('aws_ssm_document.frankfurt_tunnel[0]').name, 'plether-sepolia-aa-temp-api-tunnel')
assert.equal(resource('aws_ssm_document.frankfurt_tunnel[0]').document_type, 'Session')
for (const key of ['configure_native_aa_backend', 'enable_native_aa_sponsorship', 'enable_native_aa_submission',
  'aa_native_global_rollout_enabled', 'protection_worker_execution_enabled', 'aa_protection_commits_enabled']) {
  assert.equal(vars[key].value, false, `Activation flag: ${key}`)
}
assert.equal(vars.perps_order_router.value.toLowerCase(), '0x6215d36fcbd610ca1525252eebcbfd8b223a6072')
assert.equal(vars.perps_cfd_engine.value.toLowerCase(), '0xafece93321be41aa73474457e2f47cf7b2fb738f')
assert.equal(vars.perps_indexer_start_block.value, '307397196')
console.log(JSON.stringify({
  planSha256: createHash('sha256').update(readFileSync(file)).digest('hex'),
  target, creates: managed.length, changes: 0, destroys: 0, services,
  types: Object.fromEntries([...new Set(managed.map(change => change.type))].sort().map(type =>
    [type, managed.filter(change => change.type === type).length])),
  coreRelease: 'v1.2.3', sharedOidcAndDnsManaged: false, access: 'private-alb-via-ssm-relay', reviewedPlanOnly: true,
}, null, 2))
