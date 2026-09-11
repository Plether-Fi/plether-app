import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { frankfurtLocalProfile } from './aa-frankfurt-local.ts'
import { outputs } from './aa-frankfurt-fixture.mjs'

const read = path => readFileSync(new URL(`../${path}`, import.meta.url), 'utf8')
const source = JSON.parse(read('apps/frontend/public/perps-aa-manifest.json'))
const env = {
  AA_PROXY_ORIGIN_TOKEN: '0123456789abcdef'.repeat(4), // synthetic only
  AA_FRANKFURT_PAYMASTER_ADDRESS: `0x${'12'.repeat(20)}`,
}

test('protection heartbeat metric matches a JSON boolean, not a string', () => {
  assert.match(read('infra/terraform/protection.tf'), /\$\.caughtUp IS TRUE/)
  assert.doesNotMatch(read('infra/terraform/protection.tf'), /\$\.caughtUp = true/)
})

test('Frankfurt CORS origins use the backend space-delimited configuration contract', () => {
  const raw = /cors_origins\s*=\s*"([^"]+)"/.exec(read('infra/terraform/frankfurt-aa-preparation.tfvars'))?.[1]
  assert(raw)
  assert.match(read('apps/backend/src/Plether/Config.hs'), /T\.splitOn " " \$ T\.pack corsStr/)
  assert.deepEqual(raw.split(' ').map(x => x.trim()).filter(Boolean),
    ['http://127.0.0.1:5173', 'http://localhost:5173'])
  assert(read('infra/terraform/deployment_target.tf').includes(`var.cors_origins == "${raw}"`))
})

test('local native manifest uses v1.2.3, never alters the public manifest and defaults issuance off', () => {
  const before = JSON.stringify(source)
  const profile = frankfurtLocalProfile(env, outputs, source)
  assert.equal(profile.target, 'http://127.0.0.1:18081')
  assert.equal(profile.manifest.bundlerRpcUrl, '/api/perps/v1/aa/rpc')
  assert.equal(profile.manifest.paymasterRpcUrl, '/api/perps/v1/aa/rpc')
  assert.equal(profile.manifest.paymasterVersion, 'plether-verifying-v1')
  assert.equal(profile.manifest.sponsorshipEnabled, false)
  assert.equal(profile.manifest.pimlicoRpcUrl, undefined)
  assert.equal(JSON.stringify(source), before)
  assert(!JSON.stringify(profile).includes(env.AA_PROXY_ORIGIN_TOKEN))
  assert.equal(frankfurtLocalProfile({ ...env, AA_FRANKFURT_SPONSORSHIP_ENABLED: 'true' }, outputs, source).manifest.sponsorshipEnabled, true)
})

test('API-only profile needs no signer or paymaster and cannot issue sponsorship', () => {
  const profile = frankfurtLocalProfile({ AA_FRANKFURT_MODE: 'api-readonly' }, outputs, source)
  assert.equal(profile.apiOnly, true)
  assert.equal(profile.manifest.sponsorshipEnabled, false)
  assert.equal(profile.manifest.paymasterAddress, undefined)
  assert.equal(profile.manifest.pimlicoRpcUrl, '/api/perps/v1/aa/pimlico')
  assert.throws(() => frankfurtLocalProfile({ AA_FRANKFURT_MODE: 'api-readonly', AA_FRANKFURT_SPONSORSHIP_ENABLED: 'true' }, outputs, source))
  assert.throws(() => frankfurtLocalProfile({ AA_FRANKFURT_MODE: 'unknown' }, outputs, source))
})

test('preparation capability requires explicit native sponsorship opt-in', () => {
  assert.equal(frankfurtLocalProfile(env, outputs, source).manifest.preparationRpcVersion, undefined)
  assert.throws(() => frankfurtLocalProfile({ ...env, AA_FRANKFURT_PREPARATION_ENABLED: 'true' }, outputs, source))
  assert.throws(() => frankfurtLocalProfile({ ...env, AA_FRANKFURT_PREPARATION_ENABLED: 'yes' }, outputs, source))
  const enabled = { ...env, AA_FRANKFURT_SPONSORSHIP_ENABLED: 'true', AA_FRANKFURT_PREPARATION_ENABLED: 'true' }
  assert.equal(frankfurtLocalProfile(enabled, outputs, source).manifest.preparationRpcVersion, 1)
  assert.throws(() => frankfurtLocalProfile({ ...enabled, AA_FRANKFURT_MODE: 'api-readonly' }, outputs, source))
})

test('rejects incorrect account, region, network, target and insecure/live endpoints', () => {
  for (const [key, value] of Object.entries({ id: 'sepolia', region: 'ap-southeast-1', account_id: '111111111111', chain_id: '1' })) {
    const bad = structuredClone(outputs)
    bad.deployment_target.value[key] = value
    assert.throws(() => frankfurtLocalProfile(env, bad, source))
  }
  for (const value of ['http://aa-temp-api.sepolia.plether.com', 'https://app.sepolia.plether.com',
    'https://aa-temp-api.sepolia.plether.com:8443', 'https://user:secret@aa-temp-api.sepolia.plether.com',
    'https://aa-temp-api.sepolia.plether.com/?secret=1', 'https://aa-temp-api.plether.com.evil.invalid',
    'https://aa-temp-api.sepolia.plether.com/api']) {
    const bad = structuredClone(outputs)
    bad.frankfurt_tunnel.value.local_url = value
    assert.throws(() => frankfurtLocalProfile(env, bad, source))
  }
})

test('rejects missing secrets/paymaster, browser credential exposure, bypasses and old contracts', () => {
  for (const overrides of [
    { AA_PROXY_ORIGIN_TOKEN: '' }, { AA_PROXY_ORIGIN_TOKEN: '0'.repeat(64) },
    { VITE_AA_PROXY_ORIGIN_TOKEN: env.AA_PROXY_ORIGIN_TOKEN },
    { AA_FRANKFURT_PAYMASTER_ADDRESS: `0x${'0'.repeat(40)}` },
    { VITE_API_URL: 'https://app.sepolia.plether.com/api' },
    { VITE_API_PROXY_TARGET: 'https://app.sepolia.plether.com' },
    { VITE_API_PROXY_PRESERVE_PATH: '1' }, { AA_FRANKFURT_SPONSORSHIP_ENABLED: 'yes' },
  ]) assert.throws(() => frankfurtLocalProfile({ ...env, ...overrides }, outputs, source))
  assert.throws(() => frankfurtLocalProfile(env, outputs, { ...source, version: 'old' }))
})

test('preparation CI cannot deploy and planner cannot reuse local Singapore state', () => {
  const workflow = read('.github/workflows/aa-frankfurt-preparation.yml')
  assert(!workflow.includes('id-token: write'))
  assert(!workflow.includes('workflow_dispatch:'))
  assert(!workflow.includes('terraform apply'))
  const planner = read('scripts/plan-frankfurt-aa.mjs')
  assert.match(planner, /mkdtempSync/)
  assert.match(planner, /-backend-config=frankfurt\.backend\.hcl/)
  assert(!planner.includes('-migrate-state'))
  const backend = read('infra/terraform/frankfurt.backend.hcl')
  assert.match(backend, /plether\/sepolia-aa-temp\/terraform.tfstate/)
  assert(!backend.includes('plether/sepolia/terraform.tfstate'))
})

test('AA task definitions retain explicit empty ECS defaults without adding capabilities', () => {
  for (const name of ['aa_admin', 'aa_reconciler', 'alto']) {
    const source = read(`infra/terraform/${name}.tf`)
    const capabilities = [...source.matchAll(/capabilities\s*=\s*\{([^}]+)\}/g)]
    assert(capabilities.length >= 2)
    for (const [, block] of capabilities) {
      assert.match(block, /add\s*=\s*\[\]/, `${name}: prevent ECS empty-default replacement drift`)
      assert.match(block, /drop\s*=\s*\["ALL"\]/, `${name}: retain capability dropping`)
    }
  }
  const init = read('infra/terraform/alto.tf').split('name                   = "alto-tmp-init"')[1]
  assert.match(init, /environment\s*=\s*\[\]/)
})

test('zero-postOp Alto estimation overrides are isolated to the Frankfurt paymaster', () => {
  const source = read('infra/terraform/alto.tf')
  assert.match(source, /local\.frankfurt_preparation \? \[\s*\/\/|local\.frankfurt_preparation \? \[\s*#/)
  const overrides = source.split('alto_zero_post_op_environment = local.frankfurt_preparation ? [')[1]?.split('] : []')[0]
  assert(overrides)
  assert.match(source, /\], local\.alto_zero_post_op_environment\)/)
  for (const name of ['ALTO_SIMULATION_PAYMASTER_POST_OP_GAS_LIMIT', 'ALTO_V7_PAYMASTER_POST_OP_GAS_LIMIT_MULTIPLIER']) {
    assert.match(overrides, new RegExp(`name = "${name}", value = "0"`))
    assert.equal(source.split(name).length, 2)
  }
})
