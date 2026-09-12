import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync, existsSync } from 'node:fs'
import { execFileSync, spawnSync } from 'node:child_process'
const root = new URL('../', import.meta.url)
const read = path => readFileSync(new URL(path, root), 'utf8')
const bindings = {
  "perps_usdc": "mockUsdc",
  "perps_order_router": "orderRouter",
  "perps_order_lifecycle_book": "orderLifecycleBook",
  "perps_cfd_engine": "cfdEngine",
  "perps_margin_clearinghouse": "marginClearinghouse",
  "perps_house_pool": "housePool",
  "perps_plether_oracle": "pletherOracle",
  "perps_senior_vault": "seniorVault",
  "perps_junior_vault": "juniorVault",
  "perps_settlement_monitor_lens": "settlementMonitorLens",
  "perps_cfd_engine_settlement_sidecar": "cfdEngineSettlementSidecar",
  "perps_cfd_engine_lens": "cfdEngineLens",
  "perps_account_lens": "cfdEngineAccountLens",
  "vault_history_house_pool_address": "housePool",
  "vault_history_senior_vault_address": "seniorVault",
  "vault_history_junior_vault_address": "juniorVault"
}
test('Singapore release overlay exactly matches the canonical Core manifest', () => {
  const release = JSON.parse(read('config/perps/arbitrum-sepolia-v2.json'))
  const overlay = JSON.parse(read('infra/terraform/sepolia-core-v1.2.3.tfvars.json'))
  assert.equal(release.release.version, 'v1.2.3')
  assert.equal(overlay.perps_indexer_start_block, String(release.release.deploymentBlock))
  assert.equal(overlay.vault_history_deployment_block, String(release.release.deploymentBlock))
  assert.equal(overlay.perps_chain_id, String(release.network.chainId))
  for (const [name, contract] of Object.entries(bindings)) assert.equal(overlay[name], release.contracts[contract].address)
  const guards = read('infra/terraform/deployment_target.tf')
  assert.ok(guards.includes('var.vault_history_deployment_block == tostring(local.sepolia_core.release.deploymentBlock)'))
  for (const [name, contract] of Object.entries(bindings)) assert.ok(guards.includes('lower(var.' + name + ') == lower(local.sepolia_core.contracts.' + contract + '.address)'))
})
test('public overlay stages policy without enabling sponsorship or preparation', () => {
  const overlay = read('infra/terraform/sepolia-aa-public.tfvars')
  for (const flag of ['enable_native_aa_sponsorship','enable_native_aa_preparation','enable_native_aa_submission','enable_aa_readiness_enforcement'])
    assert.match(overlay, new RegExp(flag + '\\s*= false'))
  assert.match(overlay, /aa_native_global_rollout_enabled = true/)
})
test('active Alto gate rejects unsafe mode, skipped validation, debug and duplicate controls', () => {
  assert.match(read('infra/terraform/alto.tf'), /name = "ALTO_SAFE_MODE", value = tostring\(local.alto_safe_mode\)/)
  assert.match(read('infra/terraform/alto.tf'), /alto_safe_mode\s*= true/)
  const fixture = () => ({containerDefinitions:[{name:'plether-alto',environment:[
    {name:'ALTO_SAFE_MODE',value:'true'},
    {name:'ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION',value:'false'},
    {name:'ALTO_ENABLE_DEBUG_ENDPOINTS',value:'false'}]}]})
  const validate = data => spawnSync('jq',['-e','-f',new URL('.github/scripts/validate-aa-public-alto.jq',root).pathname],{input:JSON.stringify(data)}).status
  assert.equal(validate(fixture()),0)
  for (let i=0;i<3;i++) {
    const data=fixture(); data.containerDefinitions[0].environment[i].value=i===0?'false':'true'
    assert.notEqual(validate(data),0)
  }
  const duplicate=fixture(); duplicate.containerDefinitions[0].environment.push(duplicate.containerDefinitions[0].environment[0])
  assert.notEqual(validate(duplicate),0)
  assert.notEqual(validate({containerDefinitions:[]}),0)
})
test('backend deployment promotes funding observer and keeps manual approval boundary', () => {
  const workflow=read('.github/workflows/deploy-backend.yml')
  assert.match(workflow,/validate-aa-public-alto\.jq/)
  assert.match(workflow,/else\s+\.image = \$applicationImage/)
  assert.ok(!workflow.split('jobs:')[0].includes('  push:'))
  assert.match(read('infra/terraform/aa_funding.tf'),/command\s*= \["node", "\/app\/protection\/funding-main.mjs"\]/)
})
test('maintained text and filenames contain no retired temporary-stack references', () => {
  const patterns = new RegExp(['frank'+'furt','sepolia-aa'+'-temp','eu-'+'central-1'].join('|'),'i')
  const paths=execFileSync('git',['ls-files','--cached','--others','--exclude-standard'],{cwd:root,encoding:'utf8'}).trim().split('\n')
  for (const path of paths) {
    if (!existsSync(new URL(path,root)) || path === 'docs/runbooks/v1.2.3-tranche-liquidity-2026-09-11.md') continue
    assert.ok(!patterns.test(path),path)
    if (/\.(md|tf|hcl|json|yml|yaml|mjs|ts|tsx|hs|lua|conf)$/.test(path) && !path.endsWith('package-lock.json'))
      assert.ok(!patterns.test(read(path)),path)
  }
})
