#!/usr/bin/env node
// Plan only. Deliberately has no apply, import, migration, funding or deployment path.
import { execFileSync } from 'node:child_process'
import { copyFileSync, mkdirSync, mkdtempSync, readdirSync, chmodSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { loadPreparedInputs } from './frankfurt-aa-secrets.mjs'
import { loadTradingInputs } from './frankfurt-trading-inputs.mjs'

const apiOnly = process.argv.length === 3 && process.argv[2] === '--api-readonly'
const qualification = process.argv.length === 3 && process.argv[2] === '--aa-qualification'
const prepared = process.argv.length === 3 && process.argv[2] === '--aa-prepared'
const canary = process.argv.length === 3 && process.argv[2] === '--aa-canary'
const tradingPrepared = process.argv.length === 3 && process.argv[2] === '--trading-prepared'
const trading = process.argv.length === 3 && (process.argv[2] === '--trading-canary' || tradingPrepared)
if (process.argv.length !== 2 && !apiOnly && !qualification && !prepared && !canary && !trading) throw new Error('Unsupported Frankfurt plan stage')
if (Object.keys(process.env).some(key => key.startsWith('TF_CLI_ARGS'))
  || (process.env.TF_WORKSPACE && process.env.TF_WORKSPACE !== 'default')) {
  throw new Error('Remove Terraform CLI argument/workspace overrides before planning')
}
const terraform = process.env.TERRAFORM_BIN || 'terraform'
const version = JSON.parse(execFileSync(terraform, ['version', '-json'], { encoding: 'utf8' }))
if (!version.terraform_version.startsWith('1.16.')) throw new Error('Terraform 1.16.x required')
const identity = JSON.parse(execFileSync('aws', ['--profile', 'plether', 'sts', 'get-caller-identity'], { encoding: 'utf8' }))
if (identity.Account !== '932542905614') throw new Error('Wrong AWS account')

process.umask(0o077)
const root = fileURLToPath(new URL('../', import.meta.url))
const staging = mkdtempSync(join(tmpdir(), 'plether-frankfurt-plan-'))
const directory = join(staging, 'infra/terraform')
mkdirSync(directory, { recursive: true })
mkdirSync(join(staging, 'config/perps'), { recursive: true })
// Do not copy .terraform, local auto.tfvars, old plans or any existing state.
for (const name of readdirSync(join(root, 'infra/terraform'))) {
  if (name.endsWith('.tf') || name === '.terraform.lock.hcl'
    || name === 'frankfurt-aa-preparation.tfvars' || name === 'frankfurt-api-readonly.tfvars'
    || name === 'frankfurt-aa-qualification.tfvars' || name === 'frankfurt-aa-canary.tfvars' || name === 'frankfurt-trading-canary.tfvars' || name === 'frankfurt.backend.hcl') {
    copyFileSync(join(root, 'infra/terraform', name), join(directory, name))
  }
}
copyFileSync(join(root, 'config/perps/arbitrum-sepolia-v2.json'), join(staging, 'config/perps/arbitrum-sepolia-v2.json'))
const env = { ...process.env, AWS_PROFILE: 'plether', TF_DATA_DIR: join(staging, 'terraform-data'),
  TF_WORKSPACE: 'default', TF_IN_AUTOMATION: '1' }
if (process.env.FRANKFURT_USE_PREPARED_INPUTS === '1') {
  for (const [name, value] of Object.entries(loadPreparedInputs())) env[`TF_VAR_${name}`] = value
}
if (trading) {
  for (const [name, value] of Object.entries(loadTradingInputs())) env[`TF_VAR_${name}`] = value
}
// Explicit profile must win over unrelated environment credentials.
for (const key of ['AWS_ACCESS_KEY_ID', 'AWS_SECRET_ACCESS_KEY', 'AWS_SESSION_TOKEN',
  'AWS_WEB_IDENTITY_TOKEN_FILE', 'AWS_ROLE_ARN', 'AWS_ENDPOINT_URL',
  // The login credential_process must retain its own session region. Resource
  // region is already fixed independently in the backend and Terraform vars.
  'AWS_REGION', 'AWS_DEFAULT_REGION']) delete env[key]
const run = args => execFileSync(terraform, args, { cwd: directory, env, stdio: 'inherit' })
console.log(`Private plan workspace: ${staging}`)
run(['init', '-input=false', '-lockfile=readonly', '-backend-config=frankfurt.backend.hcl'])
// -input=false deliberately fails if real secrets have not been supplied via
// TF_VAR_*; no synthetic values or copying Singapore credentials automatically.
run(['plan', '-input=false', '-lock-timeout=60s', '-var-file=frankfurt-aa-preparation.tfvars',
  ...(apiOnly ? ['-var-file=frankfurt-api-readonly.tfvars'] : []),
  ...(qualification || prepared || canary || trading ? ['-var-file=frankfurt-aa-qualification.tfvars'] : []),
  ...(canary || trading ? ['-var-file=frankfurt-aa-canary.tfvars'] : []),
  ...(trading ? ['-var-file=frankfurt-trading-canary.tfvars'] : []),
  ...(tradingPrepared ? ['-var=frankfurt_activation_stage=trading-prepared', '-var=workers_desired_count=0', '-var=liquidation_worker_desired_count=0', '-var=protection_worker_desired_count=0', '-var=protection_worker_execution_enabled=false', '-var=aa_protection_commits_enabled=false'] : []),
  ...(trading && process.env.FRANKFURT_LP_PREFLIGHT_CONFIRMED === '1' ? ['-var=lp_settlement_signer_funding_confirmed=true', '-var=lp_settlement_mode=execute'] : []),
  ...(prepared ? ['-var=frankfurt_activation_stage=aa-prepared', '-var=alto_desired_count=0',
    '-var=aa_reconciler_desired_count=0', '-var=configure_native_aa_backend=false'] : []), '-out=frankfurt.tfplan'])
chmodSync(join(directory, 'frankfurt.tfplan'), 0o600)
console.log('Plan saved locally; contains secrets. Do not upload it to GitHub or commit it. Nothing applied.')
