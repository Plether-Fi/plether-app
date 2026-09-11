// AWS values stay in process memory/stdin, never argv, files or tool output.
import { execFileSync } from 'node:child_process'
import { readFileSync, realpathSync } from 'node:fs'
export const region = 'eu-central-1'
export const account = '932542905614'
export const namespace = '/plether/sepolia-aa-temp/'
// Outside the ECS runtime wildcard namespace; operator-only bootstrap inputs.
export const inputsParameter = '/plether/bootstrap/sepolia-aa-temp/terraform-inputs'

export function parseExecutorKeys(value) {
  if (typeof value !== 'string' || !/^0x[0-9a-f]{64}(,0x[0-9a-f]{64}){3}$/.test(value)) {
    throw new Error('Expected four comma-separated lowercase Alto executor keys')
  }
  const keys = value.split(',')
  if (new Set(keys).size !== 4 || keys.some(key => /^0x0{64}$/.test(key))) throw new Error('Invalid or reused Alto executor key')
  return keys
}

export function awsJson(service, operation, args = [], input, awsRegion = region) {
  try {
    const commandArgs = ['--profile', 'plether', '--region', awsRegion, service, operation, ...args, '--output', 'json']
    // macOS cannot reopen a Node child pipe as /dev/stdin. Use the installed
    // AWS CLI's own Python driver for input-bearing calls, keeping payloads off
    // OS argv and disk. Its configured credential_process/profile still applies.
    const awsPath = input === undefined ? null : realpathSync(execFileSync('which', ['aws'], { encoding: 'utf8' }).trim())
    const python = awsPath && /^#!(\/[^\n]+python[^\n]*)\n/.exec(readFileSync(awsPath, 'utf8'))?.[1]
    if (input !== undefined && !python) throw new Error('Expected a Python-backed AWS CLI for secure stdin input')
    const driver = 'import sys\nfrom awscli.clidriver import create_clidriver\nsys.exit(create_clidriver().main(sys.argv[1:] + ["--cli-input-json", sys.stdin.read()]))'
    return JSON.parse(execFileSync(input === undefined ? 'aws' : python,
      input === undefined ? commandArgs : ['-c', driver, ...commandArgs], {
      encoding: 'utf8', input: input === undefined ? undefined : JSON.stringify(input),
      stdio: ['pipe', 'pipe', 'pipe'], maxBuffer: 10 * 1024 * 1024,
    }) || '{}')
  } catch (error) {
    const code = /An error occurred \(([A-Za-z0-9]+)\)/.exec(error.stderr?.toString() ?? '')?.[1]
    const failure = new Error(`AWS ${service}/${operation} failed: ${code ?? 'CLI or connection error; response withheld'}`)
    failure.code = code
    throw failure
  }
}

export function verifyAccount() {
  const identity = awsJson('sts', 'get-caller-identity')
  if (identity.Account !== account) throw new Error('Unexpected AWS account')
  return identity
}

export function readSecret(name, awsRegion = region) {
  try {
    const value = awsJson('ssm', 'get-parameter', ['--name', name, '--with-decryption'], undefined, awsRegion).Parameter
    if (value.Type !== 'SecureString' || !value.Value) throw new Error('Expected a nonempty SecureString')
    return value.Value
  } catch (error) {
    if (error.code === 'ParameterNotFound') return null
    throw error
  }
}

export function createSecret(name, value) {
  if (!name.startsWith(namespace) && name !== inputsParameter && name !== '/plether/bootstrap/sepolia-aa-temp/trading-inputs') throw new Error('Refusing secret outside temporary namespace')
  // --overwrite is deliberately absent. Existing secrets are never rotated.
  awsJson('ssm', 'put-parameter', [], {
    Name: name, Type: 'SecureString', KeyId: 'alias/aws/ssm', Value: value,
    Tags: [{ Key: 'Project', Value: 'plether' }, { Key: 'Deployment', Value: 'sepolia-aa-temp' }],
  })
  if (readSecret(name) !== value) throw new Error('SecureString readback mismatch')
}

export function loadPreparedInputs() {
  const raw = readSecret(inputsParameter)
  if (!raw) throw new Error('Frankfurt bootstrap inputs have not been prepared')
  let parsed
  try { parsed = JSON.parse(raw) } catch { throw new Error('Invalid bootstrap input envelope') }
  const names = ['rpc_url', 'perps_rpc_url', 'keeper_private_key', 'oracle_updater_private_key',
    'liquidation_keeper_private_key', 'db_password', 'posthog_project_token', 'aa_proxy_origin_token']
  if (parsed.deployment !== 'sepolia-aa-temp' || parsed.schemaVersion !== 1
    || names.some(key => typeof parsed.inputs?.[key] !== 'string' || !parsed.inputs[key])
    || Object.keys(parsed.inputs).some(key => !names.includes(key))) throw new Error('Unexpected bootstrap inputs')
  return parsed.inputs
}
