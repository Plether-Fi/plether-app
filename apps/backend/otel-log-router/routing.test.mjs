import assert from 'node:assert/strict'
import { test } from 'node:test'
import { readFileSync, writeFileSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { spawnSync } from 'node:child_process'

for (const [container,event,reason] of [
  ['plether-keeper','keeper_transaction_failed','KEEPER_INSUFFICIENT_FUNDS'],
  ['plether-funding-monitor','worker_funding_observation','WORKER_INSUFFICIENT_FUNDS'],
]) test(`real Fluent Bit preserves CloudWatch and redacts PostHog for ${container}`, { skip: !process.env.AA_LOG_ROUTER_TEST_IMAGE }, () => {
  const directory = mkdtempSync(join(tmpdir(), 'plether-log-routing-'))
  try {
    const input = { container_name: container, log: JSON.stringify({
      event, message: 'private provider payload', level: 'ERROR',
      error: 'private exception', order_ids: [8], signer_balance_wei: '1234',
      attempt_id: '12345678-1234-4123-8123-123456789abc', reason_code: reason,
    }) }
    const filters = readFileSync(new URL('otel-enrichment.conf', import.meta.url), 'utf8').split('[OUTPUT]')[0]
      .replace('/fluent-bit/etc/posthog-projection.lua', '/fixtures/posthog-projection.lua')
    writeFileSync(join(directory, 'test.conf'), `[SERVICE]
    Flush 1
    Grace 1
    Parsers_File /fixtures/plether-parsers.conf
[INPUT]
    Name dummy
    Tag ${container}-firelens-fixture
    Samples 1
    Dummy ${JSON.stringify(input)}
${filters}
[FILTER]
    Name modify
    Match cloudwatch.*
    Add test_sink cloudwatch
[FILTER]
    Name modify
    Match plether-*
    Add test_sink posthog
[OUTPUT]
    Name stdout
    Match *
    Format json_lines
`)
    for (const name of ['posthog-projection.lua', 'plether-parsers.conf']) {
      writeFileSync(join(directory, name), readFileSync(new URL(name, import.meta.url)))
    }
    const result = spawnSync('docker', ['run', '--rm', '--network=none',
      '--mount', `type=bind,src=${directory},dst=/fixtures,readonly`,
      '--env', 'SERVICE_VERSION=fixture', '--env', 'DEPLOYMENT_ENVIRONMENT=sepolia',
      '--env', 'AWS_REGION=eu-central-1', '--env', 'ECS_CLUSTER_NAME=fixture',
      '--entrypoint', '/usr/bin/timeout', process.env.AA_LOG_ROUTER_TEST_IMAGE,
      '5s', '/fluent-bit/bin/fluent-bit', '-c', '/fixtures/test.conf'], { encoding: 'utf8', timeout: 20_000 })
    assert.equal(result.status, 124, result.stderr)
    const records = result.stdout.split('\n').filter(line => line.startsWith('{')).map(line => JSON.parse(line))
    const cloudwatch = records.find(record => record.test_sink === 'cloudwatch')
    const posthog = records.find(record => record.test_sink === 'posthog')
    assert.ok(cloudwatch, result.stdout + result.stderr)
    assert.ok(posthog, result.stdout + result.stderr)
    assert.equal(cloudwatch.error, 'private exception')
    assert.deepEqual(cloudwatch.order_ids, [8])
    assert.equal(cloudwatch.signer_balance_wei, '1234')
    assert.equal(cloudwatch.event, event)
    assert.equal(posthog.message, event)
    assert.equal(posthog.attempt_id, input.log && JSON.parse(input.log).attempt_id)
    assert.equal(posthog.reason_code, reason)
    for (const key of ['error', 'order_ids', 'signer_balance_wei', 'log', 'container_name']) assert.equal(posthog[key], undefined)
    assert.equal(records.length, 2, 'Cloning must not recurse or duplicate exports')
  } finally { rmSync(directory, { recursive: true, force: true }) }
})
