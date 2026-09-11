import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { routeAlarm } from './frankfurt-alarm-routing.mjs'

const topic = 'arn:aws:sns:eu-central-1:932542905614:plether-sepolia-aa-temp-operations'
function fixture(suffix = 'rpc-request-rate-critical') {
  const name = 'plether-sepolia-aa-temp-' + suffix
  return { AlarmName: name, AlarmArn: 'arn:aws:cloudwatch:eu-central-1:932542905614:alarm:' + name,
    ActionsEnabled: true, AlarmActions: [topic], OKActions: [], InsufficientDataActions: [],
    Threshold: 25000, Period: 3600, EvaluationPeriods: 1, TreatMissingData: 'notBreaching',
    StateValue: 'ALARM', StateReason: 'fixture', Dimensions: [] }
}
test('only duplicate warning is dashboard-only; critical keeps email and gains recovery', () => {
  const warning = routeAlarm(fixture('rpc-request-rate-warning'))
  assert.deepEqual(warning.after.AlarmActions, [])
  assert.deepEqual(warning.after.OKActions, [])
  const { before, after } = routeAlarm(fixture())
  assert.deepEqual(after, { ...before, OKActions: [topic] })
  assert(!('StateValue' in after))
})
test('safety and memory alarms retain their notifications and all evaluation fields', () => {
  for (const suffix of ['aa-reconciler-fatal', 'aa-native-api-fault', 'rds-freeable-memory-low', 'protection-failure']) {
    const { before, after } = routeAlarm(fixture(suffix))
    assert.deepEqual(after, { ...before, OKActions: [topic] })
  }
})
test('routing is idempotent', () => {
  const a = fixture()
  a.OKActions = [topic]
  assert.deepEqual(routeAlarm(a).after, routeAlarm(a).before)
})
test('metric-math round trips omit the read-only empty top-level dimensions', () => {
  const alarm = { ...fixture('rpc-failure-rate'), Metrics: [
    { Id: 'failure_rate', Expression: 'IF(requests >= 100, 100 * failures / requests, 0)', ReturnData: true },
  ] }
  const { before, after } = routeAlarm(alarm)
  assert(!('Dimensions' in after))
  assert.deepEqual(after.Metrics, alarm.Metrics)
  assert.deepEqual(after, { ...before, OKActions: [topic] })
  assert.throws(() => routeAlarm({ ...alarm, Dimensions: [{ Name: 'unexpected', Value: 'x' }] }))
})
test('rejects other environments, destinations and disabled actions', () => {
  for (const patch of [{ AlarmName: 'plether-sepolia-rpc-request-rate-critical' },
    { AlarmArn: 'arn:aws:cloudwatch:ap-southeast-1:932542905614:alarm:wrong' },
    { AlarmActions: ['unexpected'] }, { ActionsEnabled: false }, { AlarmActions: [] },
    { InsufficientDataActions: [topic] }]) assert.throws(() => routeAlarm({ ...fixture(), ...patch }))
})
test('Terraform scopes recovery to Frankfurt and routes every metric alarm explicitly', () => {
  const local = readFileSync(new URL('../infra/terraform/alarm_routing.tf', import.meta.url), 'utf8')
  assert.match(local, /local.frankfurt_preparation \? compact/)
  for (const file of ['monitoring.tf', 'protection.tf']) {
    const source = readFileSync(new URL('../infra/terraform/' + file, import.meta.url), 'utf8')
    for (const block of source.split(/(?=resource ")/).filter(x => x.startsWith('resource "aws_cloudwatch_metric_alarm"'))) {
      assert.match(block, /ok_actions\s*=/)
    }
  }
})
