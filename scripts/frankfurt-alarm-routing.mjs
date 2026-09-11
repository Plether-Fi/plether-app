import assert from 'node:assert/strict'
import { resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { awsJson, verifyAccount } from './frankfurt-aa-secrets.mjs'

const prefix = 'plether-sepolia-aa-temp-'
const topic = 'arn:aws:sns:eu-central-1:932542905614:plether-sepolia-aa-temp-operations'
const fields = ['AlarmName', 'AlarmDescription', 'ActionsEnabled', 'OKActions', 'AlarmActions',
  'InsufficientDataActions', 'MetricName', 'Namespace', 'Statistic', 'ExtendedStatistic',
  'Dimensions', 'Period', 'Unit', 'EvaluationPeriods', 'DatapointsToAlarm', 'Threshold',
  'ComparisonOperator', 'TreatMissingData', 'EvaluateLowSampleCountPercentile', 'Metrics',
  'ThresholdMetricId']

export function alarmSpec(alarm) {
  const spec = Object.fromEntries(fields.filter(key => alarm[key] !== undefined).map(key => [key, alarm[key]]))
  if (spec.Metrics) {
    // DescribeAlarms materializes an empty top-level Dimensions list for
    // metric-math alarms; PutMetricAlarm rejects it alongside Metrics.
    assert.equal((spec.Dimensions ?? []).length, 0)
    delete spec.Dimensions
  }
  return spec
}

export function routeAlarm(alarm) {
  assert(alarm.AlarmName.startsWith(prefix), 'Not a Frankfurt canary alarm')
  assert.equal(alarm.AlarmArn, `arn:aws:cloudwatch:eu-central-1:932542905614:alarm:${alarm.AlarmName}`)
  assert.equal(alarm.ActionsEnabled, true, 'Do not silently enable disabled actions')
  for (const key of ['AlarmActions', 'OKActions']) {
    assert((alarm[key] ?? []).every(action => action === topic), 'Unexpected notification destination')
  }
  assert.equal((alarm.InsufficientDataActions ?? []).length, 0, 'Preserve unexpected actions for manual review')
  const before = alarmSpec(alarm)
  const warning = alarm.AlarmName === prefix + 'rpc-request-rate-warning'
  // Only the duplicate RPC volume warning becomes dashboard-only. All other
  // failure/capacity/safety alerts keep their existing email destination.
  assert(warning || alarm.AlarmActions?.includes(topic), 'Do not subscribe previously unrouted alarms')
  const after = { ...before, AlarmActions: warning ? [] : before.AlarmActions,
    OKActions: warning ? [] : [topic] }
  return { before, after }
}

async function main() {
  assert(process.argv.length <= 3 && [undefined, '--apply'].includes(process.argv[2]))
  verifyAccount()
  const alarms = awsJson('cloudwatch', 'describe-alarms', ['--alarm-name-prefix', prefix])
  assert.equal(alarms.CompositeAlarms?.length ?? 0, 0, 'Review existing composite routing first')
  assert.equal(alarms.MetricAlarms.length, 59, 'Alarm inventory changed; review before applying')
  const changes = alarms.MetricAlarms.map(routeAlarm)
    .filter(({ before, after }) => JSON.stringify(before) !== JSON.stringify(after))
  console.log(JSON.stringify({ mode: process.argv[2] === '--apply' ? 'apply' : 'plan',
    changes: changes.map(({ after }) => ({ name: after.AlarmName,
      alarmEmail: after.AlarmActions.length > 0, recoveryEmail: after.OKActions.length > 0 })) }))
  if (process.argv[2] !== '--apply') return
  for (const { before, after } of changes) {
    const current = awsJson('cloudwatch', 'describe-alarms', ['--alarm-names', before.AlarmName]).MetricAlarms[0]
    assert.deepEqual(alarmSpec(current), before, 'Alarm changed after planning')
    awsJson('cloudwatch', 'put-metric-alarm', [], after)
    const checked = awsJson('cloudwatch', 'describe-alarms', ['--alarm-names', after.AlarmName]).MetricAlarms[0]
    assert.deepEqual(alarmSpec(checked), after, 'Alarm readback differs from reviewed routing')
    console.log(JSON.stringify({ verified: after.AlarmName }))
  }
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) await main()
