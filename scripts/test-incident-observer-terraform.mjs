// Execute the actual observer resources in an isolated mocked module. No state,
// credentials, application variables or network resources are copied.
import assert from 'node:assert/strict'
import {copyFileSync, mkdirSync, mkdtempSync, rmSync, writeFileSync} from 'node:fs'
import {spawnSync} from 'node:child_process'
import {tmpdir} from 'node:os'
import {join} from 'node:path'
import {fileURLToPath} from 'node:url'

const root = fileURLToPath(new URL('..', import.meta.url))
const directory = mkdtempSync(join(tmpdir(), 'plether-incident-observer-'))
const terraform = process.env.TERRAFORM_BIN ?? 'terraform'
const run = (command, args, cwd = directory) => {
  const result = spawnSync(command, args, {cwd, stdio: 'inherit', env: {...process.env, PYTHONDONTWRITEBYTECODE: '1', TF_IN_AUTOMATION: '1'}})
  if (result.error) throw result.error
  assert.equal(result.signal, null)
  assert.equal(result.status, 0, `${command} ${args.join(' ')} failed`)
}
try {
  mkdirSync(join(directory, 'tests'))
  for (const file of ['incident_events.tf', 'incident_monitoring.tf', '.terraform.lock.hcl']) copyFileSync(join(root, 'infra/terraform', file), join(directory, file))
  const zip = join(directory, 'observer.zip')
  run('python3', [join(root, 'scripts/package-incident-observer.py'), '--output', zip])
  let context = `terraform {
    required_version = "~> 1.16.0"
    required_providers { aws = { source = "hashicorp/aws", version = "~> 5.0" } }
  }
  provider "aws" { region = "ap-southeast-1" }
  variable "aws_region" { default = "ap-southeast-1" }
  variable "operations_alarm_sns_topic_arn" { default = "arn:aws:sns:ap-southeast-1:932542905614:operations" }
  locals { deployment_name = "sepolia" }
  data "aws_caller_identity" "current" {}
  resource "aws_ecs_cluster" "main" { name = "plether-sepolia" }
  `
  for (const [resource, name, counted] of [
    ['api', 'plether-api'], ['keeper', 'plether-keeper'], ['liquidation_worker', 'plether-liquidation-worker'],
    ['basket_worker', 'plether-basket-worker'], ['perps_indexer', 'plether-perps-indexer'], ['insights_worker', 'plether-insights-worker'],
    ['workers', 'plether-workers', true], ['aa_reconciler', 'plether-aa-reconciler', true], ['protection_worker', 'plether-position-protection-worker', true],
    ['alto', 'plether-alto', true], ['rpc_gateway', 'plether-rpc-gateway', true],
  ]) context += `resource "aws_ecs_service" "${resource}" {
    ${counted ? 'count = 1' : ''}
    name = "${name}"
    cluster = aws_ecs_cluster.main.id
    task_definition = "arn:aws:ecs:ap-southeast-1:932542905614:task-definition/fixture:1"
    desired_count = 1
  }\n`
  writeFileSync(join(directory, 'context.tf'), context)
  writeFileSync(join(directory, 'tests/observer.tftest.hcl'), `
mock_provider "aws" {
  mock_data "aws_caller_identity" { defaults = { account_id = "932542905614" } }
  mock_resource "aws_ecs_cluster" {
    override_during = plan
    defaults = { arn = "arn:aws:ecs:ap-southeast-1:932542905614:cluster/plether-sepolia" }
  }
  mock_resource "aws_iam_role" {
    override_during = plan
    defaults = { arn = "arn:aws:iam::932542905614:role/fixture-observer" }
  }
  mock_resource "aws_lambda_function" {
    override_during = plan
    defaults = { arn = "arn:aws:lambda:ap-southeast-1:932542905614:function:fixture-observer" }
  }
  mock_resource "aws_cloudwatch_log_group" {
    override_during = plan
    defaults = { arn = "arn:aws:logs:ap-southeast-1:932542905614:log-group:fixture-evidence" }
  }
  mock_resource "aws_cloudwatch_event_rule" {
    override_during = plan
    defaults = { arn = "arn:aws:events:ap-southeast-1:932542905614:rule/fixture-observer" }
  }
}
variables { incident_observer_package_path = ${JSON.stringify(zip)} }
run "disabled_by_default" {
  command = plan
  assert {
    condition = length(aws_lambda_function.incident_observer) == 0 && length(aws_cloudwatch_event_target.incident_observer) == 0 && length(aws_cloudwatch_metric_alarm.incident_service_task_deficit) == 0
    error_message = "Observer rollout must be explicit, with no new resources by default."
  }
}
run "enabled_observer_is_scoped_and_fail_closed" {
  command = plan
  variables { enable_incident_observer = true }
  assert {
    condition = aws_cloudwatch_log_group.ecs_incident_evidence[0].retention_in_days == 90 && aws_cloudwatch_log_group.incident_observer_runtime[0].retention_in_days == 90
    error_message = "Sanitized evidence must survive the short ECS stopped-task window."
  }
  assert {
    condition = length(aws_cloudwatch_metric_alarm.incident_service_task_deficit) == 11 && alltrue([for a in aws_cloudwatch_metric_alarm.incident_service_task_deficit : a.metric_name == "ServiceTaskDeficit" && a.datapoints_to_alarm == 2 && a.treat_missing_data == "missing"])
    error_message = "Every configured service, including dormant topology, requires independent desired/running checks."
  }
  assert {
    condition = jsondecode(aws_cloudwatch_event_rule.ecs_task_stopped[0].event_pattern).detail.lastStatus == ["STOPPED"] && length(jsondecode(aws_cloudwatch_event_rule.ecs_task_stopped[0].event_pattern).detail.group) == 11
    error_message = "Stop events must be scoped to the configured cluster services."
  }
  assert {
    condition = toset(flatten([for s in jsondecode(aws_iam_role_policy.incident_observer[0].policy).Statement : s.Action])) == toset(["ecs:DescribeServices", "logs:CreateLogStream", "logs:PutLogEvents", "cloudwatch:PutMetricData", "sns:Publish"])
    error_message = "Observer must have no secret, database or transaction mutation privileges."
  }
  assert {
    condition = aws_cloudwatch_metric_alarm.incident_observer_missing[0].treat_missing_data == "breaching" && aws_cloudwatch_metric_alarm.incident_observer_events_dropped[0].metric_name == "AsyncEventsDropped" && length(aws_cloudwatch_metric_alarm.incident_event_delivery_failed) == 2
    error_message = "Observer silence and both delivery failure paths must be monitored."
  }
}
run "notifications_cannot_be_silently_disabled" {
  command = plan
  variables {
    enable_incident_observer = true
    operations_alarm_sns_topic_arn = ""
  }
  expect_failures = [terraform_data.incident_observer_guard]
}
run "foreign_account_topic_is_refused" {
  command = plan
  variables {
    enable_incident_observer = true
    incident_escalation_sns_topic_arn = "arn:aws:sns:ap-southeast-1:000000000000:foreign"
  }
  expect_failures = [terraform_data.incident_observer_guard]
}
`)
  run(terraform, ['init', '-backend=false', '-input=false', '-no-color', ...(process.env.TERRAFORM_PLUGIN_DIR ? [`-plugin-dir=${process.env.TERRAFORM_PLUGIN_DIR}`] : [])])
  run(terraform, ['test', '-filter=tests/observer.tftest.hcl', '-no-color'])
} finally {
  rmSync(directory, {recursive: true, force: true})
}
