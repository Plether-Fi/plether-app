import test from 'node:test'
import assert from 'node:assert/strict'
import { closeSync, copyFileSync, createReadStream, mkdtempSync, mkdirSync, openSync, rmSync, writeFileSync } from 'node:fs'
import { createInterface } from 'node:readline'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

// Exercise the actual production ECR resources in an isolated mocked module.
// No state, credentials, tfvars or other infrastructure resources are copied.
test('runtime repositories retain referenced artifacts and refuse destructive defaults', { timeout: 180_000 }, async () => {
  const root = fileURLToPath(new URL('../', import.meta.url))
  const directory = mkdtempSync(join(tmpdir(), 'plether-runtime-repositories-'))
  const terraform = process.env.TERRAFORM_BIN ?? 'terraform'
  const run = args => {
    const output = join(directory, 'terraform-output.ndjson')
    const fd = openSync(output, 'w')
    let result
    try {
      result = spawnSync(terraform, args, { cwd: directory, encoding: 'utf8', stdio: ['ignore', fd, 'pipe'],
        env: { ...process.env, TF_IN_AUTOMATION: '1', AWS_PROFILE: 'plether' } })
    } finally { closeSync(fd) }
    assert.equal(result.status, 0, result.error?.message ?? `${result.stderr}\nSee ${output}`)
    return output
  }
  try {
    mkdirSync(join(directory, 'tests'))
    copyFileSync(join(root, 'infra/terraform/ecr.tf'), join(directory, 'ecr.tf'))
    copyFileSync(join(root, 'infra/terraform/.terraform.lock.hcl'), join(directory, '.terraform.lock.hcl'))
    writeFileSync(join(directory, 'main.tf'), `terraform {
  required_version = "~> 1.16.0"
  required_providers { aws = { source = "hashicorp/aws", version = "~> 5.0" } }
}
variable "environment" { default = "sepolia" }
locals {
  deployment_name = var.environment
  self_hosted_aa_resource_count = var.environment == "sepolia" ? 1 : 0
}
`)
    writeFileSync(join(directory, 'tests/retention.tftest.hcl'), `mock_provider "aws" {}
run "sepolia_runtime_repositories" {
  command = plan
  assert {
    condition = alltrue([
      aws_ecr_repository.api.image_tag_mutability == "IMMUTABLE",
      aws_ecr_repository.otel_log_router.image_tag_mutability == "IMMUTABLE",
      aws_ecr_repository.alto[0].image_tag_mutability == "IMMUTABLE",
      !aws_ecr_repository.api.force_delete,
      !aws_ecr_repository.otel_log_router.force_delete,
      !aws_ecr_repository.alto[0].force_delete,
    ])
    error_message = "Runtime image repositories must not overwrite releases or force-delete their artifacts."
  }
}
run "mainnet_runtime_repositories" {
  command = plan
  variables { environment = "mainnet" }
  assert {
    condition = alltrue([
      aws_ecr_repository.api.image_tag_mutability == "IMMUTABLE",
      aws_ecr_repository.otel_log_router.image_tag_mutability == "IMMUTABLE",
      !aws_ecr_repository.api.force_delete,
      !aws_ecr_repository.otel_log_router.force_delete,
    ])
    error_message = "Mainnet must retain the same artifact protection."
  }
}
`)
    run(['init', '-backend=false', '-input=false', '-lockfile=readonly', '-no-color'])
    const output = run(['test', '-json', '-verbose', '-no-color'])
    let plans = 0
    for await (const line of createInterface({ input: createReadStream(output), crlfDelay: Infinity })) {
      const event = JSON.parse(line)
      if (!event.test_plan) continue
      plans++
      const plan = event.test_plan
      const resources = plan.resource_changes ?? []
      assert.ok(resources.some(resource => resource.type === 'aws_ecr_repository'), 'inspect a real repository plan')
      assert.equal(resources.filter(resource => resource.type === 'aws_ecr_lifecycle_policy').length, 0,
        'automatic expiry must not reappear in any runtime repository')
    }
    assert.equal(plans, 2, 'both actual mocked plans must be inspected')
  } finally {
    rmSync(directory, { recursive: true, force: true })
  }
})
