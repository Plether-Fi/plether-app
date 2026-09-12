import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'

const root = new URL('../', import.meta.url)
const workflows = ['deploy-backend', 'deploy-alto', 'aa-admin']

for (const workflow of workflows) {
  const source = readFileSync(new URL(`.github/workflows/${workflow}.yml`, root), 'utf8')
  // Execute the actual inline jq filters, both before and after human approval.
  const filters = [...source.matchAll(/'(\.name == \$name\n[\s\S]*?)'\s+\\/g)].map(match => match[1])
  test(`${workflow}: checks the environment before and after approval`, () => {
    assert.equal(filters.length, 2)
    assert.match(source, /GITHUB_REF.*refs\/heads\/master/)
  })
  for (const [index, filter] of filters.entries()) {
    const valid = () => ({
      name: 'test-admin-sepolia',
      can_admins_bypass: false,
      protection_rules: [{
        type: 'required_reviewers',
        prevent_self_review: false,
        reviewers: [{ type: 'User', reviewer: { id: 41941, login: 'Stanley' } }],
      }],
      deployment_branch_policy: { custom_branch_policies: true },
    })
    const cases = [
      ['sole maintainer explicitly approves', () => {}, true],
      ['administrator bypass', value => { value.can_admins_bypass = true }, false],
      ['no reviewers', value => { value.protection_rules[0].reviewers = [] }, false],
      ['no review rule', value => { value.protection_rules = [] }, false],
      ['duplicate review rules', value => { value.protection_rules.push(value.protection_rules[0]) }, false],
      ['wrong environment', value => { value.name = 'test-admin-mainnet' }, false],
      ['unrestricted branches', value => { value.deployment_branch_policy.custom_branch_policies = false }, false],
      ['missing self-review policy', value => { delete value.protection_rules[0].prevent_self_review }, false],
      ['incompatible independent-only policy', value => { value.protection_rules[0].prevent_self_review = true }, false],
    ]
    for (const [label, mutate, expected] of cases) {
      test(`${workflow} gate ${index + 1}: ${label}`, () => {
        const value = valid()
        mutate(value)
        const result = spawnSync('jq', ['--exit-status', '--arg', 'name', 'test-admin-sepolia', filter], {
          input: JSON.stringify(value), encoding: 'utf8',
        })
        assert.ifError(result.error)
        assert.ok([0, 1].includes(result.status), result.stderr)
        assert.equal(result.status === 0, expected, result.stderr)
      })
    }
  }
}
