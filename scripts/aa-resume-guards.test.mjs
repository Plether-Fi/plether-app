import test from 'node:test'
import assert from 'node:assert/strict'
import {readFileSync} from 'node:fs'
import {spawnSync} from 'node:child_process'

const source = readFileSync(new URL('../.github/workflows/aa-admin.yml', import.meta.url), 'utf8')
const runJq = (filter, args, input) => spawnSync('jq', [input === undefined ? '-ne' : '-e', ...Object.entries(args).flatMap(([k,v]) => ['--arg', k, v]), filter], {input: input === undefined ? undefined : JSON.stringify(input), encoding: 'utf8'})
const validation = source.split('- name: Validate allowlisted operation')[1].split('- name: Check out reviewed audit lifecycle policy')[0]
const evidenceFilter = validation.match(/'(def safe:[\s\S]*?)' >\/dev\/null/)?.[1]
assert.ok(evidenceFilter, 'extract actual resume evidence validation')

test('resume approval binds a lossless canonical BIGINT generation and nonzero paymaster', () => {
  const base = {reason:'exact incident', note:'approved recovery evidence', generation:'9007199254740993', paymaster:'0x'+'a'.repeat(40)}
  const cases = [
    ['greater than JS integer precision', {}, true],
    ['largest PostgreSQL BIGINT', {generation:'9223372036854775807'}, true],
    ['overflow', {generation:'9223372036854775808'}, false],
    ['missing generation', {generation:''}, false],
    ['zero', {generation:'0'}, false],
    ['leading zero', {generation:'01'}, false],
    ['signed integer', {generation:'+1'}, false],
    ['exponent', {generation:'1e3'}, false],
    ['noncanonical whitespace', {generation:' 1'}, false],
    ['zero paymaster', {paymaster:'0x'+'0'.repeat(40)}, false],
    ['wrong address length', {paymaster:'0x123'}, false],
    ['control character in note', {note:'unsafe\nnote'}, false],
  ]
  for (const [label, overrides, accepted] of cases) {
    const result=runJq(evidenceFilter,{...base,...overrides})
    assert.equal(result.status===0, accepted, `${label}: ${result.stderr}`)
  }
})

test('the immutable admin command carries the approved generation without numeric conversion', () => {
  const block=source.split('command_json=$(jq -cn')[1]
  const filter=block.match(/'(\["\/usr\/local\/bin\/plether-aa-admin", "resume-issuance",[\s\S]*?)'/)?.[1]
  assert.ok(filter)
  const args={reason:'expected reason',generation:'9007199254740993',paymaster:'0x'+'a'.repeat(40),note:'operator note'}
  const result=runJq(filter,args)
  assert.equal(result.status,0,result.stderr)
  assert.deepEqual(JSON.parse(result.stdout),['/usr/local/bin/plether-aa-admin','resume-issuance','--expected-pause-event-id',args.generation,'--paymaster',args.paymaster,'--expected-reason',args.reason,'--operator-note',args.note])
  assert.match(source,/expectedPauseEventId:\$expected_pause_event_id/)
  assert.match(source,/\.previous_pause_event_id == \$generation/)
})

test('resume paymaster scope must match the selected API definition on Sepolia', () => {
  const block=source.split('if [ "$ACTION" = "resume-issuance" ] && ! jq')[1]
  const filter=block.match(/'(\[\.taskDefinition[\s\S]*?)' \\\n\s+"\$api_definition_file"/)?.[1]
  assert.ok(filter)
  const paymaster='0x'+'a'.repeat(40)
  const fixture=()=>({taskDefinition:{containerDefinitions:[{name:'plether-api',environment:[{name:'AA_PAYMASTER_ADDRESS',value:paymaster},{name:'PERPS_CHAIN_ID',value:'421614'}]}]}})
  for (const [label,mutate,accepted] of [
    ['selected matching deployment',()=>{},true],
    ['different paymaster',v=>{v.taskDefinition.containerDefinitions[0].environment[0].value='0x'+'b'.repeat(40)},false],
    ['different chain',v=>{v.taskDefinition.containerDefinitions[0].environment[1].value='1'},false],
    ['duplicate scope',v=>{v.taskDefinition.containerDefinitions[0].environment.push({name:'AA_PAYMASTER_ADDRESS',value:paymaster})},false],
  ]) {
    const value=fixture();mutate(value)
    const result=runJq(filter,{paymaster},value)
    assert.equal(result.status===0,accepted,`${label}: ${result.stderr}`)
  }
})

test('operator fingerprint instructions match the exact pre-approval workflow payload', () => {
  const runbook=readFileSync(new URL('../docs/runbooks/self-hosted-aa-rollout.md',import.meta.url),'utf8')
  const workflowFilter=source.split('- name: Record secret-safe dispatch fingerprint')[1].match(/'(\{schema:2,[\s\S]*?\})'/)?.[1]
  const documentedFilter=runbook.split('aa_admin_dispatch_fingerprint()')[1].match(/'(\{schema:2,[\s\S]*?\})'/)?.[1]
  assert.ok(workflowFilter && documentedFilter)
  const args={action:'resume-issuance',confirmation:'RUN RESUME-ISSUANCE ON SEPOLIA',expected_reason:'incident reason',expected_pause_event_id:'9007199254740993',expected_paymaster:'0x'+'a'.repeat(40),operator_note:'reviewed evidence'}
  const workflow=runJq(workflowFilter,args),documented=runJq(documentedFilter,args)
  assert.equal(workflow.status,0,workflow.stderr)
  assert.equal(documented.status,0,documented.stderr)
  assert.equal(documented.stdout,workflow.stdout)
  const changed=runJq(workflowFilter,{...args,expected_pause_event_id:'9007199254740994'})
  assert.notEqual(changed.stdout,workflow.stdout,'a later incident must change approved inputs')
})

test('source task-definition repository gate accepts both tagged and digest-pinned ECR references', () => {
  const fnMatch = source.match(/(is_valid_ecr_reference\(\) \{[\s\S]*?\n\s+\})/)
  assert.ok(fnMatch, 'extract actual is_valid_ecr_reference helper')
  const fnBody = fnMatch[1]
  const registry = '932542905614.dkr.ecr.ap-southeast-1.amazonaws.com'
  const repo = 'plether-api-sepolia'
  const digest = 'sha256:' + 'a'.repeat(64)
  const cases = [
    ['digest-pinned image', `${registry}/${repo}@${digest}`, true],
    ['tag-pinned image', `${registry}/${repo}:latest`, true],
    ['sha-tagged image', `${registry}/${repo}:v1.2.3-arm64`, true],
    ['wrong repository', `${registry}/other-repo@${digest}`, false],
    ['prefix-spoofed repository', `${registry}/${repo}-evil@${digest}`, false],
    ['wrong registry', `111111111111.dkr.ecr.ap-southeast-1.amazonaws.com/${repo}@${digest}`, false],
    ['short digest', `${registry}/${repo}@sha256:deadbeef`, false],
    ['uppercase digest', `${registry}/${repo}@sha256:${'A'.repeat(64)}`, false],
    ['empty tag', `${registry}/${repo}:`, false],
    ['tag plus digest suffix', `${registry}/${repo}:latest@${digest}`, false],
  ]
  for (const [label, reference, expected] of cases) {
    const script = `registry=${JSON.stringify(registry)}\n${fnBody}\nis_valid_ecr_reference ${JSON.stringify(reference)} ${JSON.stringify(repo)}\n`
    const result = spawnSync('bash', ['-c', script], {encoding: 'utf8'})
    assert.ifError(result.error)
    assert.equal(result.status === 0, expected, `${label}: ${result.stderr}`)
  }
})
