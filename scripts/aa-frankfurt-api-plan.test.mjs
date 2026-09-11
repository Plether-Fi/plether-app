import test from 'node:test'
import assert from 'node:assert/strict'
import { reviewApiPlan } from './review-frankfurt-api-plan.mjs'
import { bootstrapProgram } from './bootstrap-frankfurt-spot-schema.mjs'

function fixture(stage = 'dormant') {
  const vars = { deployment_id:'sepolia-aa-temp', aws_region:'eu-central-1', expected_aws_account_id:'932542905614',
    perps_chain_id:'421614',frankfurt_activation_stage:stage,api_desired_count:stage==='dormant'?0:1,
    faucet_private_key:'',lp_settlement_mode:'off' }
  for(const key of ['configure_native_aa_backend','enable_native_aa_sponsorship','enable_native_aa_submission',
    'aa_native_global_rollout_enabled','enable_aa_sponsorship','provision_aa_proxy','provision_insights_registration',
    'enable_insights_registration','protection_worker_execution_enabled','aa_protection_commits_enabled']) vars[key]=false
  return {errored:false,variables:Object.fromEntries(Object.entries(vars).map(([k,value])=>[k,{value}])),
    resource_changes:['api',...Array.from({length:9},(_,i)=>`worker${i}`)].map(name=>({
      mode:'managed',type:'aws_ecs_service',address:`aws_ecs_service.${name}`,change:{
        actions:name==='api'&&stage==='api-readonly'?['update']:['no-op'],
        before:{name,desired_count:0},after:{name,desired_count:name==='api'?vars.api_desired_count:0},
      },
    }))}
}

test('base schema bootstrap fixes the database identity, verifies TLS and keeps credentials off argv',()=>{
  assert.match(bootstrapProgram,/plether-sepolia-aa-temp\.c5yc28qc87mw\.eu-central-1\.rds\.amazonaws\.com/)
  assert.match(bootstrapProgram,/verify-full/)
  assert.match(bootstrapProgram,/b038354df90eb4cd3e79356976906a55078a9d9693b6810d3b96e9d5ead02bc7/)
  assert.match(bootstrapProgram,/PGDATABASE:'plether'/)
  assert.match(bootstrapProgram,/PGPASSWORD:decodeURIComponent\(url.password\)/)
  assert.match(bootstrapProgram,/--single-transaction/)
  assert.match(bootstrapProgram,/CREATE TABLE IF NOT EXISTS/)
  assert(!bootstrapProgram.includes('--dbname'))
})

test('incremental reviewer permits only exact API activation, not workers, IAM or extra service changes',()=>{
  assert.equal(reviewApiPlan(fixture()).stage,'dormant')
  assert.equal(reviewApiPlan(fixture('api-readonly')).changes.length,1)
  for(const mutate of [
    p=>{p.variables.enable_native_aa_sponsorship.value=true},
    p=>{p.variables.aws_region.value='ap-southeast-1'},
    p=>{p.resource_changes[1].change.after.desired_count=1},
    p=>{p.resource_changes[0].change.after.task_definition='unreviewed'},
    p=>{p.resource_changes.push({mode:'managed',type:'aws_iam_role_policy',address:'aws_iam_role_policy.extra',change:{actions:['create'],after:{}}})},
  ]){const p=fixture('api-readonly');mutate(p);assert.throws(()=>reviewApiPlan(p))}
})

test('incremental reviewer checks complete container identity beyond the image',()=>{
  const p=fixture()
  const container={name:'worker',image:'932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-api-sepolia-aa-temp:latest'}
  const before={family:'plether-sepolia-aa-temp-worker',volume:[],container_definitions:JSON.stringify([container])}
  const after={...before,container_definitions:JSON.stringify([{...container,image:'932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-api-sepolia-aa-temp@sha256:4629cc418d52f183b5fdd21a2d4409124eaf7ea4e72229bd983c1602f67bb3be'}])}
  p.resource_changes.push({mode:'managed',type:'aws_ecs_task_definition',address:'aws_ecs_task_definition.worker',change:{actions:['delete','create'],before,after}})
  assert.equal(reviewApiPlan(p).changes.length,1)
  after.task_role_arn='unreviewed-role'
  assert.throws(()=>reviewApiPlan(p))
  delete after.task_role_arn
  after.container_definitions=JSON.stringify([{...JSON.parse(after.container_definitions)[0],privileged:true}])
  assert.throws(()=>reviewApiPlan(p))
})
