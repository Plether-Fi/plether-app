#!/usr/bin/env node
// One-shot base-table bootstrap for the fresh Frankfurt database only.
import assert from 'node:assert/strict'
import { resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { createHash } from 'node:crypto'
import { awsJson, verifyAccount } from './frankfurt-aa-secrets.mjs'

export const bootstrapProgram = String.raw`
const fs=require('fs'),crypto=require('crypto'),cp=require('child_process'),assert=require('assert/strict');
let stage='database-identity';
try {
  const url=new URL(process.env.DATABASE_URL);
  assert.equal(url.hostname,'plether-sepolia-aa-temp.c5yc28qc87mw.eu-central-1.rds.amazonaws.com');
  assert.equal(url.searchParams.get('sslmode'),'verify-full');
  assert.equal(url.pathname,'/plether');
  assert.equal(url.searchParams.get('sslrootcert'),'/etc/ssl/certs/aws-rds-global-bundle.pem');
  stage='schema-integrity';
  const schema=fs.readFileSync('/app/schema.sql','utf8');
  assert.equal(crypto.createHash('sha256').update(schema).digest('hex'),'b038354df90eb4cd3e79356976906a55078a9d9693b6810d3b96e9d5ead02bc7');
  const marker='-- Coherent hourly Senior/Junior vault observations';
  assert(schema.includes(marker));
  const sql=schema.slice(0,schema.indexOf(marker));
  assert.equal((sql.match(/CREATE TABLE IF NOT EXISTS/g)||[]).length,4);
  assert(!/\b(DROP|DELETE|TRUNCATE|ALTER)\b/.test(sql));
  stage='psql';
  cp.execFileSync('psql',['-X','--set=ON_ERROR_STOP=1','--single-transaction','--file=-'],{
    input:sql,env:{...process.env,PGHOST:url.hostname,PGPORT:url.port||'5432',PGDATABASE:'plether',
      PGUSER:decodeURIComponent(url.username),PGPASSWORD:decodeURIComponent(url.password),
      PGSSLMODE:'verify-full',PGSSLROOTCERT:url.searchParams.get('sslrootcert'),PGCONNECT_TIMEOUT:'15'},
    stdio:['pipe','ignore','pipe'],timeout:60000,
  });
  console.log(JSON.stringify({event:'frankfurt_spot_schema_bootstrapped',tables:4}));
} catch(error) {
  const sqlError=/ERROR:\s*([^\n]+)/.exec(error.stderr?.toString()||'')?.[1];
  console.error(JSON.stringify({event:'frankfurt_spot_schema_bootstrap_failed',stage,
    exitStatus:Number.isInteger(error.status)?error.status:null,
    sqlError:sqlError?.replace(/https?:\S+|postgresql:\S+/g,'[redacted]')}));
  process.exitCode=1;
}
// Let FireLens flush the one-shot result before ECS stops the essential task.
setTimeout(()=>{},5000);
`

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  assert.deepEqual(process.argv.slice(2),['--run'])
  verifyAccount()
  const cluster='plether-sepolia-aa-temp',startedBy='frankfurt-spot-schema-20260911'
  assert.equal(awsJson('ecs','list-tasks',['--cluster',cluster,'--started-by',startedBy]).taskArns.length,0,'Bootstrap already running')
  const service=awsJson('ecs','describe-services',['--cluster',cluster,'--services','plether-api']).services[0]
  const revision='arn:aws:ecs:eu-central-1:932542905614:task-definition/plether-sepolia-aa-temp:2'
  assert.equal(service.taskDefinition,revision)
  const definition=awsJson('ecs','describe-task-definition',['--task-definition',revision]).taskDefinition
  assert.equal(definition.containerDefinitions.find(c=>c.name==='plether-api').image,'932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-api-sepolia-aa-temp@sha256:4629cc418d52f183b5fdd21a2d4409124eaf7ea4e72229bd983c1602f67bb3be')
  const result=awsJson('ecs','run-task',[],{
    cluster,taskDefinition:revision,launchType:'FARGATE',count:1,startedBy,
    clientToken:createHash('sha256').update(startedBy+bootstrapProgram).digest('hex'),networkConfiguration:service.networkConfiguration,
    overrides:{containerOverrides:[{name:'plether-api',command:['node','-e',bootstrapProgram]}]},
  })
  assert.equal(result.failures.length,0)
  assert.equal(result.tasks.length,1)
  console.log(JSON.stringify({taskArn:result.tasks[0].taskArn,startedBy,expectedExitCode:0}))
}
