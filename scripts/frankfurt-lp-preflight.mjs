// One-shot read-only settlement qualification; never starts a second keeper loop.
import { awsJson, verifyAccount } from './frankfurt-aa-secrets.mjs'
try {
  if (process.argv[2] !== '--run') throw new Error('Explicit --run required')
  verifyAccount()
  const cluster = 'plether-sepolia-aa-temp'
  const service = awsJson('ecs', 'describe-services', ['--cluster', cluster, '--services', 'plether-workers']).services[0]
  if (!service.taskDefinition.startsWith(`arn:aws:ecs:eu-central-1:932542905614:task-definition/${cluster}-workers:`)) throw new Error('Wrong target')
  const source = awsJson('ecs', 'describe-task-definition', ['--task-definition', service.taskDefinition]).taskDefinition
  const keeper = structuredClone(source.containerDefinitions.find(c => c.name === 'plether-keeper'))
  if (!keeper.image.includes('@sha256:')) throw new Error('Unpinned image')
  keeper.command = ['plether-keeper', '--lp-settlement-preflight']
  keeper.secrets = keeper.secrets.filter(s => s.name !== 'KEEPER_PRIVATE_KEY')
  for (const e of keeper.environment) if (e.name === 'LP_SETTLEMENT_MODE') e.value = 'execute'
  const input = Object.fromEntries(['executionRoleArn', 'taskRoleArn', 'networkMode', 'requiresCompatibilities', 'cpu', 'memory', 'runtimePlatform'].map(k => [k, source[k]]))
  input.family = `${cluster}-lp-preflight`
  input.containerDefinitions = [keeper, source.containerDefinitions.find(c => c.name === 'otel-log-router')]
  const task = awsJson('ecs', 'register-task-definition', [], input).taskDefinition
  const result = awsJson('ecs', 'run-task', [], { cluster, taskDefinition: task.taskDefinitionArn,
    count: 1, launchType: 'FARGATE', networkConfiguration: service.networkConfiguration,
    startedBy: 'frankfurt-lp-preflight',
  })
  if (result.failures?.length || result.tasks?.length !== 1) throw new Error('Task did not start')
  console.log(JSON.stringify({ task: result.tasks[0].taskArn, taskDefinition: task.taskDefinitionArn, mode: 'read-only preflight' }))
} catch { console.error('Frankfurt LP preflight dispatch failed; sensitive details withheld'); process.exitCode = 1 }
