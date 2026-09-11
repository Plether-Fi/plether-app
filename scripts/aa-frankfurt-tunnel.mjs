#!/usr/bin/env node
import { readFileSync } from 'node:fs'
import { spawn, spawnSync } from 'node:child_process'
import net from 'node:net'
import { parseFrankfurtTarget } from './aa-frankfurt-target.ts'
import { verifyTunnelMetadata } from './aa-frankfurt-tunnel-lib.mjs'
import { awsJson, verifyAccount } from './frankfurt-aa-secrets.mjs'

async function main() {
  if (process.argv.length !== 3) throw new Error('Usage: node scripts/aa-frankfurt-tunnel.mjs <applied-outputs.json>')
  const tunnel = parseFrankfurtTarget(JSON.parse(readFileSync(process.argv[2], 'utf8')))
  if (spawnSync('session-manager-plugin', ['--version'], { stdio: 'ignore' }).status !== 0) {
    throw new Error('Install the official AWS Session Manager plugin before starting the tunnel')
  }
  verifyAccount()
  const instance = awsJson('ec2', 'describe-instances', ['--instance-ids', tunnel.instance_id]).Reservations?.[0]?.Instances?.[0]
  const node = awsJson('ssm', 'describe-instance-information', ['--filters', `Key=InstanceIds,Values=${tunnel.instance_id}`]).InstanceInformationList?.[0]
  const lb = awsJson('elbv2', 'describe-load-balancers', ['--names', 'plether-sepolia-aa-temp']).LoadBalancers?.[0]
  const groups = awsJson('ec2', 'describe-security-groups', ['--group-ids', tunnel.security_group_id, ...(lb?.SecurityGroups ?? [])]).SecurityGroups ?? []
  const document = JSON.parse(awsJson('ssm', 'get-document', ['--name', tunnel.document_name, '--document-format', 'JSON']).Content)
  verifyTunnelMetadata(tunnel, instance, node, lb, groups, document)
  // Refuse occupied ports instead of silently targeting another local service.
  await new Promise((resolve, reject) => {
    const probe = net.createServer()
    probe.once('error', () => reject(new Error('Local port 18081 is already occupied; stop and inspect it first')))
    probe.listen(18081, '127.0.0.1', () => probe.close(resolve))
  })
  console.log('Starting authenticated tunnel to the private Frankfurt API at http://127.0.0.1:18081. Keep this terminal open; Ctrl-C closes it.')
  const child = spawn('aws', ['--profile', 'plether', '--region', 'eu-central-1', 'ssm', 'start-session',
    '--target', tunnel.instance_id, '--document-name', tunnel.document_name], { stdio: 'inherit' })
  child.once('error', () => { console.error('Unable to start Session Manager'); process.exitCode = 1 })
  child.once('exit', code => { process.exitCode = code ?? 1 })
  for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => child.kill(signal))
}
main().catch(error => { console.error(error.message); process.exitCode = 1 })
