import test from 'node:test'
import assert from 'node:assert/strict'
import { outputs } from './aa-frankfurt-fixture.mjs'
import { parseFrankfurtTarget } from './aa-frankfurt-target.ts'
import { verifyTunnelMetadata } from './aa-frankfurt-tunnel-lib.mjs'

function fixture() {
  const tunnel = parseFrankfurtTarget(outputs)
  const apiGroup = 'sg-1123456789abcdef0'
  return [tunnel, {
    InstanceId: tunnel.instance_id, State: { Name: 'running' }, VpcId: tunnel.vpc_id,
    MetadataOptions: { HttpTokens: 'required' }, SecurityGroups: [{ GroupId: tunnel.security_group_id }],
    Tags: [{ Key: 'Deployment', Value: 'sepolia-aa-temp' }],
  }, { InstanceId: tunnel.instance_id, PingStatus: 'Online', AgentVersion: '3.3.4000.0' }, {
    LoadBalancerName: 'plether-sepolia-aa-temp', Scheme: 'internal', DNSName: tunnel.remote_host,
    VpcId: tunnel.vpc_id, SecurityGroups: [apiGroup],
  }, [
    { GroupId: tunnel.security_group_id, VpcId: tunnel.vpc_id, IpPermissions: [] },
    { GroupId: apiGroup, VpcId: tunnel.vpc_id, IpPermissions: [{
      IpProtocol: 'tcp', FromPort: 80, ToPort: 80, IpRanges: [], Ipv6Ranges: [], PrefixListIds: [],
      UserIdGroupPairs: [{ GroupId: tunnel.security_group_id }],
    }] },
  ], { schemaVersion: '1.0', sessionType: 'Port', properties: {
    host: tunnel.remote_host, portNumber: '80', localPortNumber: '18081', type: 'LocalPortForwarding',
  } }]
}

test('fixed private tunnel accepts expected topology and supported agents', () => {
  verifyTunnelMetadata(...fixture())
  for (const version of ['3.1.1374.0', '3.2.0.0', '4.0.0.0']) {
    const args = fixture()
    args[2].AgentVersion = version
    verifyTunnelMetadata(...args)
  }
})

test('tunnel rejects public/SSH access, drift, offline agents and parameterized targets', () => {
  for (const mutate of [
    args => { args[1].KeyName = 'ssh-key' },
    args => { args[1].VpcId = 'vpc-other' },
    args => { args[1].State.Name = 'stopped' },
    args => { args[1].MetadataOptions.HttpTokens = 'optional' },
    args => { args[2].PingStatus = 'Offline' },
    args => { args[2].AgentVersion = '3.1.1373.0' },
    args => { args[3].Scheme = 'internet-facing' },
    args => { args[3].DNSName = 'other.internal' },
    args => { args[4][0].IpPermissions.push({ FromPort: 22 }) },
    args => { args[4][1].IpPermissions[0].IpRanges.push({ CidrIp: '0.0.0.0/0' }) },
    args => { args[4][1].IpPermissions[0].UserIdGroupPairs[0].GroupId = 'sg-other' },
    args => { args[5].parameters = { host: { type: 'String' } } },
    args => { args[5].properties.host = 'database.internal' },
    args => { args[5].properties.portNumber = '5432' },
  ]) {
    const args = fixture()
    mutate(args)
    assert.throws(() => verifyTunnelMetadata(...args), /Tunnel preflight/)
  }
})

test('local output parser rejects superseded public plans and arbitrary destinations', () => {
  assert.throws(() => parseFrankfurtTarget({ deployment_target: outputs.deployment_target,
    alb_url: { value: 'https://aa-temp-api.sepolia.plether.com' } }))
  for (const [key, value] of Object.entries({ remote_host: 'public.example.com', remote_port: 443,
    local_url: 'http://localhost:18081', document_name: 'AWS-StartPortForwardingSessionToRemoteHost',
    instance_id: 'i-other', transport: 'ssh' })) {
    const changed = structuredClone(outputs)
    changed.frankfurt_tunnel.value[key] = value
    assert.throws(() => parseFrankfurtTarget(changed))
  }
})

test('activated trading tunnel accepts only its reviewed private worker client', () => {
  const args = fixture()
  args[4].push({ GroupId: 'sg-worker', GroupName: 'plether-sepolia-aa-temp-worker-api-client',
    VpcId: args[0].vpc_id, IpPermissions: [], Tags: [{ Key: 'Deployment', Value: 'sepolia-aa-temp' }] })
  args[4][1].IpPermissions[0].UserIdGroupPairs.push({ GroupId: 'sg-worker' })
  verifyTunnelMetadata(...args)
  for (const mutate of [
    a => { a[4][2].VpcId = 'vpc-other' },
    a => { a[4][2].Tags = [] },
    a => { a[4][2].IpPermissions.push({ FromPort: 22 }) },
    a => { a[4][2].GroupName = 'arbitrary-worker' },
    a => { a[4][1].IpPermissions[0].UserIdGroupPairs.push({ GroupId: 'sg-other' }) },
  ]) {
    const changed = structuredClone(args)
    mutate(changed)
    assert.throws(() => verifyTunnelMetadata(...changed), /Tunnel preflight/)
  }
})
