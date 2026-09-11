export function verifyTunnelMetadata(tunnel, instance, node, loadBalancer, groups, document) {
  const fail = message => { throw new Error(`Tunnel preflight: ${message}`) }
  if (instance?.InstanceId !== tunnel.instance_id || instance.State?.Name !== 'running'
    || instance.VpcId !== tunnel.vpc_id || instance.KeyName
    || instance.MetadataOptions?.HttpTokens !== 'required'
    || instance.SecurityGroups?.length !== 1 || instance.SecurityGroups[0].GroupId !== tunnel.security_group_id
    || !instance.Tags?.some(tag => tag.Key === 'Deployment' && tag.Value === 'sepolia-aa-temp')) fail('unexpected relay identity/topology')
  const version = node?.AgentVersion?.split('.').map(Number) ?? []
  const minimum = [3, 1, 1374, 0]
  const differing = minimum.findIndex((part, index) => part !== version[index])
  if (node?.InstanceId !== tunnel.instance_id || node.PingStatus !== 'Online' || version.length !== 4
    || version.some(part => !Number.isInteger(part)) || (differing >= 0 && version[differing] < minimum[differing])) fail('relay offline or SSM agent too old')
  if (loadBalancer?.LoadBalancerName !== 'plether-sepolia-aa-temp' || loadBalancer.Scheme !== 'internal'
    || loadBalancer.DNSName !== tunnel.remote_host || loadBalancer.VpcId !== tunnel.vpc_id
    || loadBalancer.SecurityGroups?.length !== 1) fail('backend is not the expected internal ALB')
  const relayGroup = groups.find(group => group.GroupId === tunnel.security_group_id)
  const apiGroup = groups.find(group => group.GroupId === loadBalancer.SecurityGroups[0])
  if (relayGroup?.VpcId !== tunnel.vpc_id || relayGroup.IpPermissions?.length !== 0) fail('relay has inbound access')
  const inbound = apiGroup?.IpPermissions
  if (apiGroup?.VpcId !== tunnel.vpc_id || inbound?.length !== 1 || inbound[0].IpProtocol !== 'tcp'
    || inbound[0].FromPort !== 80 || inbound[0].ToPort !== 80
    || inbound[0].IpRanges?.length || inbound[0].Ipv6Ranges?.length || inbound[0].PrefixListIds?.length
    || inbound[0].UserIdGroupPairs?.length !== 1 || inbound[0].UserIdGroupPairs[0].GroupId !== tunnel.security_group_id) fail('API allows traffic beyond the relay')
  const properties = document?.properties
  if (document?.schemaVersion !== '1.0' || document.sessionType !== 'Port'
    || Object.keys(document.parameters ?? {}).length || properties?.host !== tunnel.remote_host
    || properties.portNumber !== '80' || properties.localPortNumber !== '18081'
    || properties.type !== 'LocalPortForwarding') fail('SSM document is not fixed to this backend/port')
}
