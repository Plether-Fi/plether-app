export type FrankfurtOutputs = {
  deployment_target?: { value?: { id?: string; region?: string; account_id?: string; chain_id?: string } }
  frankfurt_tunnel?: { value?: {
    instance_id?: string; document_name?: string; remote_host?: string; remote_port?: number
    local_url?: string; transport?: string; security_group_id?: string; vpc_id?: string
  } }
}

// Accept only applied outputs for this stack, never the old public-HTTPS plan.
export function parseFrankfurtTarget(outputs: FrankfurtOutputs) {
  const target = outputs.deployment_target?.value
  const tunnel = outputs.frankfurt_tunnel?.value
  if (target?.id !== 'sepolia-aa-temp' || target.region !== 'eu-central-1'
    || target.account_id !== '932542905614' || target.chain_id !== '421614') {
    throw new Error('Expected outputs for the isolated Frankfurt Arbitrum Sepolia target')
  }
  if (!tunnel || tunnel.transport !== 'aws-ssm-port-forwarding'
    || tunnel.local_url !== 'http://127.0.0.1:18081' || tunnel.remote_port !== 80
    || tunnel.document_name !== 'plether-sepolia-aa-temp-api-tunnel'
    || !/^i-[0-9a-f]{17}$/.test(tunnel.instance_id ?? '')
    || !/^sg-[0-9a-f]{17}$/.test(tunnel.security_group_id ?? '')
    || !/^vpc-[0-9a-f]{17}$/.test(tunnel.vpc_id ?? '')
    || !/^internal-plether-sepolia-aa-temp-[a-z0-9-]+\.eu-central-1\.elb\.amazonaws\.com$/.test(tunnel.remote_host ?? '')) {
    throw new Error('Expected applied private-tunnel outputs; public URLs and arbitrary hosts/ports are rejected')
  }
  return tunnel as Required<NonNullable<NonNullable<FrankfurtOutputs['frankfurt_tunnel']>['value']>>
}
