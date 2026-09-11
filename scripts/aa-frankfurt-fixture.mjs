// Public, synthetic topology for tests; no credentials or live resource IDs.
export const outputs = {
  deployment_target: { value: { id: 'sepolia-aa-temp', region: 'eu-central-1', account_id: '932542905614', chain_id: '421614' } },
  frankfurt_tunnel: { value: {
    instance_id: 'i-0123456789abcdef0', document_name: 'plether-sepolia-aa-temp-api-tunnel',
    remote_host: 'internal-plether-sepolia-aa-temp-1234.eu-central-1.elb.amazonaws.com',
    remote_port: 80, local_url: 'http://127.0.0.1:18081', transport: 'aws-ssm-port-forwarding',
    security_group_id: 'sg-0123456789abcdef0', vpc_id: 'vpc-0123456789abcdef0',
  } },
}
