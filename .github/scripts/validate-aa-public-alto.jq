# Active service's task definition; no credentials or provider details emitted.
# The deploy-alto workflow separately verifies the actual RPC chain ID before
# promotion. Metadata alone is not a substitute for that network check.
def values($container; $name):
  [$container.environment[] | select(.name == $name) | .value];
[.containerDefinitions[] | select(.name == "plether-alto")] as $containers
| ($containers | length == 1)
  and (
    values($containers[0]; "ALTO_SAFE_MODE") == ["true"]
    or (
      values($containers[0]; "ALTO_SAFE_MODE") == ["false"]
      and .family == "plether-sepolia-alto"
      and (.taskDefinitionArn | test("^arn:aws:ecs:ap-southeast-1:932542905614:task-definition/plether-sepolia-alto:[1-9][0-9]*$"))
      and values($containers[0]; "PLETHER_ALTO_NETWORK_CHAIN_ID") == ["421614"]
      and values($containers[0]; "PLETHER_ALTO_VALIDATION_POLICY") == ["sepolia-testnet-exception-v1"]
    )
  )
  and ([$containers[0].environment[] | select(.name == "ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION") | .value] == ["false"])
  and ([$containers[0].environment[] | select(.name == "ALTO_ENABLE_DEBUG_ENDPOINTS") | .value] == ["false"])
