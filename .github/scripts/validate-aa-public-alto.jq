# Active service's task definition; no credentials or provider details emitted.
[.containerDefinitions[] | select(.name == "plether-alto")] as $containers
| ($containers | length == 1)
  and ([$containers[0].environment[] | select(.name == "ALTO_SAFE_MODE") | .value] == ["true"])
  and ([$containers[0].environment[] | select(.name == "ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION") | .value] == ["false"])
  and ([$containers[0].environment[] | select(.name == "ALTO_ENABLE_DEBUG_ENDPOINTS") | .value] == ["false"])
