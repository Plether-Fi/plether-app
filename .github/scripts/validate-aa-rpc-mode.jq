# Input: an ECS task definition (inner object); --arg container NAME.
# Output: a non-secret mode name. Missing mode preserves legacy dual default.
def checked:
  if . then true else error("Unsafe AA RPC mode, cohort, chain, or secret references") end;
def envvalues($name): [(.environment // [])[] | select(.name == $name) | .value];
def secretvalues($name): [(.secrets // [])[] | select(.name == $name) | .valueFrom];
[.containerDefinitions[] | select(.name == $container)] as $containers
| ($containers | length == 1 | checked) as $one
| $containers[0]
| envvalues("AA_RPC_MODE") as $modes
| ($modes | length <= 1 | checked) as $unique
| ($modes[0] // "dual-independent") as $mode
| secretvalues("PERPS_RPC_URL") as $primary
| secretvalues("AA_RECONCILER_SECONDARY_RPC_URL") as $verification
| (($primary | length) == 1 and ($verification | length) == 1 | checked) as $secrets
| if $mode == "single-provider-sepolia" then
    envvalues("AA_NATIVE_CANARY_OWNERS") as $owners
    | (envvalues("PERPS_CHAIN_ID") == ["421614"]
       and envvalues("AA_NATIVE_GLOBAL_ROLLOUT_ENABLED") == ["false"]
       and ($owners | length) == 1
       and ($owners[0] | test("^0x[0-9A-Fa-f]{40}(,0x[0-9A-Fa-f]{40})*$"))
       and ($owners[0] | ascii_downcase | split(",") | all(. != "0x0000000000000000000000000000000000000000"))
       and ($owners[0] | ascii_downcase | split(",") | length == (unique | length))
       and $primary == $verification
       and $primary == ["arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/perps-rpc-url"]
       | checked) as $canary
    | $mode
  elif $mode == "dual-independent" then
    ($primary[0] != $verification[0] | checked) as $distinct
    | $mode
  else error("Unknown AA_RPC_MODE") end
