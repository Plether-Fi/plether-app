# These checks mirror the runtime parsers before starting a task. On-chain
# verification remains the responsibility of the profile attestation.
locals {
  kernel_zero_address      = "0x0000000000000000000000000000000000000000"
  kernel_zero_hash         = "0x0000000000000000000000000000000000000000000000000000000000000000"
  kernel_empty_hash        = "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
  kernel_profile_addresses = toset(["entryPoint", "factory", "implementation", "validator"])
  kernel_profile_hashes = toset([
    "entryPointCodeHash", "factoryCodeHash",
    "implementationCodeHash", "validatorCodeHash", "accountCodeHash",
  ])
  kernel_profile_keys = setunion(local.kernel_profile_addresses, local.kernel_profile_hashes, toset([
    "deployment", "chainId", "entryPointVersion", "accountVersion", "accountIndex",
  ]))
  kernel_canary_profile = try(jsondecode(var.kernel_canary_profile_json), null)
  kernel_canary_profile_valid = try(
    toset(keys(local.kernel_canary_profile)) == local.kernel_profile_keys
    && local.kernel_canary_profile.deployment == "kernel-sepolia-direct-v1"
    && local.kernel_canary_profile.chainId == 421614
    && local.kernel_canary_profile.entryPointVersion == "0.9"
    && local.kernel_canary_profile.accountVersion == "kernel-v4-uups"
    && local.kernel_canary_profile.accountIndex == 0
    && lower(local.kernel_canary_profile.entryPoint) == "0x433709009b8330fda32311df1c2afa402ed8d009"
    && alltrue([for key in local.kernel_profile_addresses :
      can(regex("^0x[0-9A-Fa-f]{40}$", local.kernel_canary_profile[key]))
      && lower(local.kernel_canary_profile[key]) != local.kernel_zero_address
    ])
    && alltrue([for key in local.kernel_profile_hashes :
      can(regex("^0x[0-9A-Fa-f]{64}$", local.kernel_canary_profile[key]))
      && !contains([local.kernel_zero_hash, local.kernel_empty_hash], lower(local.kernel_canary_profile[key]))
    ]),
    false,
  )

  kernel_canary_owners = try(jsondecode(var.kernel_canary_owners_json), null)
  kernel_canary_owners_valid = try(
    startswith(jsonencode(local.kernel_canary_owners), "[")
    && length(local.kernel_canary_owners) <= 100
    && length(distinct([for owner in local.kernel_canary_owners : lower(owner)])) == length(local.kernel_canary_owners)
    && alltrue([for owner in local.kernel_canary_owners :
      can(regex("^0x[0-9A-Fa-f]{40}$", owner)) && lower(owner) != local.kernel_zero_address
    ]),
    false,
  )

  kernel_canary_sponsor = try(jsondecode(var.kernel_canary_sponsorship_json), null)
  kernel_sponsor_bounds = {
    deploymentBlock      = [1, 9223372036854775807]
    minDepositWei        = [1, 1000000000000000000]
    verificationGasLimit = [1, 1000000]
    validitySeconds      = [60, 570]
  }
  kernel_sponsor_budget = try(local.kernel_canary_sponsor.budget, null)
  kernel_sponsor_budget_bounds = {
    maxCostWei            = [1, 10000000000000000]
    accountOutstandingWei = [try(local.kernel_sponsor_budget.maxCostWei, null), 20000000000000000]
    clientOutstandingWei  = [try(local.kernel_sponsor_budget.maxCostWei, null), 20000000000000000]
    globalOutstandingWei  = [try(max(local.kernel_sponsor_budget.accountOutstandingWei, local.kernel_sponsor_budget.clientOutstandingWei), null), 100000000000000000]
    accountHourlyWei      = [try(local.kernel_sponsor_budget.maxCostWei, null), 30000000000000000]
    globalHourlyWei       = [try(local.kernel_sponsor_budget.accountHourlyWei, null), 100000000000000000]
    globalDailyWei        = [try(local.kernel_sponsor_budget.globalHourlyWei, null), 250000000000000000]
  }
  kernel_canary_sponsor_valid = try(
    toset(keys(local.kernel_canary_sponsor)) == setunion(toset(keys(local.kernel_sponsor_bounds)), toset([
      "paymaster", "paymasterCodeHash", "policyId", "signerAddress", "deploymentBlockHash", "budget", "canaryOwners",
    ]))
    # A new database scope cannot settle the old paymaster's events. They are
    # selected by EntryPoint and paymaster on chain, independent of deployment ID.
    && lower(local.kernel_canary_sponsor.paymaster) != "0x778b6962467632e1cc9d91c6801443d17d842fdb"
    && alltrue([for key in ["paymaster", "signerAddress"] :
      can(regex("^0x[0-9A-Fa-f]{40}$", local.kernel_canary_sponsor[key]))
      && lower(local.kernel_canary_sponsor[key]) != local.kernel_zero_address
    ])
    && alltrue([for key in ["paymasterCodeHash", "policyId", "deploymentBlockHash"] :
      can(regex("^0x[0-9A-Fa-f]{64}$", local.kernel_canary_sponsor[key]))
      && lower(local.kernel_canary_sponsor[key]) != local.kernel_zero_hash
    ])
    && alltrue([for key, bounds in local.kernel_sponsor_bounds :
      can(regex("^[1-9][0-9]*$", jsonencode(local.kernel_canary_sponsor[key])))
      && local.kernel_canary_sponsor[key] >= bounds[0] && local.kernel_canary_sponsor[key] <= bounds[1]
    ])
    && toset(keys(local.kernel_sponsor_budget)) == toset(keys(local.kernel_sponsor_budget_bounds))
    && alltrue([for key, bounds in local.kernel_sponsor_budget_bounds :
      can(regex("^[1-9][0-9]*$", jsonencode(local.kernel_sponsor_budget[key])))
      && local.kernel_sponsor_budget[key] >= bounds[0] && local.kernel_sponsor_budget[key] <= bounds[1]
    ])
    && startswith(jsonencode(local.kernel_canary_sponsor.canaryOwners), "[")
    && length(local.kernel_canary_sponsor.canaryOwners) <= 100
    && alltrue([for owner in local.kernel_canary_sponsor.canaryOwners :
      can(regex("^0x[0-9A-Fa-f]{40}$", owner)) && lower(owner) != local.kernel_zero_address
    ]),
    false,
  )
}
