variable "aa_funding_monitors" {
  description = "Public, dedicated signer inventory and conservative execution reserve bounds. No funding authority. Empty preserves the existing deployment."
  type = list(object({
    component    = string
    address      = string
    gasLimit     = string
    valueWei     = string
    feeBufferBps = string
  }))
  default = []
  validation {
    condition = length(var.aa_funding_monitors) <= 16 && alltrue([
      for m in var.aa_funding_monitors : contains(["alto", "keeper", "oracle", "liquidation", "protection", "lp_settlement"], m.component) &&
      can(regex("^0x[0-9a-fA-F]{40}$", m.address)) && lower(m.address) != "0x0000000000000000000000000000000000000000" &&
      can(regex("^[1-9][0-9]{0,9}$", m.gasLimit)) && try(tonumber(m.gasLimit) >= 21000 && tonumber(m.gasLimit) <= 1000000000, false) &&
      can(regex("^(0|[1-9][0-9]{0,77})$", m.valueWei)) &&
      can(regex("^(0|[1-9][0-9]{0,5})$", m.feeBufferBps)) && try(tonumber(m.feeBufferBps) <= 100000, false)
    ]) && length(distinct([for m in var.aa_funding_monitors : lower(m.address)])) == length(var.aa_funding_monitors)
    error_message = "Use at most 16 distinct dedicated public signers with valid unsigned reserve bounds."
  }
}

variable "aa_funding_monitor_image" {
  description = "Tested backend image digest containing /app/protection/funding-main.mjs. Required with a funding inventory; never reuse an older image implicitly."
  type        = string
  default     = ""
  validation {
    condition     = var.aa_funding_monitor_image == "" || can(regex("@sha256:[0-9a-f]{64}$", var.aa_funding_monitor_image))
    error_message = "Pin the funding observer to an immutable image digest."
  }
}

locals {
  aa_funding_components = sort(distinct([for m in var.aa_funding_monitors : m.component]))
  aa_funding_containers = length(var.aa_funding_monitors) == 0 ? [] : [{
    name             = "plether-funding-monitor"
    image            = var.aa_funding_monitor_image
    essential        = false
    command          = ["node", "/app/protection/funding-main.mjs"]
    logConfiguration = local.posthog_log_configuration
    restartPolicy    = { enabled = true, restartAttemptPeriod = 60 }
    mountPoints      = []
    portMappings     = []
    systemControls   = []
    volumesFrom      = []
    environment = [
      { name = "AA_FUNDING_MONITORS", value = jsonencode(var.aa_funding_monitors) },
      { name = "PERPS_RELEASE_MANIFEST", value = "/app/config/perps/arbitrum-sepolia-v2.json" },
    ]
    secrets = concat([
      { name = "DATABASE_URL", valueFrom = aws_ssm_parameter.database_url.arn },
      { name = "PERPS_RPC_URL", valueFrom = aws_ssm_parameter.perps_rpc_url.arn },
    ], local.perps_rpc_auth_token_secret)
  }]
}

resource "terraform_data" "aa_funding_inventory_guard" {
  input = local.aa_funding_components
  lifecycle {
    precondition {
      condition = length(var.aa_funding_monitors) == 0 || (
        local.frankfurt_preparation && var.consolidate_workers && local.native_aa_backend_configured &&
        var.aa_funding_monitor_image != "" && length(local.aa_funding_components) == 6
      )
      error_message = "Funding monitoring requires the complete six-role inventory, a tested image, and the consolidated Frankfurt native-AA canary. No other deployment is authorized."
    }
  }
}
