variable "provision_sepolia_rpc" {
  type        = bool
  default     = false
  description = "Provision an isolated Sepolia RPC pilot. Does not change any application RPC URL or AA configuration."
}

variable "sepolia_rpc" {
  description = "Reviewed pilot inputs. Secret values live in externally managed SSM SecureStrings, never in this object."
  type = object({
    ami_id                    = string
    nitro_image               = string
    snapshot_url              = string
    snapshot_metadata_sha256  = string
    hostname                  = string
    certificate_arn           = string
    parent_parameter          = string
    gateway_token_parameter   = string
    backup_url_parameter      = string
    backup_token_parameter    = optional(string, "")
    reference_url_parameter   = string
    reference_token_parameter = optional(string, "")
    kms_key_arns              = optional(list(string), [])
    state_history_blocks      = optional(number, 345600)
    staging_volume_gib        = optional(number, 4096)
    gateway_image             = optional(string, "")
  })
  default  = null
  nullable = true

  validation {
    condition = var.sepolia_rpc == null ? true : (
      can(regex("^ami-[0-9a-f]+$", var.sepolia_rpc.ami_id)) &&
      can(regex("^offchainlabs/nitro-node@sha256:[0-9a-f]{64}$", var.sepolia_rpc.nitro_image)) &&
      can(regex("^https://snapshot\\.arbitrum\\.foundation/sepolia-rollup/[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9a-f]+/$", var.sepolia_rpc.snapshot_url)) &&
      can(regex("^[0-9a-f]{64}$", var.sepolia_rpc.snapshot_metadata_sha256)) &&
      can(regex("^[a-z0-9][a-z0-9.-]+[a-z0-9]$", var.sepolia_rpc.hostname)) &&
      can(regex("^arn:aws:acm:ap-southeast-1:[0-9]{12}:certificate/", var.sepolia_rpc.certificate_arn)) &&
      alltrue([for name in [var.sepolia_rpc.parent_parameter, var.sepolia_rpc.gateway_token_parameter, var.sepolia_rpc.backup_url_parameter, var.sepolia_rpc.reference_url_parameter] : can(regex("^/[A-Za-z0-9_./-]+$", name))]) &&
      (var.sepolia_rpc.backup_token_parameter == "" || can(regex("^/[A-Za-z0-9_./-]+$", var.sepolia_rpc.backup_token_parameter))) &&
      (var.sepolia_rpc.reference_token_parameter == "" || can(regex("^/[A-Za-z0-9_./-]+$", var.sepolia_rpc.reference_token_parameter))) &&
      var.sepolia_rpc.state_history_blocks >= 16384 && var.sepolia_rpc.state_history_blocks <= 691200 && floor(var.sepolia_rpc.state_history_blocks) == var.sepolia_rpc.state_history_blocks &&
      (var.sepolia_rpc.staging_volume_gib == 0 || (var.sepolia_rpc.staging_volume_gib >= 4096 && var.sepolia_rpc.staging_volume_gib <= 8192)) &&
      (var.sepolia_rpc.gateway_image == "" || can(regex("^[0-9]{12}\\.dkr\\.ecr\\.ap-southeast-1\\.amazonaws\\.com/plether-rpc-gateway-sepolia@sha256:[0-9a-f]{64}$", var.sepolia_rpc.gateway_image)))
    )
    error_message = "RPC inputs require pinned images/snapshot metadata, Singapore ACM/ECR, absolute SSM names, finite recent history and sufficient optional staging."
  }
}
