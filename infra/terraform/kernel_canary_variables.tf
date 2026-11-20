# A second Sepolia AA stack. These defaults do not create resources or change
# the running EntryPoint v0.8 Alto, API, reconciler, or Pages deployment.
variable "provision_kernel_canary_repositories" {
  type        = bool
  default     = false
  description = "Stage immutable canary API and Alto ECR repositories before building image digests or creating ECS resources."
}

variable "provision_kernel_canary" {
  type    = bool
  default = false
}

variable "kernel_canary_alto_image" {
  type        = string
  default     = ""
  description = "Reviewed patched Alto ARM64 image in plether-kernel-alto-sepolia, pinned by ECR sha256 digest."
}

variable "kernel_canary_api_image" {
  type        = string
  default     = ""
  description = "Reviewed Kernel API and reconciler ARM64 image in plether-kernel-api-sepolia, pinned by ECR sha256 digest."
}

variable "kernel_canary_alto_rpc_url_ssm_parameter_name" {
  type    = string
  default = "/plether/sepolia/app-rpc-url"
}

variable "kernel_canary_alto_rpc_token_ssm_parameter_name" {
  type    = string
  default = "/plether/sepolia/rpc-gateway-token"
}

variable "kernel_canary_alto_executor_keys_ssm_parameter_name" {
  type    = string
  default = "/plether/sepolia/kernel-canary-alto-executor-private-keys"
}

variable "kernel_canary_alto_utility_key_ssm_parameter_name" {
  type    = string
  default = "/plether/sepolia/kernel-canary-alto-utility-private-key"
}

variable "kernel_canary_origin_token_ssm_parameter_name" {
  type        = string
  default     = "/plether/sepolia/kernel-canary-aa-origin-token"
  description = "Existing dedicated 64-character hex SecureString shared only with the canary Pages worker."
}

variable "kernel_canary_alto_secrets_kms_key_arn" {
  type        = string
  default     = ""
  description = "Optional customer-managed KMS key for the canary Alto SecureStrings; empty for the SSM service key."
}

variable "kernel_canary_entrypoint_simulation_contract_v9" {
  type    = string
  default = ""
}

variable "kernel_canary_pimlico_simulation_contract" {
  type    = string
  default = ""
}

variable "kernel_canary_profile_json" {
  type    = string
  default = ""
}

variable "kernel_canary_sponsorship_json" {
  type    = string
  default = ""
}

variable "kernel_canary_owners_json" {
  type        = string
  default     = "[]"
  description = "Exact external and Privy Sepolia owner addresses admitted during the isolated canary."
}

variable "kernel_canary_alto_desired_count" {
  type    = number
  default = 0
  validation {
    condition     = contains([0, 1], var.kernel_canary_alto_desired_count)
    error_message = "Kernel canary Alto desired count must be 0 or 1."
  }
}

variable "kernel_canary_api_desired_count" {
  type    = number
  default = 0
  validation {
    condition     = contains([0, 1], var.kernel_canary_api_desired_count)
    error_message = "Kernel canary API desired count must be 0 or 1."
  }
}

variable "kernel_canary_reconciler_desired_count" {
  type    = number
  default = 0
  validation {
    condition     = contains([0, 1], var.kernel_canary_reconciler_desired_count)
    error_message = "Kernel canary reconciler desired count must be 0 or 1."
  }
}

variable "kernel_canary_route_enabled" {
  type    = bool
  default = false
}

variable "kernel_canary_eth_enabled" {
  type    = bool
  default = false
}

variable "kernel_canary_sponsorship_enabled" {
  type    = bool
  default = false
}

variable "kernel_canary_preview_origin" {
  type        = string
  default     = ""
  description = "Exact HTTPS Pages preview origin for the external and Privy wallet canary. Required before the Kernel API route opens."
}

variable "kernel_canary_simulators_attested" {
  type        = bool
  default     = false
  description = "Operator attestation that both pinned v0.9 simulation contracts were deployed and independently code-hash verified."
}

variable "kernel_canary_db_schema_confirmed" {
  type        = bool
  default     = false
  description = "Operator attestation that additive Kernel tables and indexes were applied with reviewed lock impact before canary API startup."
}

variable "kernel_canary_profile_attested" {
  type        = bool
  default     = false
  description = "Operator attestation that the fresh Kernel, Staker and validator profile passed independent RPC checks."
}

variable "kernel_canary_nitro_safemode_qualified" {
  type        = bool
  default     = false
  description = "Operator attestation that signed EntryPoint v0.9 SafeMode passed on Nitro through the authenticated gateway."
}
