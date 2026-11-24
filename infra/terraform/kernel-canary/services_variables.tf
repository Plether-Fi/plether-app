variable "provision_services" {
  type        = bool
  default     = false
  description = "Create only the isolated Sepolia Kernel canary service skeleton. All service desired counts and the public route default to zero or false."
}

# Explicit live resource bindings are reviewed against read-only AWS data
# sources. They are identifiers, not credentials or copies of live state.
variable "live_vpc_id" {
  type    = string
  default = "vpc-0df986b0047f901d3"
}
variable "live_public_subnet_ids" {
  type    = list(string)
  default = ["subnet-0633bc3db0148548f", "subnet-0eef958040b7769a1"]
}
variable "live_api_alb_security_group_id" {
  type    = string
  default = "sg-08bc02814791dc349"
}
variable "live_api_https_listener_arn" {
  type    = string
  default = "arn:aws:elasticloadbalancing:ap-southeast-1:932542905614:listener/app/plether-sepolia/9544b926599859e9/d4248c26d2adefb7"
}
variable "live_aa_db_client_security_group_id" {
  type    = string
  default = "sg-0c095c11c27499ca7"
}
variable "live_rds_security_group_id" {
  type    = string
  default = "sg-085ef29e0a4896491"
}
variable "live_aa_db_egress_rule_id" {
  type    = string
  default = "sgr-0284a5c203c77b80b"
}
variable "live_aa_https_egress_rule_id" {
  type    = string
  default = "sgr-0f2eb55749d18ae33"
}
variable "live_rds_aa_ingress_rule_id" {
  type    = string
  default = "sgr-0d08be603bc66e167"
}
variable "live_api_task_definition" {
  type    = string
  default = "plether-sepolia:154"
}
variable "live_alto_task_definition" {
  type    = string
  default = "plether-sepolia-alto:12"
}
variable "live_reconciler_task_definition" {
  type    = string
  default = "plether-sepolia-aa-reconciler:72"
}
variable "api_hostname" {
  type    = string
  default = "api.sepolia.plether.com"
}

variable "kernel_canary_alto_image" {
  type    = string
  default = ""
}
variable "kernel_canary_api_image" {
  type    = string
  default = ""
}
variable "kernel_canary_alto_rpc_url_ssm_parameter_name" {
  type    = string
  default = "/plether/sepolia/perps-rpc-url"
  validation {
    condition     = var.kernel_canary_alto_rpc_url_ssm_parameter_name == "/plether/sepolia/perps-rpc-url"
    error_message = "The isolated canary Alto must use the reviewed managed Arbitrum Sepolia RPC parameter."
  }
}

variable "kernel_canary_reconciler_secondary_rpc_url_ssm_parameter_name" {
  type        = string
  default     = "/plether/sepolia/aa-reconciler-secondary-rpc-url"
  description = "Secondary RPC for the isolated Kernel reconciler. Use the dedicated archive RPC parameter if the shared provider prunes the paymaster deployment block."
  validation {
    condition = contains([
      "/plether/sepolia/aa-reconciler-secondary-rpc-url",
      "/plether/sepolia/kernel-canary-reconciler-archive-rpc-url",
    ], var.kernel_canary_reconciler_secondary_rpc_url_ssm_parameter_name)
    error_message = "The Kernel reconciler secondary RPC must use the shared Sepolia parameter or the dedicated canary archive RPC parameter."
  }
}
variable "kernel_canary_alto_executor_keys_ssm_parameter_name" {
  type    = string
  default = "/plether/sepolia/kernel-canary-alto-executor-private-keys"
  validation {
    condition     = can(regex("^/plether/sepolia/kernel-canary-alto-executor-[a-z0-9-]+$", var.kernel_canary_alto_executor_keys_ssm_parameter_name))
    error_message = "The executor parameter must use the dedicated Sepolia canary executor namespace."
  }
}
variable "kernel_canary_alto_utility_key_ssm_parameter_name" {
  type    = string
  default = "/plether/sepolia/kernel-canary-alto-utility-private-key"
  validation {
    condition     = can(regex("^/plether/sepolia/kernel-canary-alto-utility-[a-z0-9-]+$", var.kernel_canary_alto_utility_key_ssm_parameter_name))
    error_message = "The utility parameter must use the dedicated Sepolia canary utility namespace."
  }
}
variable "kernel_canary_origin_token_ssm_parameter_name" {
  type    = string
  default = "/plether/sepolia/kernel-canary-aa-origin-token"
  validation {
    condition     = can(regex("^/plether/sepolia/kernel-canary-aa-origin-[a-z0-9-]+$", var.kernel_canary_origin_token_ssm_parameter_name))
    error_message = "The origin parameter must use the dedicated Sepolia canary origin namespace."
  }
}
variable "kernel_canary_max_safe_lag_seconds" {
  type    = string
  default = "1800"
  validation {
    condition = (can(regex("^[1-9][0-9]*$", var.kernel_canary_max_safe_lag_seconds))
      && try(tonumber(var.kernel_canary_max_safe_lag_seconds) >= 60 && tonumber(var.kernel_canary_max_safe_lag_seconds) <= 1800, false)
    )
    error_message = "The Sepolia Kernel safe-head lag budget must be a canonical integer between 60 and 1800 seconds."
  }
}
variable "kernel_canary_poll_seconds" {
  type    = string
  default = "5"
  validation {
    condition = (can(regex("^[1-9][0-9]*$", var.kernel_canary_poll_seconds))
      && try(tonumber(var.kernel_canary_poll_seconds) <= 60, false)
    )
    error_message = "The Kernel reconciler poll interval must be a canonical integer between 1 and 60 seconds."
  }
}
variable "kernel_canary_batch_blocks" {
  type    = string
  default = "1000"
  validation {
    condition = (can(regex("^[1-9][0-9]*$", var.kernel_canary_batch_blocks))
      && try(tonumber(var.kernel_canary_batch_blocks) <= 2000, false)
    )
    error_message = "The Kernel reconciler batch size must be a canonical integer between 1 and 2000 blocks."
  }
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
  type    = string
  default = "[]"
}
variable "kernel_canary_preview_origin" {
  type    = string
  default = ""
}
variable "kernel_canary_simulators_attested" {
  type    = bool
  default = false
}
variable "kernel_canary_db_schema_confirmed" {
  type    = bool
  default = false
}
variable "kernel_canary_profile_attested" {
  type    = bool
  default = false
}
variable "kernel_canary_managed_alto_qualified" {
  type        = bool
  default     = false
  description = "Managed Alto completed the private Arbitrum Sepolia qualification on the exact reviewed image with normal validation enabled."
}
variable "kernel_canary_managed_alto_qualified_image" {
  type        = string
  default     = ""
  description = "The full account-local Alto image URI with immutable digest from the approved private managed qualification."
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
variable "kernel_canary_alto_desired_count" {
  type    = number
  default = 0
  validation {
    condition     = contains([0, 1], var.kernel_canary_alto_desired_count)
    error_message = "Canary Alto desired count must be zero or one."
  }
}
variable "kernel_canary_api_desired_count" {
  type    = number
  default = 0
  validation {
    condition     = contains([0, 1], var.kernel_canary_api_desired_count)
    error_message = "Canary API desired count must be zero or one."
  }
}
variable "kernel_canary_reconciler_desired_count" {
  type    = number
  default = 0
  validation {
    condition     = contains([0, 1], var.kernel_canary_reconciler_desired_count)
    error_message = "Canary reconciler desired count must be zero or one."
  }
}
