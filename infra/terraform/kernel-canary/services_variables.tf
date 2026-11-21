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
variable "live_rpc_alb_security_group_id" {
  type    = string
  default = "sg-074181cf8fdb6a814"
}
variable "live_aa_db_client_security_group_id" {
  type    = string
  default = "sg-0c095c11c27499ca7"
}
variable "live_rds_security_group_id" {
  type    = string
  default = "sg-085ef29e0a4896491"
}
variable "live_rpc_node_security_group_id" {
  type    = string
  default = "sg-0f00b1175fca303e4"
}
variable "live_rpc_node_instance_id" {
  type    = string
  default = "i-04a6af37b4583a4ee"
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
  type    = string
  default = "/plether/sepolia/kernel-canary-aa-origin-token"
}
variable "kernel_canary_rpc_alb_security_group_id" {
  type    = string
  default = "sg-074181cf8fdb6a814"
}
variable "kernel_canary_max_safe_lag_seconds" {
  type    = string
  default = "1800"
}
variable "kernel_canary_poll_seconds" {
  type    = string
  default = "5"
}
variable "kernel_canary_batch_blocks" {
  type    = string
  default = "1000"
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
variable "kernel_canary_nitro_safemode_qualified" {
  type    = bool
  default = false
}
variable "kernel_canary_route_enabled" {
  type    = bool
  default = false
}
variable "kernel_canary_ssm_tunnel_enabled" {
  type        = bool
  default     = false
  description = "Temporarily admit the reviewed Nitro SSM node to the private canary Alto ALB for pre-route signed SafeMode smoke checks."
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
