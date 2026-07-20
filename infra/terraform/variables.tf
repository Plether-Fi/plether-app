variable "aws_region" {
  type    = string
  default = "us-east-1"
}

variable "environment" {
  type    = string
  default = "sepolia"
}

variable "posthog_project_token" {
  type        = string
  sensitive   = true
  description = "PostHog project token (phc_*) used only by the ECS OTLP log driver."

  validation {
    condition     = startswith(var.posthog_project_token, "phc_")
    error_message = "posthog_project_token must be a PostHog project token beginning with phc_."
  }
}

variable "posthog_otlp_host" {
  type        = string
  default     = "eu.i.posthog.com"
  description = "PostHog OTLP/HTTP ingestion hostname without a scheme or path."

  validation {
    condition     = !strcontains(var.posthog_otlp_host, "://") && !strcontains(var.posthog_otlp_host, "/")
    error_message = "posthog_otlp_host must contain only a hostname, for example eu.i.posthog.com."
  }
}

variable "posthog_otlp_logs_uri" {
  type        = string
  default     = "/i/v1/logs"
  description = "PostHog OTLP/HTTP logs ingestion path."

  validation {
    condition     = startswith(var.posthog_otlp_logs_uri, "/")
    error_message = "posthog_otlp_logs_uri must begin with /."
  }
}

variable "rpc_url" {
  type      = string
  sensitive = true
}

variable "pyth_api_key" {
  type      = string
  default   = ""
  sensitive = true
}

variable "enable_pyth_api_key" {
  type    = bool
  default = false
}

variable "pyth_hermes_url" {
  type    = string
  default = "https://hermes.pyth.network"
}

variable "pyth_benchmarks_url" {
  type    = string
  default = "https://benchmarks.pyth.network"
}

variable "pyth_backfill_days" {
  type    = string
  default = "7"
}

variable "pyth_sample_interval_seconds" {
  type    = string
  default = "60"
}

variable "perps_rpc_url" {
  type      = string
  sensitive = true
}

variable "keeper_private_key" {
  type      = string
  sensitive = true
}

variable "oracle_updater_private_key" {
  type        = string
  sensitive   = true
  description = "Dedicated private key for the on-chain perps oracle updater. Must not be shared with another transaction worker."
}

variable "liquidation_keeper_private_key" {
  type      = string
  sensitive = true
}

variable "faucet_private_key" {
  type      = string
  default   = ""
  sensitive = true
}

variable "provision_aa_proxy" {
  type        = bool
  default     = false
  description = "Provision managed Pimlico proxy credentials on the API task, including while issuance is disabled for recovery."
}

variable "enable_aa_sponsorship" {
  type        = bool
  default     = false
  description = "Authoritative managed sponsorship issuance/submission kill switch."
}

variable "pimlico_api_key" {
  type      = string
  default   = ""
  sensitive = true
}

variable "pimlico_sponsorship_policy_id" {
  type      = string
  default   = ""
  sensitive = true
}

variable "aa_proxy_origin_token" {
  type      = string
  default   = ""
  sensitive = true
}

variable "aa_ip_rate_limit_per_minute" {
  type    = string
  default = "120"
}

variable "aa_account_rate_limit_per_minute" {
  type    = string
  default = "30"
}

variable "aa_max_request_bytes" {
  type    = string
  default = "262144"
}

variable "aa_sponsored_gas_alert_wei_per_hour" {
  type    = string
  default = "0"
}

variable "alb_certificate_arn" {
  type        = string
  default     = ""
  description = "ACM certificate ARN for the public API ALB. Required when the AA proxy is provisioned."
}

variable "api_hostname" {
  type        = string
  default     = ""
  description = "Public DNS hostname covered by alb_certificate_arn and pointed at the API ALB."
}

variable "operations_alarm_sns_topic_arn" {
  type        = string
  default     = ""
  description = "Optional SNS topic ARN for AA gas-usage and keeper-health CloudWatch alarms."
}

variable "db_password" {
  type      = string
  sensitive = true
}

variable "db_username" {
  type    = string
  default = "plether"
}

variable "chain_id" {
  type    = string
  default = "11155111"
}

variable "cors_origins" {
  type    = string
  default = "*"
}

variable "indexer_start_block" {
  type    = string
  default = "7726000"
}

variable "perps_chain_id" {
  type    = string
  default = "421614"
}

variable "perps_usdc" {
  type    = string
  default = "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3"
}

variable "perps_order_router" {
  type    = string
  default = "0x04E3103752f623fBcDcD01f588590Af4c53E4c1E"
}

variable "perps_plether_oracle" {
  type    = string
  default = "0xADfEd3bf768D810309B97b4dF9F9E77Eaa3a401c"
}

variable "perps_cfd_engine" {
  type    = string
  default = "0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a"
}

variable "perps_margin_clearinghouse" {
  type    = string
  default = "0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e"
}

variable "perps_indexer_start_block" {
  type    = string
  default = "288439939"
}

variable "perps_indexer_confirmations" {
  type    = string
  default = "120"
}

variable "perps_indexer_batch_size" {
  type    = string
  default = "5000"
}

variable "perps_indexer_poll_seconds" {
  type    = string
  default = "12"
}

variable "perps_oracle_updater_poll_seconds" {
  type    = string
  default = "30"
}

variable "perps_oracle_updater_max_payload_age_seconds" {
  type    = string
  default = "50"
}

variable "basket_worker_poll_seconds" {
  type    = string
  default = "5"
}

variable "keeper_poll_seconds" {
  type    = string
  default = "1"
}

variable "keeper_max_batch_size" {
  type    = string
  default = "20"
}

variable "keeper_confirmations" {
  type    = string
  default = "1"
}

variable "keeper_gas_buffer_bps" {
  type    = string
  default = "2000"
}

variable "keeper_fee_buffer_bps" {
  type    = string
  default = "2500"
}

variable "liquidation_worker_poll_seconds" {
  type    = string
  default = "1"
}

variable "liquidation_worker_scan_batch_size" {
  type    = string
  default = "100"
}

variable "liquidation_worker_confirmations" {
  type    = string
  default = "1"
}

variable "liquidation_worker_index_batch_size" {
  type    = string
  default = "5000"
}

variable "liquidation_worker_reorg_overlap_blocks" {
  type    = string
  default = "12"
}

variable "liquidation_worker_pending_replacement_seconds" {
  type    = string
  default = "120"
}

variable "liquidation_worker_gas_buffer_bps" {
  type    = string
  default = "2000"
}

variable "liquidation_worker_fee_buffer_bps" {
  type    = string
  default = "2500"
}

variable "liquidation_worker_desired_count" {
  type        = number
  default     = 1
  description = "Desired task count for the dedicated liquidation worker service."
}

variable "consolidate_workers" {
  type        = bool
  default     = false
  description = "Run the order keeper, basket worker, perps oracle updater, and perps indexer in one ECS service. The liquidation worker remains a dedicated service."
}

variable "workers_desired_count" {
  type        = number
  default     = 1
  description = "Desired task count for the consolidated workers service."
}

variable "container_cpu" {
  type    = number
  default = 256
}

variable "container_memory" {
  type    = number
  default = 1024
}

variable "workers_container_cpu" {
  type    = number
  default = 256
}

variable "workers_container_memory" {
  type    = number
  default = 1024
}

variable "db_instance_class" {
  type    = string
  default = "db.t4g.micro"
}
