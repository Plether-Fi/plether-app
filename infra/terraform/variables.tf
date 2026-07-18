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

variable "faucet_private_key" {
  type      = string
  default   = ""
  sensitive = true
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
  default = "0xf1e1B188b87525C51ECe4bae8627ae621D769651"
}

variable "perps_order_router" {
  type    = string
  default = "0x4A0a6c028164A1254e10C3e39cc89Af45090069e"
}

variable "perps_plether_oracle" {
  type    = string
  default = "0x8c95f554D728215b9f8D15b5F3Da5F5CD7Ba08bA"
}

variable "perps_cfd_engine" {
  type    = string
  default = "0xA1Ebfb8aD9C90367eA30A29592419d447E3f8224"
}

variable "perps_margin_clearinghouse" {
  type    = string
  default = "0x731bb0939CE531728459394A277B28Cbff8df049"
}

variable "perps_indexer_start_block" {
  type    = string
  default = "280884700"
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

variable "consolidate_workers" {
  type        = bool
  default     = false
  description = "Run keeper, basket worker, perps oracle updater, and perps indexer in one ECS service. Intended for cost-sensitive testnet environments."
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
