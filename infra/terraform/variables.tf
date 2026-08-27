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
  type        = string
  default     = ""
  sensitive   = true
  description = "Pyth API key managed by Terraform. It must be entitled to every configured basket feed, including FX feeds; prefer pyth_api_key_ssm_parameter_name for an existing SecureString."
}

variable "enable_pyth_api_key" {
  type        = bool
  default     = false
  description = "Create and manage the Pyth API key SecureString from pyth_api_key."
}

variable "pyth_api_key_ssm_parameter_name" {
  type        = string
  default     = null
  nullable    = true
  description = "Existing Pyth API key SecureString parameter name. Sepolia defaults to /plether/sepolia/pyth-api-key. Set to an empty string to disable the external reference."

  validation {
    condition = (
      var.pyth_api_key_ssm_parameter_name == null
      || trimspace(var.pyth_api_key_ssm_parameter_name) == ""
      || startswith(trimspace(var.pyth_api_key_ssm_parameter_name), "/")
    )
    error_message = "pyth_api_key_ssm_parameter_name must be null, empty, or an absolute SSM parameter name beginning with /."
  }
}

variable "pyth_hermes_url" {
  type        = string
  default     = "https://pyth.dourolabs.app/hermes"
  description = "Upgraded Hermes base URL used by backend payload consumers."

  validation {
    condition = (
      trimspace(var.pyth_hermes_url) != ""
      && replace(lower(trimspace(var.pyth_hermes_url)), "/\\/+$/", "") != "https://hermes.pyth.network"
    )
    error_message = "pyth_hermes_url must be non-empty and cannot use the legacy Hermes endpoint because its payloads are incompatible with the deployed upgraded Pyth contract."
  }
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

variable "pyth_latest_max_age_seconds" {
  type        = string
  default     = "10"
  description = "Maximum accepted age of a latest Hermes payload before it may be promoted to the cache. Capped at 10 seconds to preserve headroom below the oracle's 15-second staleness limit."

  validation {
    condition     = can(regex("^([1-9]|10)$", trimspace(var.pyth_latest_max_age_seconds)))
    error_message = "pyth_latest_max_age_seconds must be a whole number from 1 through 10 to preserve headroom below the oracle's 15-second staleness limit."
  }
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
  default     = true
  description = "Authoritative managed sponsorship issuance/submission kill switch; disable explicitly only when sponsorship must be paused."
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
  description = "SNS topic ARN for operational CloudWatch alarms. Required in mainnet."

  validation {
    condition = var.operations_alarm_sns_topic_arn == trimspace(var.operations_alarm_sns_topic_arn) && (
      var.operations_alarm_sns_topic_arn == "" || can(
        regex("^arn:(aws|aws-us-gov|aws-cn):sns:[a-z0-9-]+:[0-9]{12}:[A-Za-z0-9_-]+(\\.fifo)?$", var.operations_alarm_sns_topic_arn)
      )
    )
    error_message = "operations_alarm_sns_topic_arn must be empty or a valid, whitespace-free SNS topic ARN with a 12-digit AWS account ID."
  }
}

variable "perps_candle_write_mode" {
  type        = string
  default     = "off"
  description = "Controls additive OHLCV rollup writes. Keep off until the candle schema is migrated, then enable dual writing per environment."

  validation {
    condition     = contains(["off", "dual"], var.perps_candle_write_mode)
    error_message = "perps_candle_write_mode must be off or dual."
  }
}

variable "perps_candle_read_mode" {
  type        = string
  default     = "legacy"
  description = "Selects the Perps basket-history read source. Rollup mode remains coverage-gated in the backend; shadow is reserved and has no v1 runtime behavior."

  validation {
    condition     = contains(["legacy", "shadow", "rollup"], var.perps_candle_read_mode)
    error_message = "perps_candle_read_mode must be legacy, shadow, or rollup."
  }
}

variable "perps_candle_read_intervals" {
  type        = string
  default     = ""
  description = "Comma-separated canonical candle intervals enabled for rollup reads during a canary. Empty keeps every rollup endpoint disabled."

  validation {
    condition = alltrue([
      for token in regexall("[^,[:space:]]+", var.perps_candle_read_intervals) :
      contains(["60", "180", "300", "900", "1800", "3600", "86400"], token)
    ])
    error_message = "perps_candle_read_intervals may contain only 60, 180, 300, 900, 1800, 3600, or 86400, separated by commas or whitespace."
  }
}

variable "perps_candle_shadow_sample_bps" {
  type        = number
  default     = 0
  description = "Reserved for a future bounded shadow comparator; this value has no v1 runtime effect and should remain zero."

  validation {
    condition = (
      floor(var.perps_candle_shadow_sample_bps) == var.perps_candle_shadow_sample_bps
      && var.perps_candle_shadow_sample_bps >= 0
      && var.perps_candle_shadow_sample_bps <= 10000
    )
    error_message = "perps_candle_shadow_sample_bps must be a whole number between 0 and 10000."
  }
}

variable "perps_candle_strict_coverage" {
  type        = bool
  default     = true
  description = "Require complete price and volume coverage before a rollup page may be served."
}

variable "perps_candle_lateness_seconds" {
  type        = number
  default     = 120
  description = "Minimum source-watermark delay before a price candle is considered finalized, from 0 to 86400 seconds."

  validation {
    condition = (
      floor(var.perps_candle_lateness_seconds) == var.perps_candle_lateness_seconds
      && var.perps_candle_lateness_seconds >= 0
      && var.perps_candle_lateness_seconds <= 86400
    )
    error_message = "perps_candle_lateness_seconds must be a whole number between 0 and 86400."
  }
}

variable "perps_candle_finalization_grace_seconds" {
  type        = number
  default     = 15
  description = "Bounded publication grace after candle source lateness elapses, from 0 to 60 seconds."

  validation {
    condition = (
      floor(var.perps_candle_finalization_grace_seconds) == var.perps_candle_finalization_grace_seconds
      && var.perps_candle_finalization_grace_seconds >= 0
      && var.perps_candle_finalization_grace_seconds <= 60
    )
    error_message = "perps_candle_finalization_grace_seconds must be a whole number between 0 and 60."
  }
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

variable "vault_history_rpc_url" {
  type        = string
  default     = ""
  sensitive   = true
  description = "Optional archive-capable Arbitrum RPC URL used for vault-history backfills. Empty falls back to perps_rpc_url."
}

variable "vault_history_house_pool_address" {
  type        = string
  default     = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
  description = "HousePool deployment whose Senior and Junior vault performance is indexed."
}

variable "vault_history_senior_vault_address" {
  type        = string
  default     = "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
  description = "Senior TrancheVault deployment whose performance is indexed."
}

variable "vault_history_junior_vault_address" {
  type        = string
  default     = "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
  description = "Junior TrancheVault deployment whose performance is indexed."
}

variable "vault_history_deployment_block" {
  type        = string
  default     = "302257125"
  description = "First Arbitrum block eligible for the configured vault deployment's performance history."
}

variable "vault_history_confirmations" {
  type        = string
  default     = "12"
  description = "Blocks subtracted from the live Arbitrum head before vault-history checkpoints are sampled."
}

variable "perps_usdc" {
  type    = string
  default = "0x1647e41f49ED6D688936092B5a291c4B28106343"
}

variable "perps_order_router" {
  type    = string
  default = "0x97A901dE2B267c307E264FD5F71403F8072F73e7"
}

variable "perps_plether_oracle" {
  type    = string
  default = "0xC69ec16EfB71F62984E9b2688396F34062277FdC"
}

variable "perps_cfd_engine" {
  type    = string
  default = "0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D"
}

variable "perps_cfd_engine_settlement_sidecar" {
  type    = string
  default = "0x288F70eC7cF0e16ae4FE4b91B5c266B047c83aFF"
}

variable "perps_cfd_engine_lens" {
  type    = string
  default = "0x140067daAdd28bE4b04e649EEaCf6F5ECbEe8C79"
}

variable "perps_margin_clearinghouse" {
  type    = string
  default = "0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211"
}

variable "perps_account_lens" {
  type    = string
  default = "0x429DA61a7a616DeDD84d2a51eB6Dc1bD72427dC1"
}

variable "perps_indexer_start_block" {
  type    = string
  default = "302257125"
}

variable "perps_indexer_confirmations" {
  type    = string
  default = "1"
}

variable "perps_indexer_batch_size" {
  type    = string
  default = "5000"
}

variable "perps_indexer_poll_seconds" {
  type    = string
  default = "12"
}

variable "insights_snapshot_poll_seconds" {
  type        = string
  default     = "60"
  description = "Interval between Plether Insights account snapshot cycles, in seconds. The worker enforces a minimum of 10 seconds."
}

variable "insights_snapshot_multicall_size" {
  type        = string
  default     = "10"
  description = "Number of account-lens reads per exact-block Multicall3 request. Must be between 0 and 100; set to 0 to roll back to direct eth_call requests."

  validation {
    condition     = can(regex("^(0|[1-9][0-9]?|100)$", var.insights_snapshot_multicall_size))
    error_message = "insights_snapshot_multicall_size must be an integer between 0 and 100."
  }
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
  default = "600"
}

variable "liquidation_worker_scan_batch_size" {
  type    = string
  default = "1000"
}

variable "liquidation_worker_multicall_size" {
  type        = string
  default     = "10"
  description = "Number of account-lens reads per liquidation-worker Multicall3 request. Must be between 1 and 100."

  validation {
    condition     = can(regex("^([1-9]|[1-9][0-9]|100)$", var.liquidation_worker_multicall_size))
    error_message = "liquidation_worker_multicall_size must be an integer between 1 and 100."
  }
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
  description = "Run the order keeper, basket worker, perps oracle updater, perps indexer, and Insights snapshot worker in one ECS service. The liquidation worker remains a dedicated service."
}

variable "workers_desired_count" {
  type        = number
  default     = 1
  description = "Desired task count for the consolidated workers service."

  validation {
    condition     = floor(var.workers_desired_count) == var.workers_desired_count && var.workers_desired_count >= 0
    error_message = "workers_desired_count must be a non-negative whole number."
  }
}

variable "api_container_cpu" {
  type        = number
  default     = 512
  description = "CPU units reserved for the foreground API task independently of background services."
}

variable "api_container_memory" {
  type        = number
  default     = 1024
  description = "Memory in MiB reserved for the foreground API task independently of background services."
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

variable "db_allocated_storage" {
  type        = number
  description = "RDS baseline allocated storage in GiB. RDS can grow this through autoscaling but cannot shrink it in place; set it explicitly from the live instance before planning."

  validation {
    condition     = floor(var.db_allocated_storage) == var.db_allocated_storage && var.db_allocated_storage >= 20
    error_message = "db_allocated_storage must be a whole number of at least 20 GiB."
  }
}

variable "db_storage_type" {
  type        = string
  description = "RDS storage volume type. Set this explicitly to gp2 or gp3 so plans cannot silently rely on the provider default."

  validation {
    condition     = contains(["gp2", "gp3"], var.db_storage_type)
    error_message = "db_storage_type must be either gp2 or gp3."
  }
}

variable "db_apply_immediately" {
  type        = bool
  default     = false
  description = "Apply pending RDS modifications immediately during a supervised Sepolia maintenance operation. Keep false for routine plans."
}

variable "db_backup_retention_days" {
  type        = number
  default     = 7
  description = "Automated RDS backup retention. Production rollup migrations require at least seven days."

  validation {
    condition = (
      floor(var.db_backup_retention_days) == var.db_backup_retention_days
      && var.db_backup_retention_days >= 1
      && var.db_backup_retention_days <= 35
    )
    error_message = "db_backup_retention_days must be a whole number between 1 and 35."
  }
}

variable "db_max_allocated_storage" {
  type        = number
  default     = 100
  description = "RDS storage autoscaling ceiling in GiB."

  validation {
    condition     = floor(var.db_max_allocated_storage) == var.db_max_allocated_storage && var.db_max_allocated_storage >= 50
    error_message = "db_max_allocated_storage must be a whole number of at least 50 GiB."
  }
}

variable "db_deletion_protection" {
  type        = bool
  default     = true
  description = "Protect the RDS instance from accidental deletion."
}

variable "db_skip_final_snapshot" {
  type        = bool
  default     = false
  description = "Skip the final database snapshot during destruction. Keep false outside disposable environments."
}

variable "db_final_snapshot_identifier" {
  type        = string
  default     = null
  nullable    = true
  description = "Region-unique identifier for this DB lifecycle's final snapshot. Required when final snapshots are enabled; choose a new value before recreating the DB so an earlier retained snapshot cannot block deletion."

  validation {
    condition = var.db_final_snapshot_identifier == null || try(
      length(var.db_final_snapshot_identifier) <= 255
      && can(regex("^[a-z]([a-z0-9-]*[a-z0-9])?$", var.db_final_snapshot_identifier))
      && !strcontains(var.db_final_snapshot_identifier, "--"),
      false
    )
    error_message = "db_final_snapshot_identifier must start with a lowercase letter, contain only lowercase letters, digits, and single hyphens, not end in a hyphen, and be at most 255 characters."
  }
}

variable "rds_free_storage_alarm_bytes" {
  type        = number
  default     = 5368709120
  description = "Free RDS storage threshold for the operational alarm (5 GiB by default)."

  validation {
    condition     = var.rds_free_storage_alarm_bytes > 0
    error_message = "rds_free_storage_alarm_bytes must be greater than zero."
  }
}

variable "rds_freeable_memory_alarm_bytes" {
  type        = number
  default     = 134217728
  description = "Freeable RDS memory threshold for the operational alarm (128 MiB by default)."

  validation {
    condition     = var.rds_freeable_memory_alarm_bytes > 0
    error_message = "rds_freeable_memory_alarm_bytes must be greater than zero."
  }
}

variable "rds_database_connections_alarm_threshold" {
  type        = number
  default     = 60
  description = "Database connection count that triggers an operational alarm."

  validation {
    condition = (
      floor(var.rds_database_connections_alarm_threshold) == var.rds_database_connections_alarm_threshold
      && var.rds_database_connections_alarm_threshold > 0
    )
    error_message = "rds_database_connections_alarm_threshold must be a positive whole number."
  }
}
