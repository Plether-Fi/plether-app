variable "aws_region" {
  type    = string
  default = "ap-southeast-1"
}

variable "expected_aws_account_id" {
  type        = string
  default     = "932542905614"
  description = "AWS account that Terraform is allowed to manage. The provider refuses credentials for any other account."

  validation {
    condition     = can(regex("^[0-9]{12}$", var.expected_aws_account_id))
    error_message = "expected_aws_account_id must be a 12-digit AWS account ID without separators."
  }
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

variable "pyth_history_url" {
  type        = string
  default     = "https://pyth.dourolabs.app/v1"
  description = "Pyth Pro History API base URL used for authenticated OHLC history after the legacy Benchmarks TradingView endpoints retired."

  validation {
    condition     = replace(lower(trimspace(var.pyth_history_url)), "/\\/+$/", "") == "https://pyth.dourolabs.app/v1"
    error_message = "pyth_history_url must use the official Pyth Pro History API base https://pyth.dourolabs.app/v1."
  }
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

variable "rpc_auth_token_ssm_parameter_name" {
  type        = string
  default     = ""
  description = "Existing SecureString containing the bearer token for RPC_URL. Leave empty while using a public or legacy URL-authenticated endpoint."

  validation {
    condition     = trimspace(var.rpc_auth_token_ssm_parameter_name) == "" || can(regex("^/plether/(sepolia|mainnet)/[A-Za-z0-9_.-]+$", trimspace(var.rpc_auth_token_ssm_parameter_name)))
    error_message = "rpc_auth_token_ssm_parameter_name must be empty or a canonical /plether/<environment>/<name> SSM parameter path."
  }
}

variable "perps_rpc_auth_token_ssm_parameter_name" {
  type        = string
  default     = ""
  description = "Existing SecureString containing the bearer token for PERPS_RPC_URL. Leave empty while using a public or legacy URL-authenticated endpoint."

  validation {
    condition     = trimspace(var.perps_rpc_auth_token_ssm_parameter_name) == "" || can(regex("^/plether/(sepolia|mainnet)/[A-Za-z0-9_.-]+$", trimspace(var.perps_rpc_auth_token_ssm_parameter_name)))
    error_message = "perps_rpc_auth_token_ssm_parameter_name must be empty or a canonical /plether/<environment>/<name> SSM parameter path."
  }
}

variable "keeper_private_key" {
  type      = string
  sensitive = true
}

variable "lp_settlement_private_key" {
  type        = string
  default     = ""
  sensitive   = true
  description = "Dedicated private key used only for LP epoch settlement. Required outside off mode and kept separate from every other transaction worker."

  validation {
    condition = var.lp_settlement_private_key == "" || (
      can(regex("^0x[0-9A-Fa-f]{64}$", var.lp_settlement_private_key))
      && lower(var.lp_settlement_private_key) != "0x0000000000000000000000000000000000000000000000000000000000000000"
    )
    error_message = "lp_settlement_private_key must be empty or a non-zero, 0x-prefixed 32-byte hexadecimal private key."
  }
}

variable "lp_settlement_signer_funding_confirmed" {
  type        = bool
  default     = false
  description = "Operator attestation that the dedicated LP signer was funded and its balance verified with --lp-settlement-preflight; required for observe or execute."
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

variable "faucet_proxy_origin_token" {
  type        = string
  default     = ""
  sensitive   = true
  description = "Dedicated Cloudflare Pages-to-faucet origin credential. Generate at least 32 random bytes and never reuse the AA proxy token."

  validation {
    condition = (
      var.faucet_proxy_origin_token == ""
      || (
        trimspace(var.faucet_proxy_origin_token) == var.faucet_proxy_origin_token
        && length(var.faucet_proxy_origin_token) >= 32
      )
    )
    error_message = "faucet_proxy_origin_token must be empty or contain at least 32 whitespace-free characters."
  }
}

variable "faucet_client_requests_per_hour" {
  type        = number
  default     = 20
  description = "Maximum accepted Sepolia faucet requests per pseudonymous Cloudflare client IP in a rolling hour."

  validation {
    condition = (
      floor(var.faucet_client_requests_per_hour) == var.faucet_client_requests_per_hour
      && var.faucet_client_requests_per_hour >= 1
      && var.faucet_client_requests_per_hour <= 1000000
    )
    error_message = "faucet_client_requests_per_hour must be a whole number between 1 and 1000000."
  }
}

variable "faucet_global_requests_per_hour" {
  type        = number
  default     = 200
  description = "Maximum accepted Sepolia faucet requests across the single API task in a rolling hour."

  validation {
    condition = (
      floor(var.faucet_global_requests_per_hour) == var.faucet_global_requests_per_hour
      && var.faucet_global_requests_per_hour >= 1
      && var.faucet_global_requests_per_hour <= 1000000
    )
    error_message = "faucet_global_requests_per_hour must be a whole number between 1 and 1000000."
  }
}

variable "provision_aa_proxy" {
  type        = bool
  default     = false
  description = "Provision managed Pimlico proxy credentials on the API task, including while issuance is disabled for recovery."
}

variable "enable_aa_sponsorship" {
  type        = bool
  default     = false
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

variable "provision_insights_registration" {
  type        = bool
  default     = false
  description = "Provision the private credentials and API configuration for first-party Insights competition registration."
}

variable "enable_insights_registration" {
  type        = bool
  default     = false
  description = "Permit the one-way persisted activation of first-party Insights registration after its secrets, competition, and public edge proxy are ready. Setting false does not pause or close an already activated database window."
}

variable "insights_registration_public_origin" {
  type        = string
  default     = "https://insights.plether.com"
  description = "Canonical browser origin allowed to run registration. Pages preview origins are intentionally excluded."

  validation {
    condition     = lower(var.insights_registration_public_origin) == var.insights_registration_public_origin && can(regex("^https://[a-z0-9.-]+$", var.insights_registration_public_origin))
    error_message = "insights_registration_public_origin must be a canonical lowercase HTTPS origin without credentials, port, path, query, or fragment."
  }
}

variable "insights_registration_origin_token" {
  type        = string
  default     = ""
  sensitive   = true
  description = "Shared secret sent only by the canonical Insights Pages Worker in X-Plether-Registration-Origin."

  validation {
    condition     = var.insights_registration_origin_token == "" || can(regex("^[!-~]{32,}$", var.insights_registration_origin_token))
    error_message = "insights_registration_origin_token must be empty or at least 32 printable non-whitespace ASCII characters with no control characters."
  }
}

variable "insights_registration_origin_token_next" {
  type        = string
  default     = ""
  sensitive   = true
  description = "Optional next Pages-to-backend registration origin token accepted during a controlled rotation overlap."

  validation {
    condition     = var.insights_registration_origin_token_next == "" || can(regex("^[!-~]{32,}$", var.insights_registration_origin_token_next))
    error_message = "insights_registration_origin_token_next must be empty or at least 32 printable non-whitespace ASCII characters with no control characters."
  }
}

variable "turnstile_secret_key" {
  type        = string
  default     = ""
  sensitive   = true
  description = "Cloudflare Turnstile server-side Siteverify secret for Insights registration."

  validation {
    condition     = var.turnstile_secret_key == "" || can(regex("^[!-~]+$", var.turnstile_secret_key))
    error_message = "turnstile_secret_key must be empty or printable non-whitespace ASCII without control characters."
  }
}

variable "turnstile_expected_hostname" {
  type        = string
  default     = "insights.plether.com"
  description = "Exact Turnstile hostname accepted by the registration API."

  validation {
    condition     = lower(var.turnstile_expected_hostname) == var.turnstile_expected_hostname && can(regex("^[a-z0-9.-]+$", var.turnstile_expected_hostname))
    error_message = "turnstile_expected_hostname must be a canonical lowercase hostname without a scheme, port, path, or wildcard."
  }
}

variable "turnstile_expected_action" {
  type        = string
  default     = "competition_registration"
  description = "Exact Turnstile widget action accepted by Siteverify validation."

  validation {
    condition     = var.turnstile_expected_action == "competition_registration"
    error_message = "turnstile_expected_action must equal competition_registration so the browser widget and server validation cannot drift."
  }
}

variable "x_oauth_client_id" {
  type        = string
  default     = ""
  description = "X OAuth 2.0 client ID for the Insights registration application."

  validation {
    condition     = var.x_oauth_client_id == "" || can(regex("^[!-~]+$", var.x_oauth_client_id))
    error_message = "x_oauth_client_id must be empty or printable non-whitespace ASCII without control characters."
  }
}

variable "x_oauth_client_secret" {
  type        = string
  default     = ""
  sensitive   = true
  description = "X OAuth 2.0 client secret for the Insights registration application."

  validation {
    condition     = var.x_oauth_client_secret == "" || can(regex("^[!-~]+$", var.x_oauth_client_secret))
    error_message = "x_oauth_client_secret must be empty or printable non-whitespace ASCII without control characters."
  }
}

variable "x_oauth_callback_url" {
  type        = string
  default     = "https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/x/callback"
  description = "Exact X OAuth callback URL registered for the September 2026 competition."

  validation {
    condition     = can(regex("^https://[^/?#]+/api/insights/v1/competitions/[^/?#]+/registrations/x/callback$", var.x_oauth_callback_url))
    error_message = "x_oauth_callback_url must be a clean HTTPS Insights registration callback URL without query parameters or a fragment."
  }
}

variable "x_target_user_id" {
  type        = string
  default     = ""
  description = "Stable numeric X user ID that registrants must follow."

  validation {
    condition     = var.x_target_user_id == "" || can(regex("^[0-9]+$", var.x_target_user_id))
    error_message = "x_target_user_id must be empty or contain only decimal digits."
  }
}

variable "x_target_handle" {
  type        = string
  default     = "plether_fi"
  description = "Public X handle displayed by the registration UI, without @."

  validation {
    condition     = can(regex("^[A-Za-z0-9_]{1,15}$", var.x_target_handle))
    error_message = "x_target_handle must be a valid X handle without @."
  }
}

variable "insights_registration_email_keys" {
  type        = map(string)
  default     = {}
  sensitive   = true
  description = "Versioned AES-256-GCM email keyring. Values are 32-byte standard-base64 keys; retain old entries during rotation."

  validation {
    condition = alltrue([
      for version, key in var.insights_registration_email_keys :
      can(regex("^v[1-9][0-9]*$", version)) && can(regex("^[A-Za-z0-9+/]{43}=$", key))
    ])
    error_message = "insights_registration_email_keys must map v1-style versions to 32-byte standard-base64 keys."
  }
}

variable "insights_registration_email_key_version" {
  type        = string
  default     = "v1"
  description = "Active email-encryption key version; it must exist in insights_registration_email_keys."

  validation {
    condition     = can(regex("^v[1-9][0-9]*$", var.insights_registration_email_key_version))
    error_message = "insights_registration_email_key_version must use the v1, v2, ... format."
  }
}

variable "insights_registration_email_hmac_key_base64" {
  type        = string
  default     = ""
  sensitive   = true
  description = "Stable 32-byte standard-base64 HMAC key for normalized-email uniqueness. Rotate only with an explicit digest migration."

  validation {
    condition     = var.insights_registration_email_hmac_key_base64 == "" || can(regex("^[A-Za-z0-9+/]{43}=$", var.insights_registration_email_hmac_key_base64))
    error_message = "insights_registration_email_hmac_key_base64 must be empty or a 32-byte standard-base64 key."
  }
}

variable "insights_registration_session_ttl_seconds" {
  type        = number
  default     = 1800
  description = "Registration session lifetime in seconds."

  validation {
    condition     = floor(var.insights_registration_session_ttl_seconds) == var.insights_registration_session_ttl_seconds && var.insights_registration_session_ttl_seconds >= 300 && var.insights_registration_session_ttl_seconds <= 3600
    error_message = "insights_registration_session_ttl_seconds must be a whole number from 300 through 3600."
  }
}

variable "insights_registration_ip_rate_limit_per_minute" {
  type        = number
  default     = 10
  description = "Per-client-IP registration request limit per minute."

  validation {
    condition     = floor(var.insights_registration_ip_rate_limit_per_minute) == var.insights_registration_ip_rate_limit_per_minute && var.insights_registration_ip_rate_limit_per_minute >= 1 && var.insights_registration_ip_rate_limit_per_minute <= 1000
    error_message = "insights_registration_ip_rate_limit_per_minute must be a whole number from 1 through 1000."
  }
}

variable "insights_registration_session_rate_limit_per_minute" {
  type        = number
  default     = 30
  description = "Per-session registration request limit per minute."

  validation {
    condition     = floor(var.insights_registration_session_rate_limit_per_minute) == var.insights_registration_session_rate_limit_per_minute && var.insights_registration_session_rate_limit_per_minute >= 1 && var.insights_registration_session_rate_limit_per_minute <= 5000
    error_message = "insights_registration_session_rate_limit_per_minute must be a whole number from 1 through 5000."
  }
}

variable "insights_registration_rules_version" {
  type        = string
  default     = "2026-09-13"
  description = "Immutable rules document version recorded with consent."

  validation {
    condition     = can(regex("^[A-Za-z0-9._-]+$", var.insights_registration_rules_version))
    error_message = "insights_registration_rules_version may contain only letters, digits, dot, underscore, and hyphen."
  }
}

variable "insights_registration_privacy_version" {
  type        = string
  default     = "2026-09-13"
  description = "Immutable privacy notice version recorded with consent."

  validation {
    condition     = can(regex("^[A-Za-z0-9._-]+$", var.insights_registration_privacy_version))
    error_message = "insights_registration_privacy_version may contain only letters, digits, dot, underscore, and hyphen."
  }
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
  description = "SNS topic ARN for operational CloudWatch alarms. Required in mainnet and whenever LP settlement is active."

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
  type        = string
  sensitive   = true
  description = "RDS master password. It is percent-encoded before inclusion in the libpq URI."

  validation {
    condition     = can(regex("^[!-~]{8,128}$", var.db_password))
    error_message = "db_password must contain 8-128 printable non-whitespace ASCII characters. URI-reserved characters are supported and percent-encoded."
  }
}

variable "db_username" {
  type        = string
  default     = "plether"
  description = "RDS/PostgreSQL master username. It is percent-encoded before inclusion in the libpq URI."

  validation {
    condition     = can(regex("^[A-Za-z][A-Za-z0-9_]{0,62}$", var.db_username))
    error_message = "db_username must start with a letter and contain at most 63 ASCII letters, digits, or underscores."
  }
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

variable "vault_history_house_pool_address" {
  type        = string
  default     = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
  description = "HousePool deployment whose Senior and Junior vault performance and canonical events are indexed."

  validation {
    condition     = can(regex("^0x[0-9A-Fa-f]{40}$", var.vault_history_house_pool_address)) && lower(var.vault_history_house_pool_address) != "0x0000000000000000000000000000000000000000"
    error_message = "vault_history_house_pool_address must be a nonzero Ethereum address."
  }
}

variable "vault_history_senior_vault_address" {
  type        = string
  default     = "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
  description = "Senior TrancheVault deployment whose performance, holders, and requests are indexed."

  validation {
    condition     = can(regex("^0x[0-9A-Fa-f]{40}$", var.vault_history_senior_vault_address)) && lower(var.vault_history_senior_vault_address) != "0x0000000000000000000000000000000000000000"
    error_message = "vault_history_senior_vault_address must be a nonzero Ethereum address."
  }
}

variable "vault_history_junior_vault_address" {
  type        = string
  default     = "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
  description = "Junior TrancheVault deployment whose performance, holders, and requests are indexed."

  validation {
    condition     = can(regex("^0x[0-9A-Fa-f]{40}$", var.vault_history_junior_vault_address)) && lower(var.vault_history_junior_vault_address) != "0x0000000000000000000000000000000000000000"
    error_message = "vault_history_junior_vault_address must be a nonzero Ethereum address."
  }
}

variable "vault_history_deployment_block" {
  type        = string
  default     = "302257125"
  description = "First Arbitrum block eligible for the configured vault deployment's performance and event history."

  validation {
    condition     = can(regex("^(0|[1-9][0-9]*)$", var.vault_history_deployment_block))
    error_message = "vault_history_deployment_block must be a canonical unsigned decimal block number."
  }
}

variable "vault_history_confirmations" {
  type        = string
  default     = "12"
  description = "Blocks subtracted from the live Arbitrum head before vault history and events are published."

  validation {
    condition     = var.vault_history_confirmations == "12"
    error_message = "vault_history_confirmations must remain at the reviewed 12-block depth."
  }
}

variable "perps_usdc" {
  type    = string
  default = "0x1647e41f49ED6D688936092B5a291c4B28106343"
}

variable "perps_order_router" {
  type    = string
  default = "0x97A901dE2B267c307E264FD5F71403F8072F73e7"
}

variable "perps_house_pool" {
  type        = string
  default     = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
  description = "HousePool bound to the configured v1.2.0 settlement monitor."

  validation {
    condition = (
      can(regex("^0x[0-9A-Fa-f]{40}$", var.perps_house_pool))
      && lower(var.perps_house_pool) != "0x0000000000000000000000000000000000000000"
    )
    error_message = "perps_house_pool must be a non-zero canonical Ethereum address."
  }
}

variable "perps_senior_vault" {
  type        = string
  default     = "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
  description = "Senior TrancheVault bound to the configured HousePool settlement release."

  validation {
    condition = (
      can(regex("^0x[0-9A-Fa-f]{40}$", var.perps_senior_vault))
      && lower(var.perps_senior_vault) != "0x0000000000000000000000000000000000000000"
    )
    error_message = "perps_senior_vault must be a non-zero canonical Ethereum address."
  }
}

variable "perps_junior_vault" {
  type        = string
  default     = "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
  description = "Junior TrancheVault bound to the configured HousePool settlement release."

  validation {
    condition = (
      can(regex("^0x[0-9A-Fa-f]{40}$", var.perps_junior_vault))
      && lower(var.perps_junior_vault) != "0x0000000000000000000000000000000000000000"
    )
    error_message = "perps_junior_vault must be a non-zero canonical Ethereum address."
  }
}

variable "perps_settlement_monitor_lens" {
  type        = string
  default     = "0xd251AC0BD90780c48F31F575152808315200664E"
  description = "Settlement Monitor facade used by the keeper. Never configure the sidecar address here."

  validation {
    condition = (
      can(regex("^0x[0-9A-Fa-f]{40}$", var.perps_settlement_monitor_lens))
      && lower(var.perps_settlement_monitor_lens) != "0x0000000000000000000000000000000000000000"
      && lower(var.perps_settlement_monitor_lens) != "0xe1fc0a465dabdfd8ee33d4aa960108f800b3f151"
    )
    error_message = "perps_settlement_monitor_lens must be the facade, never the v1.2.0 monitor sidecar."
  }
}

variable "perps_order_lifecycle_book" {
  description = "V2 immutable order lifecycle book from the pinned bounded-order deployment"
  type        = string
  default     = ""

  validation {
    condition = (
      var.perps_order_lifecycle_book == ""
      || (
        can(regex("^0x[0-9A-Fa-f]{40}$", var.perps_order_lifecycle_book))
        && lower(var.perps_order_lifecycle_book) != "0x0000000000000000000000000000000000000000"
      )
    )
    error_message = "perps_order_lifecycle_book must be empty or a nonzero Ethereum address."
  }
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

variable "insights_active_competition_slug" {
  type        = string
  default     = ""
  description = "Explicit Insights competition seed/selection slug. Leave empty to preserve the existing deployed competition; set to testnet-trading-2026-09 only with the new release addresses."

  validation {
    condition     = var.insights_active_competition_slug == "" || can(regex("^[a-z0-9]+(-[a-z0-9]+)*$", var.insights_active_competition_slug))
    error_message = "insights_active_competition_slug must be empty or a lowercase hyphenated slug."
  }
}

variable "insights_competition_release_id" {
  type        = string
  default     = ""
  description = "Explicit release-manifest identifier for one-time competition release binding. Leave empty while September is registration-only; binding requires testnet-trading-2026-09."

  validation {
    condition     = var.insights_competition_release_id == "" || can(regex("^[a-z0-9]+(-[a-z0-9]+)*$", var.insights_competition_release_id))
    error_message = "insights_competition_release_id must be empty or a lowercase hyphenated identifier."
  }
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

  validation {
    condition     = can(regex("^[1-9][0-9]*$", var.keeper_poll_seconds)) && try(tonumber(var.keeper_poll_seconds) <= 3600, false)
    error_message = "keeper_poll_seconds must be a canonical whole number from 1 through 3600."
  }
}

variable "keeper_idle_poll_seconds" {
  type        = string
  default     = "5"
  description = "Order-keeper polling cadence while its durable pending queue is empty."

  validation {
    condition = (
      can(regex("^[1-9][0-9]*$", var.keeper_idle_poll_seconds))
      && try(tonumber(var.keeper_idle_poll_seconds) <= 3600, false)
    )
    error_message = "keeper_idle_poll_seconds must be a canonical whole number from 1 through 3600."
  }
}

variable "keeper_max_batch_size" {
  type    = string
  default = "20"
}

variable "keeper_confirmations" {
  type    = string
  default = "1"

  validation {
    condition     = can(regex("^[1-9][0-9]*$", var.keeper_confirmations)) && try(tonumber(var.keeper_confirmations) <= 10000, false)
    error_message = "keeper_confirmations must be a canonical whole number from 1 through 10000."
  }
}

variable "keeper_gas_buffer_bps" {
  type    = string
  default = "2000"
}

variable "keeper_fee_buffer_bps" {
  type    = string
  default = "2500"
}

variable "lp_settlement_mode" {
  type        = string
  default     = "off"
  description = "LP epoch settlement mode: off disables monitoring, observe performs all read/simulation checks without broadcasting, and execute permits bounded transactions."

  validation {
    condition     = contains(["off", "observe", "execute"], var.lp_settlement_mode)
    error_message = "lp_settlement_mode must be off, observe, or execute."
  }
}

variable "lp_settlement_poll_seconds" {
  type        = string
  default     = "15"
  description = "Exact interval between LP settlement monitor cycles in the shared keeper process."

  validation {
    condition     = var.lp_settlement_poll_seconds == "15"
    error_message = "lp_settlement_poll_seconds must be exactly 15."
  }
}

variable "lp_settlement_max_drain_transactions" {
  type        = number
  default     = 4
  description = "Maximum LP settlement transactions confirmed successfully during one eligible drain cycle."

  validation {
    condition = (
      floor(var.lp_settlement_max_drain_transactions) == var.lp_settlement_max_drain_transactions
      && var.lp_settlement_max_drain_transactions >= 1
      && var.lp_settlement_max_drain_transactions <= 4
    )
    error_message = "lp_settlement_max_drain_transactions must be a whole number from 1 through 4."
  }
}

variable "lp_settlement_pending_replacement_seconds" {
  type        = number
  default     = 60
  description = "Age in seconds at which an unconfirmed LP settlement transaction becomes eligible for same-nonce replacement."

  validation {
    condition = (
      floor(var.lp_settlement_pending_replacement_seconds) == var.lp_settlement_pending_replacement_seconds
      && var.lp_settlement_pending_replacement_seconds >= 60
      && var.lp_settlement_pending_replacement_seconds <= 3600
    )
    error_message = "lp_settlement_pending_replacement_seconds must be a whole number from 60 through 3600."
  }
}

variable "lp_settlement_max_replacements" {
  type        = number
  default     = 3
  description = "Maximum same-nonce fee-bump replacements allowed for one LP settlement transaction."

  validation {
    condition = (
      floor(var.lp_settlement_max_replacements) == var.lp_settlement_max_replacements
      && var.lp_settlement_max_replacements >= 0
      && var.lp_settlement_max_replacements <= 3
    )
    error_message = "lp_settlement_max_replacements must be a whole number from 0 through 3."
  }
}

variable "lp_settlement_max_tx_cost_wei" {
  type        = string
  default     = "0"
  description = "Maximum total native-token cost accepted for one LP settlement transaction. Zero is allowed only in off or observe mode."

  validation {
    condition     = can(regex("^(0|[1-9][0-9]*)$", var.lp_settlement_max_tx_cost_wei))
    error_message = "lp_settlement_max_tx_cost_wei must be a canonical non-negative whole-number string."
  }
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

variable "liquidation_worker_execution_batch_size" {
  type        = string
  default     = "20"
  description = "Number of candidate accounts submitted per executeLiquidationBatch transaction. Must be between 1 and 256."

  validation {
    condition     = can(regex("^([1-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-6])$", var.liquidation_worker_execution_batch_size))
    error_message = "liquidation_worker_execution_batch_size must be an integer between 1 and 256."
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

variable "api_desired_count" {
  type        = number
  default     = 1
  description = "Desired API task count. Set to zero while restoring or validating a migrated database."

  validation {
    condition     = floor(var.api_desired_count) == var.api_desired_count && var.api_desired_count >= 0
    error_message = "api_desired_count must be a non-negative whole number."
  }
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

variable "db_storage_encrypted" {
  type        = bool
  default     = true
  description = "Encrypt RDS storage at rest. The company-account migration restores from an encrypted snapshot; existing unencrypted instances must be migrated through snapshot copy/restore rather than modified in place."
}

variable "db_kms_key_id" {
  type        = string
  default     = ""
  description = "Optional customer-managed AWS KMS key ARN for encrypted RDS storage. Leave empty to use the AWS-managed RDS key."

  validation {
    condition = (
      var.db_kms_key_id == ""
      || can(regex("^arn:aws[a-z-]*:kms:[a-z0-9-]+:[0-9]{12}:key/[0-9A-Fa-f-]{36}$", var.db_kms_key_id))
    )
    error_message = "db_kms_key_id must be empty or a customer-managed AWS KMS key ARN."
  }
}

variable "db_ssl_root_cert_path" {
  type        = string
  default     = "/etc/ssl/certs/aws-rds-global-bundle.pem"
  description = "Absolute in-container path to the checksum-pinned AWS RDS CA bundle used by libpq verify-full connections."

  validation {
    condition     = var.db_ssl_root_cert_path == "/etc/ssl/certs/aws-rds-global-bundle.pem"
    error_message = "db_ssl_root_cert_path must use the checksum-pinned AWS RDS CA bundle shipped in the backend image."
  }
}

variable "db_ca_cert_identifier" {
  type        = string
  default     = "rds-ca-rsa2048-g1"
  description = "Pinned RDS server CA family. The backend image ships the AWS global root bundle so planned CA rotations remain explicit."

  validation {
    condition = contains([
      "rds-ca-rsa2048-g1",
      "rds-ca-rsa4096-g1",
      "rds-ca-ecc384-g1",
    ], var.db_ca_cert_identifier)
    error_message = "db_ca_cert_identifier must be one of the supported RDS G1 CA identifiers."
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

variable "db_snapshot_identifier" {
  type        = string
  default     = null
  nullable    = true
  description = "Optional target-region manual snapshot used only to create this RDS lifecycle. Keep the value stable after restoration because changing it replaces the DB instance."

  validation {
    condition = var.db_snapshot_identifier == null || try(
      length(var.db_snapshot_identifier) <= 255
      && can(regex("^[a-z]([a-z0-9-]*[a-z0-9])?$", var.db_snapshot_identifier))
      && !strcontains(var.db_snapshot_identifier, "--"),
      false
    )
    error_message = "db_snapshot_identifier must start with a lowercase letter, contain only lowercase letters, digits, and single hyphens, not end in a hyphen, and be at most 255 characters."
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
