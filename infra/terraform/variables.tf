variable "aws_region" {
  type    = string
  default = "us-east-1"
}

variable "environment" {
  type    = string
  default = "sepolia"
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

variable "perps_order_router" {
  type    = string
  default = "0x485703D16fE36369c134dEe2A61c057733E7830f"
}

variable "perps_plether_oracle" {
  type    = string
  default = "0x0e7c23b6Eb951DF97f7d2Fb2382B4405d88318bb"
}

variable "perps_cfd_engine" {
  type    = string
  default = "0x128f195B92b50db1eEBCbBd249d5C5e946DCd786"
}

variable "perps_margin_clearinghouse" {
  type    = string
  default = "0x00B89B6e696A43129DA7Ec8a814bb61C9A6189b8"
}

variable "perps_indexer_start_block" {
  type    = string
  default = "273137426"
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

variable "container_cpu" {
  type    = number
  default = 256
}

variable "container_memory" {
  type    = number
  default = 512
}

variable "workers_container_cpu" {
  type    = number
  default = 256
}

variable "workers_container_memory" {
  type    = number
  default = 512
}

variable "db_instance_class" {
  type    = string
  default = "db.t4g.micro"
}
