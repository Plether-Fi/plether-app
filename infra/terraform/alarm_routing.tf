locals {
  # Preserve existing Singapore Sepolia notification behavior while enabling
  # OK recovery emails on mainnet so operators know when incidents clear.
  operations_alarm_recovery_actions = var.environment == "mainnet" ? compact([var.operations_alarm_sns_topic_arn]) : []
}
