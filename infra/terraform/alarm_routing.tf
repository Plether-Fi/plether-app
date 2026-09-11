locals {
  # Frankfurt canary only. Preserve legacy/Singapore notification behavior.
  # Recovery messages describe the alarm state, not a blanket service all-clear.
  operations_alarm_recovery_actions = local.frankfurt_preparation ? compact([var.operations_alarm_sns_topic_arn]) : []
}
