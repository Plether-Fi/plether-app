locals {
  # Preserve existing Singapore notification behavior.
  # Recovery messages describe the alarm state, not a blanket service all-clear.
  operations_alarm_recovery_actions = []
}
