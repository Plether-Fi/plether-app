variable "aws_region" {
  type    = string
  default = "ap-southeast-1"
  validation {
    condition     = var.aws_region == "ap-southeast-1"
    error_message = "This isolated drill is restricted to Singapore."
  }
}
variable "vpc_id" {
  type    = string
  default = "vpc-0df986b0047f901d3"
}
variable "subnet_ids" {
  type    = set(string)
  default = ["subnet-0633bc3db0148548f", "subnet-0eef958040b7769a1"]
}
variable "fixture_desired_count" {
  type    = number
  default = 0
  validation {
    condition     = contains([0, 1], var.fixture_desired_count)
    error_message = "The nonfinancial fixture may run at most one task."
  }
}
variable "operations_alarm_sns_topic_arn" {
  type    = string
  default = "arn:aws:sns:ap-southeast-1:932542905614:plether-sepolia-operations"
  validation {
    condition     = var.operations_alarm_sns_topic_arn == "arn:aws:sns:ap-southeast-1:932542905614:plether-sepolia-operations"
    error_message = "The drill must notify the existing reviewed operations destination."
  }
}
variable "observer_package_path" {
  type        = string
  description = "Absolute deterministic zip of the reviewed incident observer, produced by scripts/package-incident-observer.py."
}
