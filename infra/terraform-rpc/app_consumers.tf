variable "existing_aa_reconciler_security_group_id" {
  type        = string
  default     = null
  description = "Optional existing Sepolia reconciler security group allowed to reach only gateway HTTPS."
  validation {
    condition     = var.existing_aa_reconciler_security_group_id == null || can(regex("^sg-[0-9a-f]+$", var.existing_aa_reconciler_security_group_id))
    error_message = "Select an exact existing security group ID."
  }
}

variable "existing_alto_security_group_id" {
  type        = string
  default     = null
  description = "Optional existing Sepolia Alto task security group allowed to reach only gateway HTTPS."
  validation {
    condition     = var.existing_alto_security_group_id == null || can(regex("^sg-[0-9a-f]+$", var.existing_alto_security_group_id))
    error_message = "Select an exact existing Alto security group ID."
  }
}

data "aws_security_group" "existing_alto" {
  count = var.existing_alto_security_group_id == null ? 0 : 1
  id    = var.existing_alto_security_group_id
}

resource "aws_vpc_security_group_ingress_rule" "rpc_alb_from_alto" {
  count                        = var.existing_alto_security_group_id == null ? 0 : local.rpc_count
  security_group_id            = aws_security_group.rpc_alb[0].id
  referenced_security_group_id = data.aws_security_group.existing_alto[0].id
  ip_protocol                  = "tcp"
  from_port                    = 443
  to_port                      = 443
  description                  = "Authenticated RPC from the existing Sepolia Alto task only."

  lifecycle {
    precondition {
      condition = (
        data.aws_security_group.existing_alto[0].vpc_id == local.rpc_vpc_id &&
        startswith(data.aws_security_group.existing_alto[0].name, "plether-sepolia-alto-task-")
      )
      error_message = "The Alto group must belong to the existing Sepolia VPC and expected task family."
    }
  }
}

data "aws_security_group" "existing_aa_reconciler" {
  count = var.existing_aa_reconciler_security_group_id == null ? 0 : 1
  id    = var.existing_aa_reconciler_security_group_id
}

resource "aws_vpc_security_group_ingress_rule" "rpc_alb_from_aa_reconciler" {
  count                        = var.existing_aa_reconciler_security_group_id == null ? 0 : local.rpc_count
  security_group_id            = aws_security_group.rpc_alb[0].id
  referenced_security_group_id = data.aws_security_group.existing_aa_reconciler[0].id
  ip_protocol                  = "tcp"
  from_port                    = 443
  to_port                      = 443
  description                  = "Direct RPC verification from the existing Sepolia AA reconciler only."

  lifecycle {
    precondition {
      condition = (
        data.aws_security_group.existing_aa_reconciler[0].vpc_id == local.rpc_vpc_id &&
        startswith(data.aws_security_group.existing_aa_reconciler[0].name, "plether-sepolia-aa-reconciler-")
      )
      error_message = "The reconciler group must belong to the existing Sepolia VPC and expected task family."
    }
  }
}
