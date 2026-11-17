locals {
  rpc_log_range_policy = jsondecode(file("${path.module}/../../config/rpc-log-range-policy.json"))
}

# Validate stopped/bootstrap configurations too: their next start must not
# restore a scan size that the gateway rejects during backlog recovery.
resource "terraform_data" "rpc_log_range_policy_guard" {
  input = {
    policy_version = local.rpc_log_range_policy.version
    batch_blocks   = var.aa_reconciler_batch_blocks
  }

  lifecycle {
    precondition {
      condition = try(
        local.rpc_log_range_policy.version == 1
        && local.rpc_log_range_policy.maxLogRangeBlocks >= 1
        && floor(local.rpc_log_range_policy.maxLogRangeBlocks) == local.rpc_log_range_policy.maxLogRangeBlocks
        && local.rpc_log_range_policy.reconcilerDefaultBatchBlocks >= 1
        && local.rpc_log_range_policy.reconcilerDefaultBatchBlocks <= local.rpc_log_range_policy.maxLogRangeBlocks
        && floor(local.rpc_log_range_policy.reconcilerDefaultBatchBlocks) == local.rpc_log_range_policy.reconcilerDefaultBatchBlocks,
        false
      )
      error_message = "The shared RPC inclusive log-range policy must be version 1 with positive integer limits."
    }

    precondition {
      condition = try(
        can(regex("^[1-9][0-9]*$", var.aa_reconciler_batch_blocks))
        && tonumber(var.aa_reconciler_batch_blocks) <= local.rpc_log_range_policy.maxLogRangeBlocks,
        false
      )
      error_message = "aa_reconciler_batch_blocks must be a positive integer no larger than the shared RPC inclusive block-range limit."
    }
  }
}
