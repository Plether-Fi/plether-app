resource "aws_db_subnet_group" "main" {
  name       = "plether-${var.environment}"
  subnet_ids = aws_subnet.public[*].id
}

resource "aws_db_instance" "postgres" {
  identifier     = "plether-${var.environment}"
  engine         = var.db_snapshot_identifier == null ? "postgres" : null
  engine_version = var.db_snapshot_identifier == null ? "16" : null
  instance_class = var.db_instance_class

  snapshot_identifier = var.db_snapshot_identifier

  allocated_storage     = var.db_allocated_storage
  max_allocated_storage = var.db_max_allocated_storage
  storage_type          = var.db_storage_type
  storage_encrypted     = var.db_storage_encrypted
  kms_key_id            = var.db_storage_encrypted && var.db_kms_key_id != "" ? var.db_kms_key_id : null
  ca_cert_identifier    = var.db_ca_cert_identifier
  apply_immediately     = var.db_apply_immediately

  db_name  = var.db_snapshot_identifier == null ? "plether" : null
  username = var.db_snapshot_identifier == null ? var.db_username : null
  password = var.db_snapshot_identifier == null ? var.db_password : null

  db_subnet_group_name      = aws_db_subnet_group.main.name
  vpc_security_group_ids    = [aws_security_group.rds.id]
  publicly_accessible       = false
  deletion_protection       = var.db_deletion_protection
  skip_final_snapshot       = var.db_skip_final_snapshot
  final_snapshot_identifier = var.db_skip_final_snapshot ? null : var.db_final_snapshot_identifier

  backup_retention_period = var.db_backup_retention_days

  lifecycle {
    # Storage encryption cannot be enabled in place. Keep Terraform from
    # replacing the database while an operator prepares an encrypted snapshot
    # copy/restore and an explicitly reviewed endpoint/state cutover.
    prevent_destroy = true

    precondition {
      condition     = var.db_storage_encrypted || var.db_kms_key_id == ""
      error_message = "db_kms_key_id may be set only when db_storage_encrypted=true."
    }

    precondition {
      condition     = var.db_max_allocated_storage >= ceil(var.db_allocated_storage * 110 / 100)
      error_message = "db_max_allocated_storage must be at least 10 percent greater than db_allocated_storage for RDS storage autoscaling."
    }

    precondition {
      condition = var.db_skip_final_snapshot || try(
        length(trimspace(var.db_final_snapshot_identifier)) > 0,
        false
      )
      error_message = "db_final_snapshot_identifier must be set to a region-unique value when db_skip_final_snapshot is false. Use a new value before each recreated DB lifecycle."
    }

    precondition {
      condition = var.environment != "mainnet" || (
        var.db_backup_retention_days >= 7
        && var.db_deletion_protection
        && !var.db_skip_final_snapshot
      )
      error_message = "Mainnet requires at least seven days of RDS backups, deletion protection, and a final snapshot on deletion."
    }

    precondition {
      condition = !var.db_apply_immediately || (
        var.environment == "sepolia"
        && var.db_storage_type == "gp3"
        && var.db_backup_retention_days >= 7
        && var.db_deletion_protection
        && !var.db_skip_final_snapshot
        && try(length(trimspace(var.db_final_snapshot_identifier)) > 0, false)
      )
      error_message = "Immediate RDS modification is allowed only for a supervised Sepolia gp3 conversion with at least seven days of backups, deletion protection, and final snapshots enabled."
    }
  }
}
