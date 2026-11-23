resource "aws_db_subnet_group" "main" {
  name       = "plether-${local.deployment_name}"
  subnet_ids = aws_subnet.public[*].id
}

resource "aws_db_parameter_group" "postgres" {
  name_prefix = "plether-${local.deployment_name}-pg16-"
  family      = "postgres16"
  description = "Hardened PostgreSQL 16 parameters for Plether ${local.deployment_name}."

  parameter {
    name  = "rds.force_ssl"
    value = "1"
  }

  parameter {
    name  = "log_min_duration_statement"
    value = "1000"
  }

  lifecycle {
    create_before_destroy = true
  }
}

resource "aws_db_instance" "postgres" {
  identifier     = "plether-${local.deployment_name}"
  engine         = var.db_snapshot_identifier == null ? "postgres" : null
  engine_version = var.db_snapshot_identifier == null ? var.db_engine_version : null
  instance_class = var.db_instance_class

  snapshot_identifier = var.db_snapshot_identifier

  allocated_storage     = var.db_allocated_storage
  max_allocated_storage = var.db_max_allocated_storage
  storage_type          = var.db_storage_type
  storage_encrypted     = var.db_storage_encrypted
  kms_key_id            = var.db_storage_encrypted && var.db_kms_key_id != "" ? var.db_kms_key_id : null
  ca_cert_identifier    = var.db_ca_cert_identifier
  apply_immediately     = var.db_apply_immediately

  db_name                     = var.db_snapshot_identifier == null ? "plether" : null
  username                    = var.db_snapshot_identifier == null ? var.db_username : null
  manage_master_user_password = var.db_snapshot_identifier == null && var.db_manage_master_user_password ? true : null
  password                    = var.db_snapshot_identifier == null && !var.db_manage_master_user_password ? var.db_password : null

  db_subnet_group_name       = aws_db_subnet_group.main.name
  parameter_group_name       = aws_db_parameter_group.postgres.name
  vpc_security_group_ids     = [aws_security_group.rds.id]
  publicly_accessible        = false
  deletion_protection        = var.db_deletion_protection
  skip_final_snapshot        = var.db_skip_final_snapshot
  final_snapshot_identifier  = var.db_skip_final_snapshot ? null : var.db_final_snapshot_identifier
  copy_tags_to_snapshot      = true
  auto_minor_version_upgrade = var.environment != "mainnet"

  backup_retention_period = var.db_backup_retention_days
  backup_window           = "18:00-19:00"
  maintenance_window      = "sun:19:30-sun:20:30"

  performance_insights_enabled          = true
  performance_insights_retention_period = 7

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
        var.db_storage_encrypted
        && var.db_backup_retention_days >= 7
        && var.db_deletion_protection
        && !var.db_skip_final_snapshot
      )
      error_message = "Mainnet requires encrypted RDS storage, at least seven days of backups, deletion protection, and a final snapshot on deletion."
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

resource "aws_db_event_subscription" "postgres" {
  count = trimspace(var.operations_alarm_sns_topic_arn) != "" ? 1 : 0

  name             = "plether-${local.deployment_name}-rds-events"
  sns_topic        = var.operations_alarm_sns_topic_arn
  source_type      = "db-instance"
  source_ids       = [aws_db_instance.postgres.identifier]
  event_categories = ["availability", "deletion", "failover", "failure", "low storage", "maintenance", "recovery"]
}
