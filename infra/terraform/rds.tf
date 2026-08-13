resource "aws_db_subnet_group" "main" {
  name       = "plether-${var.environment}"
  subnet_ids = aws_subnet.public[*].id
}

resource "aws_db_instance" "postgres" {
  identifier     = "plether-${var.environment}"
  engine         = "postgres"
  engine_version = "16"
  instance_class = var.db_instance_class

  allocated_storage     = var.db_allocated_storage
  max_allocated_storage = var.db_max_allocated_storage
  storage_type          = var.db_storage_type
  storage_encrypted     = false

  db_name  = "plether"
  username = var.db_username
  password = var.db_password

  db_subnet_group_name      = aws_db_subnet_group.main.name
  vpc_security_group_ids    = [aws_security_group.rds.id]
  publicly_accessible       = false
  deletion_protection       = var.db_deletion_protection
  skip_final_snapshot       = var.db_skip_final_snapshot
  final_snapshot_identifier = var.db_skip_final_snapshot ? null : var.db_final_snapshot_identifier

  backup_retention_period = var.db_backup_retention_days

  lifecycle {
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
  }
}
