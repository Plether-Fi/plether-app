# Single-AZ RDS Snapshot & Point-in-Time Recovery (PITR) Runbook

Mainnet PostgreSQL runs on a **Single-AZ** RDS instance (`plether-mainnet`) with `storage_encrypted = true`, `backup_retention_period >= 7`, `deletion_protection = true`, `skip_final_snapshot = false`, `rds.force_ssl = 1`, and automated RDS Event Subscriptions routed to the operations SNS email topic.

Because Single-AZ is an accepted cost trade-off, operators must run a **non-destructive restore drill** before mainnet launch and after any major PostgreSQL upgrade.

---

## 1. Pre-Launch Non-Destructive Restore Drill

1. Verify caller identity against the dedicated mainnet AWS account (`<MAINNET_AWS_ACCOUNT_ID>`):

   ```bash
   aws --profile plether-mainnet sts get-caller-identity
   ```

2. Record the start time (`UTC`) and restore the latest automated backup (or a manual pre-migration snapshot) into an isolated temporary instance in the same VPC/subnet group/security group:

   ```bash
   aws --profile plether-mainnet --region ap-southeast-1 rds restore-db-instance-to-point-in-time \
     --source-db-instance-identifier plether-mainnet \
     --target-db-instance-identifier plether-mainnet-pitr-drill \
     --use-latest-restorable-time \
     --db-subnet-group-name plether-mainnet \
     --vpc-security-group-ids <RDS_SECURITY_GROUP_ID> \
     --no-publicly-accessible \
     --no-multi-az \
     --copy-tags-to-snapshot
   ```

3. Wait for the restored instance to become `available` and record elapsed **RTO**:

   ```bash
   aws --profile plether-mainnet --region ap-southeast-1 rds wait db-instance-available \
     --db-instance-identifier plether-mainnet-pitr-drill
   ```

4. Verify TLS (`sslmode=verify-full`), row counts for critical tables (`aa_sponsorship_authorizations`, `perps_orders`, `vault_epochs`, `position_protection_orders`), and schema integrity from a one-off read-only ECS task using the temporary endpoint.
5. Delete the temporary drill instance immediately after verification (skip final snapshot on the drill instance only):

   ```bash
   aws --profile plether-mainnet --region ap-southeast-1 rds delete-db-instance \
     --db-instance-identifier plether-mainnet-pitr-drill \
     --skip-final-snapshot
   ```

---

## 2. Emergency Production Recovery Procedure

1. **Freeze writers first:** Scale ECS services (`plether-api`, `plether-workers`, `plether-liquidation-worker`, `plether-aa-reconciler`, `plether-position-protection-worker`) to `desired_count = 0` so no split-brain transactions or journal entries occur during cutover.
2. **Restore to the exact clean timestamp** (`--restore-time` before bad migration/corruption, or `--use-latest-restorable-time` for host/AZ loss) into `plether-mainnet-recovered`.
3. **Verify encryption, parameter group (`rds.force_ssl = 1`), and schema/data integrity** on `plether-mainnet-recovered`.
4. **Update `/plether/mainnet/database-url`** SSM SecureString (keeping `sslmode=verify-full&sslrootcert=/etc/ssl/certs/aws-rds-global-bundle.pem`) to point to the recovered endpoint, or rename instances in a reviewed Terraform plan (`db_snapshot_identifier` / state import).
5. **Resume services in order:** `plether-aa-reconciler` → `plether-workers` → `plether-liquidation-worker` & `plether-position-protection-worker` → `plether-api`. Verify `/api/health`, `/api/aa/status`, indexer cursors, and email `OK` recovery notifications.
