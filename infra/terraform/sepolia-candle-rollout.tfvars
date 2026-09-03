# Non-secret desired state for the supervised Sepolia candle rollout.
# Apply this after the environment's secret/base tfvars file so these safety
# controls cannot be omitted from an operator command.
environment = "sepolia"

consolidate_workers        = true
workers_desired_count      = 1
basket_worker_poll_seconds = "5"
keeper_poll_seconds        = "1"
keeper_idle_poll_seconds   = "5"

perps_candle_write_mode                 = "dual"
perps_candle_read_mode                  = "rollup"
perps_candle_read_intervals             = "60,180,300,900,1800,3600,86400"
perps_candle_shadow_sample_bps          = 0
perps_candle_strict_coverage            = true
perps_candle_lateness_seconds           = 120
perps_candle_finalization_grace_seconds = 15
