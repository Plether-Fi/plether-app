-- Read-only release evidence, run with psql -v router=0x... -v chain_id=421614.
-- Wait at least 120 seconds after the cohort closes; retain unknown/pending rows.
BEGIN READ ONLY;
SET LOCAL statement_timeout = '5s';
WITH cohort AS (
  SELECT k.status, k.failure_reason, k.commit_time,
         o.terminal_timestamp,
         CASE WHEN e.data->>'isClose' IN ('true','false')
           THEN (e.data->>'isClose')::boolean END AS is_close
  FROM perps_keeper_orders k
  LEFT JOIN perps_orders o ON o.order_router=k.order_router AND o.order_id=k.order_id
    AND o.chain_id=:'chain_id'::bigint
  LEFT JOIN perps_events e ON e.release_router=k.order_router AND e.order_id=k.order_id
    AND e.chain_id=:'chain_id'::bigint AND e.event_name='IntentRegistered'
  WHERE k.order_router=lower(:'router')
    AND k.commit_time >= extract(epoch FROM current_timestamp)::bigint-86520
    AND k.commit_time < extract(epoch FROM current_timestamp)::bigint-120
)
SELECT count(*) AS committed_orders,
       count(*) FILTER (WHERE is_close) AS committed_closes,
       count(*) FILTER (WHERE is_close AND status='executed') AS successful_closes,
       count(*) FILTER (WHERE status='failed' AND failure_reason=2) AS expirations,
       count(*) FILTER (WHERE is_close AND status='failed' AND failure_reason=2) AS close_expirations,
       count(*) FILTER (WHERE status NOT IN ('executed','failed')) AS pending_orders,
       count(*) FILTER (WHERE is_close IS NULL) AS unclassified_orders,
       count(*) FILTER (WHERE status IN ('executed','failed') AND terminal_timestamp IS NULL) AS missing_terminal_times,
       percentile_cont(0.99) WITHIN GROUP (ORDER BY terminal_timestamp-commit_time)
         FILTER (WHERE terminal_timestamp>=commit_time) AS inclusion_to_terminal_p99_seconds,
       percentile_cont(0.99) WITHIN GROUP (ORDER BY terminal_timestamp-commit_time)
         FILTER (WHERE is_close AND status='executed' AND terminal_timestamp>=commit_time)
         AS successful_close_p99_seconds
FROM cohort;
-- Rejected submissions are not part of the commitment denominator. Report them
-- separately; browser observations may be missing and are never canonical proof.
SELECT e.source,e.stage,count(DISTINCT e.attempt_id) AS attempts
FROM aa_attempt_events e JOIN aa_attempt_diagnostics d USING(attempt_id)
WHERE d.chain_id=:'chain_id'::bigint AND d.deployment=lower(:'router')
  AND e.observed_at>=current_timestamp-interval '24 hours'
  AND e.stage IN ('wallet_requested','wallet_approved','deadline_elapsed',
    'submission_requested','bundler_forwarded','bundler_acknowledged','safe_expiry_verified')
GROUP BY e.source,e.stage ORDER BY e.source,e.stage;
COMMIT;
