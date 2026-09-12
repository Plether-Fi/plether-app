# Temporary Alto image risk acceptance

The owner explicitly approved accepting CVE-2024-5535 for the exact existing
Alto image on Sepolia so activation can continue. This is acceptance of a known
risk, not remediation or a claim that the runtime is unaffected.

## Exact boundary

- Environment: `sepolia`, AWS account `932542905614`, region `ap-southeast-1`.
- Repository: `plether-alto-sepolia`.
- Upstream digest: `sha256:28cee87ea6b58ba10a37273e58602b50321516c36a81d0c35d50526d1f06995d`.
- Verified ECR mirror digest: `sha256:9db94fbd439a26f01b0ece3cc5f76b3791c1e1660b990d74892274267096f12a`.
- Finding: `CVE-2024-5535`, package `openssl`, version `3.1.4-r5`.
- Valid from September 12, 2026; expires **September 19, 2026 at 00:00 UTC**.

The workflow must obtain a completed scan for the selected account, repository
and image. Its detailed critical findings must agree with the summary count.
Only one exact matching finding is accepted. Another critical finding, a
different package/version, duplicate matching findings, incomplete/malformed
evidence, a different scope/image, or expiry blocks deployment. Clean completed
scans require no exception. The finding remains visible in ECR, workflow
warnings and the run summary; no scanner findings are deleted or suppressed.

## Rationale and limitations

ECR classified the observed finding as critical. The
[OpenSSL advisory](https://openssl-library.org/news/secadv/20240627.txt) rates
it low severity and describes application misuse involving a zero-length
protocol list. The pinned image runs Node v20.12.2 with OpenSSL 3.0.13+quic;
we have not established that the deployed execution paths are unaffected.
Other high/medium findings remain subject to the existing scanner policy.

Alto remains private-network-only with safe mode and normal validation enabled.
This exception grants no new funding, changes no KMS or sponsorship limits,
and does not itself change canonical verification or the safe-head ceiling.
The separately approved Sepolia finality-age policy is documented in the
[release procedure](singapore-sepolia-aa-release.md).
It does not qualify native sponsorship or permit mainnet deployment.

## Follow-up and revocation

Replace the aging runtime with a patched, pinned and qualified image. A new
digest does not inherit this acceptance. Renewal requires a new explicit owner
decision and reviewed change; there is no automatic extension. At expiry the
gate blocks new deployments with this finding; it does not automatically stop
an already running service.

Revoke by removing the exact acceptance branch from the deployment scan policy.
If stopping an active Alto service is necessary, first disable new issuance and
preserve signed operations, liabilities, recovery records and reconciliation.
Protected deployment approvals remain specific to each run and commit.
