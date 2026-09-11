# Frankfurt image preparation evidence — 2026-09-10

Scope: publish candidate images to the already-created Frankfurt ECR repositories
and prepare a controlled activation plan. No services, admin tasks or on-chain
transactions were started. Core, frontend, DNS and Singapore are out of scope.

Source: application commit `fd472f471b4731d8bad316d87c6586ef9bc27ab9`, tree
`398d5688cee10fc3f2374eb265cf6e8a08570fce`. GitHub comparison confirmed current
master `2967aa5ea348c5314ae6adbce5abf4c5bb49c43a` has identical file contents.
Backend/Tests runs for both revisions succeeded. No deployment workflow was
dispatched; existing deployment workflows remain Singapore-only.

The user explicitly approved restarting the unresponsive local Docker Desktop
engine. Restart succeeded. Builds use archived committed inputs, isolated ECR
authentication, ARM64 output and digest readback. No `latest` tag is published.

## Prepared candidates

Registry: `932542905614.dkr.ecr.eu-central-1.amazonaws.com`.

| Repository | ECR digest | Status |
| --- | --- | --- |
| `plether-alto-sepolia-aa-temp` | `sha256:9db94fbd439a26f01b0ece3cc5f76b3791c1e1660b990d74892274267096f12a` | Published; ARM64/config/ordered-layer identity verified |
| `plether-otel-log-router-sepolia-aa-temp` | `sha256:f442ecc9aff9c3fc768ba76a200690c33c4cb0ffd49b9660b09ce7738ba213cc` | Published; ARM64/source label/config identity verified |
| `plether-api-sepolia-aa-temp` | `sha256:4629cc418d52f183b5fdd21a2d4409124eaf7ea4e72229bd983c1602f67bb3be` | Published; ARM64/source/recipe labels/config identity verified; offline smoke passed |

Alto tag: `v1.2.7-arm64-28cee87ea6b5`. Source is the pinned GHCR manifest
`sha256:28cee87ea6b58ba10a37273e58602b50321516c36a81d0c35d50526d1f06995d`.
Direct HTTPS retrieval and SHA-256 hashing confirmed its raw bytes. All 14 ordered
layer descriptors and the configuration match the ECR mirror, ignoring only
descriptor media types as in the existing deployment workflow. Configuration
digest: `sha256:4005e26e10f547def11cbf53a43170e962b37c79f0f0ee9ff68b1b1303a7225d`.
Docker's manifest inspection command failed on this OCI manifest, so verification
uses direct registry retrieval rather than weakening digest verification.

Log-router tag: `source-fd472f471b4731d8bad316d87c6586ef9bc27ab9`.
Configuration digest: `sha256:065b64d306be194139b93718c1c326f0bc1411338c2ab8c899d0bbb5d3e42191`.

API tag: `source-fd472f471b4731d8bad316d87c6586ef9bc27ab9`.
Configuration digest: `sha256:dbc6d0417a286e20fde4e8fd891f74a1f22c8f44c434b69f5c44aed41fdbbb71`.
The build compiled the backend library and all 11 executables. The offline smoke
check verified embedded Core v1.2.3/release commit and executable availability for
the API, AA reconciler, AA admin and provider preflight. It did not start the API,
connect to a database/RPC, sign, or qualify live sponsorship.

## Scan findings — activation is blocked pending review

ECR scan summaries were COMPLETE when read. These are scanner-reported package
findings, **not independently validated exploitability findings**:

| Image | Critical | High | Medium | Low | Undefined |
| --- | ---: | ---: | ---: | ---: | ---: |
| Alto | 1 | 5 | 10 | 2 | 0 |
| Log router | 0 | 48 | 78 | 24 | 4 |
| API/backend | 4 | 15 | 6 | 0 | 0 |

Alto's reported critical finding is `CVE-2024-5535` in OpenSSL `3.1.4-r5`.
Reported highs: OpenSSL `CVE-2025-9230`, `CVE-2024-6119`, `CVE-2024-4741`, and
musl `CVE-2025-26519`, `CVE-2026-40200` (`1.2.4_git20230717-r4`). The image remains
stopped. Do not silently substitute a new Alto version/digest: its pinned runtime
and simulation behavior need explicit review and requalification. Assess/rebuild
the log router and backend before activation as well. Image publication is not a security
approval, and no finding has been waived by this work.

## Backend build transport issue

Two builds using the committed Dockerfile stopped at APT checksum mismatches,
including `libperl5.36_5.36.0-7+deb12u3_arm64.deb`. Expected SHA-256:
`9f88187766759ddc21b4b2fe2956929c4883a6674884cedde261e3ccf9a32d5c`.
Received HTTP bytes varied across attempts. The same package fetched over HTTPS
matched the expected checksum on both host and inside an isolated container.

The Frankfurt-only build recipe changes APT transport to HTTPS and bootstraps
trusted roots from the Node base image, retaining package verification and all
committed application inputs. Recipe SHA-256:
`5534710ccd61653e6d21b48f56905ad1f2c97abea36beeaba834e9527d69ab9b`.
The builder checks this hash and records it as an API image label and in evidence.
Shared Dockerfiles/deployment workflows are unchanged.

## Final verification and next gate

All three candidate images were published and read back successfully. The complete
validated evidence record is retained locally at
`/var/folders/xz/scmmjjld2pq8k763z54mzjtr0000gn/T/plether-frankfurt-images-BAVqWd/images.json`.
Temporary Docker authentication was removed. All 15 focused preparation/image/
tunnel/secret tests passed, and `git diff --check` passed.

Post-publication AWS readback confirmed all 10 Frankfurt ECS services have desired,
running and pending counts of zero, with no running tasks. No activation, Core or
paymaster deployment, funding, frontend/DNS change or Singapore mutation occurred.

Review/remediate the scanner findings before activation, then follow the
[activation plan](frankfurt-aa-activation-plan.md). All activation still requires
separate approval; the dormant Terraform guard has not been relaxed. Prepared
candidate images are not security-qualified deployment artifacts yet.
