# Plether AWS infrastructure

The company AWS account is `932542905614`. Regional infrastructure and state
live in Asia Pacific (Singapore), `ap-southeast-1`.

## Authenticate

Use an MFA-protected console administrator and temporary AWS CLI credentials.
Do not create an IAM access key for a human operator.

```bash
aws login --profile plether-company --region ap-southeast-1
aws sts get-caller-identity --profile plether-company
```

The checked identity must belong to account `932542905614` and must not be the
root user. The pinned Terraform AWS provider does not consume the CLI's
`login_session` profile directly, so export the refreshed temporary credentials
into the current shell before running Terraform:

```bash
eval "$(aws configure export-credentials --profile plether-company --format env)"
```

## Bootstrap state

Run this once after authenticating:

```bash
PLETHER_AWS_PROFILE=plether-company ./bootstrap-state.sh
```

The script refuses any account other than `932542905614`, then creates and
secures the versioned Singapore state bucket.

## Initialize and validate

Terraform `1.16.x` is required.

```bash
terraform init -backend-config=backend.sepolia.hcl
terraform fmt -check -recursive
terraform validate
```

Keep the real Sepolia tfvars outside Git. Every plan must be saved and reviewed
before applying:

```bash
terraform plan -var-file=terraform.tfvars.sepolia -out=sepolia.tfplan
terraform show sepolia.tfplan
```

The provider's `allowed_account_ids` guard rejects credentials for the source
account or any account other than `932542905614`.

## LP settlement rollout

LP epoch settlement is restricted to Sepolia. Keep `lp_settlement_mode = "off"`
for the first deployment, provision a dedicated signer key and operations SNS
topic, fund the signer, and then apply `observe`. Active modes require exactly
one consolidated `plether-workers` task and
`lp_settlement_signer_funding_confirmed = true`; set that attestation only
after funding and a successful read-only preflight. Terraform rejects any
other topology.

Before changing from `observe` to `execute`, you must run the deployed keeper
image once with the same environment and secrets as the `plether-keeper`
container in `plether-sepolia-workers`, overriding its command to:

```bash
plether-keeper --lp-settlement-preflight
```

This preflight is read-only: it checks configuration, database/RPC access, and
contract bindings without acquiring the long-lived keeper lock or mutating the
database. Run it as a separate one-shot process; do not replace or stop the
live consolidated worker task. Require exit code zero and the
`lp_settlement_preflight_succeeded` log event. Then review observe-mode
heartbeats, ready backlog, signer balance, and simulated transaction costs;
configure a reviewed non-zero `lp_settlement_max_tx_cost_wei` before applying
`execute`. Do not use a different signer for the preflight, and do not bypass a
failing preflight by changing the task command permanently.
