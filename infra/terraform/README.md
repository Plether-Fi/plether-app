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
