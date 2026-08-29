#!/usr/bin/env bash
set -euo pipefail

readonly PLETHER_EXPECTED_ACCOUNT_ID="932542905614"
readonly PLETHER_AWS_REGION="ap-southeast-1"
readonly PLETHER_STATE_BUCKET="plether-terraform-state-${PLETHER_EXPECTED_ACCOUNT_ID}"
readonly PLETHER_AWS_PROFILE="${PLETHER_AWS_PROFILE:-plether-company}"

actual_account_id=$(aws --profile "$PLETHER_AWS_PROFILE" sts get-caller-identity --query Account --output text)
if [ "$actual_account_id" != "$PLETHER_EXPECTED_ACCOUNT_ID" ]; then
  echo "Refusing to bootstrap state in AWS account ${actual_account_id}; expected ${PLETHER_EXPECTED_ACCOUNT_ID}." >&2
  exit 1
fi

actual_principal_arn=$(aws --profile "$PLETHER_AWS_PROFILE" sts get-caller-identity --query Arn --output text)
case "$actual_principal_arn" in
  arn:aws:iam::*:root)
    echo "Refusing to bootstrap state as the AWS account root user. Sign in with the named administrator instead." >&2
    exit 1
    ;;
esac

if ! aws --profile "$PLETHER_AWS_PROFILE" --region "$PLETHER_AWS_REGION" \
    s3api head-bucket --bucket "$PLETHER_STATE_BUCKET" 2>/dev/null; then
  aws --profile "$PLETHER_AWS_PROFILE" --region "$PLETHER_AWS_REGION" \
    s3api create-bucket \
    --bucket "$PLETHER_STATE_BUCKET" \
    --create-bucket-configuration "LocationConstraint=${PLETHER_AWS_REGION}"
fi

aws --profile "$PLETHER_AWS_PROFILE" --region "$PLETHER_AWS_REGION" \
  s3api put-public-access-block \
  --bucket "$PLETHER_STATE_BUCKET" \
  --public-access-block-configuration \
  BlockPublicAcls=true,IgnorePublicAcls=true,BlockPublicPolicy=true,RestrictPublicBuckets=true

aws --profile "$PLETHER_AWS_PROFILE" --region "$PLETHER_AWS_REGION" \
  s3api put-bucket-versioning \
  --bucket "$PLETHER_STATE_BUCKET" \
  --versioning-configuration Status=Enabled

aws --profile "$PLETHER_AWS_PROFILE" --region "$PLETHER_AWS_REGION" \
  s3api put-bucket-encryption \
  --bucket "$PLETHER_STATE_BUCKET" \
  --server-side-encryption-configuration \
  'Rules=[{ApplyServerSideEncryptionByDefault={SSEAlgorithm=AES256},BucketKeyEnabled=false}]'

aws --profile "$PLETHER_AWS_PROFILE" --region "$PLETHER_AWS_REGION" \
  s3api put-bucket-tagging \
  --bucket "$PLETHER_STATE_BUCKET" \
  --tagging 'TagSet=[{Key=Project,Value=plether},{Key=Environment,Value=shared},{Key=ManagedBy,Value=bootstrap}]'

policy_file=$(mktemp)
trap 'rm -f "$policy_file"' EXIT
sed "s/STATE_BUCKET/${PLETHER_STATE_BUCKET}/g" > "$policy_file" <<'JSON'
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Sid": "DenyInsecureTransport",
      "Effect": "Deny",
      "Principal": "*",
      "Action": "s3:*",
      "Resource": [
        "arn:aws:s3:::STATE_BUCKET",
        "arn:aws:s3:::STATE_BUCKET/*"
      ],
      "Condition": {
        "Bool": {
          "aws:SecureTransport": "false"
        }
      }
    }
  ]
}
JSON

aws --profile "$PLETHER_AWS_PROFILE" --region "$PLETHER_AWS_REGION" \
  s3api put-bucket-policy \
  --bucket "$PLETHER_STATE_BUCKET" \
  --policy "file://${policy_file}"

echo "Terraform state bucket is ready: ${PLETHER_STATE_BUCKET} (${PLETHER_AWS_REGION})"
