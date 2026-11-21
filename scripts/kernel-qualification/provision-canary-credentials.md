# Sepolia Kernel canary credentials

Use [`provision-canary-credentials.mjs`](./provision-canary-credentials.mjs) after the isolated Terraform root has created a **customer-managed symmetric** KMS encryption key for SSM secrets. This script does not create that key. It uses only the named `plether` AWS profile and requires an explicit region and KMS key ARN in the same account. The default invocation is read-only and does not generate keys.

```sh
aws --profile plether sts get-caller-identity
node scripts/kernel-qualification/provision-canary-credentials.mjs \
  --region ap-southeast-1 \
  --kms-key-arn "$KERNEL_CANARY_SSM_KMS_KEY_ARN"
```

The plan confirms that these three names are absent:

| Parameter | ECS binding | Format |
| --- | --- | --- |
| `/plether/sepolia/kernel-canary-alto-executor-private-keys` | `ALTO_EXECUTOR_PRIVATE_KEYS` | one fresh secp256k1 private key, `0x` + 64 hex |
| `/plether/sepolia/kernel-canary-alto-utility-private-key` | `ALTO_UTILITY_PRIVATE_KEY` | a different fresh secp256k1 private key |
| `/plether/sepolia/kernel-canary-aa-origin-token` | `AA_PROXY_ORIGIN_TOKEN` | 64 lowercase hex characters, no `0x` |

Once the KMS key and empty names are reviewed, provision with the explicit gate:

```sh
node scripts/kernel-qualification/provision-canary-credentials.mjs \
  --region ap-southeast-1 \
  --kms-key-arn "$KERNEL_CANARY_SSM_KMS_KEY_ARN" \
  --apply --confirm kernel-sepolia-canary-secrets
```

The script generates secrets in process memory only after all reads pass. It sends each value to `aws ssm put-parameter --value file:///dev/stdin --no-overwrite`; no private key or origin token appears in a command argument, Terraform state, repository file, or console output. AWS CLI output and errors are filtered to public metadata. The result prints only the executor and utility **public addresses**, parameter names, account, region, and KMS ARN. Keep this JSON as public canary evidence. Set `kernel_canary_alto_secrets_kms_key_arn` to the same symmetric key ARN so the isolated Alto task execution role can decrypt its two keys. The canary API execution role also needs `kms:Decrypt` for the origin token when a customer-managed key is used; confirm the isolated Terraform root grants it before starting the API.

`put-parameter` is not atomic across three names. If any call fails, stop: the current call may have succeeded despite the error. The script does not retry or delete anything. Inspect all three SSM names and reconcile the state before another run. An existing name always causes the plan and apply to fail closed, even if two other names are absent.

## Sponsor KMS address

The paymaster signer is a **separate** KMS key with `ECC_SECG_P256K1` and `SIGN_VERIFY`; do not use the symmetric SSM key. Once the standalone root creates it, derive the public address without sending a signature request:

```sh
node scripts/kernel-qualification/provision-canary-credentials.mjs \
  --region ap-southeast-1 \
  --sponsor-key-arn "$KERNEL_CANARY_PAYMASTER_KMS_KEY_ARN"
```

The helper verifies AWS account, region, key ARN, state, curve, usage, and the exact canonical secp256k1 SPKI encoding accepted by `Plether.AA.Kms`. Its `sponsorSigner` output goes into the public `deploy-sepolia.mjs` config and must match the canary sponsorship profile. It never calls `kms:Sign`.

For the direct-factory deployment, use the reviewed managed HTTPS RPC bindings. Alto reads `/plether/sepolia/perps-rpc-url`. The API and reconciler also use `/plether/sepolia/aa-reconciler-secondary-rpc-url` to check the independent chain view. Keep both credential-bearing URLs in their approved secret bindings.

## GitHub and Pages token handoff

The active Pages workflow reads the GitHub Actions repository secret `KERNEL_CANARY_AA_PROXY_ORIGIN_TOKEN` and binds it to the isolated `plether-kernel-canary` project. After provisioning the SSM origin token, copy it from SSM to that GitHub secret without echoing it or placing it in a process argument:

```sh
(
  set +x
  set -e
  set -o pipefail
  token="$(aws --profile plether --region ap-southeast-1 ssm get-parameter \
    --name /plether/sepolia/kernel-canary-aa-origin-token \
    --with-decryption --query Parameter.Value --output text)"
  [[ "$token" =~ ^[0-9a-f]{64}$ ]]
  printf '%s' "$token" | gh secret set KERNEL_CANARY_AA_PROXY_ORIGIN_TOKEN \
    --repo Plether-Fi/plether-app
  unset token
)
```

Run this only after `gh auth status` and `gh api user --jq .login` succeed. The shell variable stays in the subshell; `printf` writes only to the `gh` input pipe. Do not use `gh secret set --body "$token"`, shell tracing, or a plaintext `.env`/temporary file. The workflow's `active_canary` phase performs the Pages binding after checking that the exact backend route accepts the credential; `stage_preview` does not bind it. See [`pages-canary.md`](./pages-canary.md).
