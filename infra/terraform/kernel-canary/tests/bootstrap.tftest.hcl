# Synthetic plans only. No AWS credentials, remote state, image writes or keys.
mock_provider "aws" {
  mock_data "aws_caller_identity" {
    defaults = { account_id = "932542905614" }
  }
  mock_data "aws_iam_openid_connect_provider" {
    defaults = { arn = "arn:aws:iam::932542905614:oidc-provider/token.actions.githubusercontent.com" }
  }
  mock_resource "aws_ecr_repository" {
    override_during = plan
    defaults = {
      arn = "arn:aws:ecr:ap-southeast-1:932542905614:repository/synthetic"
    }
  }
  mock_resource "aws_iam_role" {
    override_during = plan
    defaults        = { arn = "arn:aws:iam::932542905614:role/plether-sepolia-kernel-canary-image-build" }
  }
  mock_resource "aws_kms_key" {
    override_during = plan
    defaults = {
      arn    = "arn:aws:kms:ap-southeast-1:932542905614:key/11111111-1111-1111-1111-111111111111"
      key_id = "11111111-1111-1111-1111-111111111111"
    }
  }
}

run "nothing_is_created_by_default" {
  command = plan
  assert {
    condition = (length(aws_ecr_repository.api) == 0 && length(aws_ecr_repository.alto) == 0
      && length(aws_iam_role.image_build) == 0 && length(aws_kms_key.paymaster) == 0
      && length(aws_kms_key.kernel_canary_ssm) == 0
    )
    error_message = "The isolated canary root must be opt-in."
  }
}

run "ssm_key_is_symmetric_and_independent" {
  command = plan
  variables { provision_ssm_encryption_key = true }

  assert {
    condition = (length(aws_ecr_repository.api) == 0
      && length(aws_iam_role.image_build) == 0
      && length(aws_iam_role.kernel_canary_execution) == 0
      && length(aws_iam_role.kernel_canary_task) == 0
      && length(aws_kms_key.paymaster) == 0
      && aws_kms_key.kernel_canary_ssm[0].key_usage == "ENCRYPT_DECRYPT"
      && aws_kms_key.kernel_canary_ssm[0].customer_master_key_spec == "SYMMETRIC_DEFAULT"
      && aws_kms_key.kernel_canary_ssm[0].enable_key_rotation
      && !aws_kms_key.kernel_canary_ssm[0].bypass_policy_lockout_safety_check
      && aws_kms_alias.kernel_canary_ssm[0].name == "alias/plether-sepolia-kernel-canary-ssm"
    )
    error_message = "SSM encryption must use a separate rotating symmetric key without creating the paymaster signer or image resources."
  }
  assert {
    condition = (length(jsondecode(aws_kms_key.kernel_canary_ssm[0].policy).Statement) == 4
      && alltrue([
        for statement in jsondecode(aws_kms_key.kernel_canary_ssm[0].policy).Statement :
        statement.Principal.AWS == "arn:aws:iam::932542905614:root"
        && statement.Effect == "Allow"
        && alltrue([for action in statement.Action : length(regexall("\\*", action)) == 0])
      ])
      && toset(jsondecode(aws_kms_key.kernel_canary_ssm[0].policy).Statement[0].Action) == toset([
        "kms:CancelKeyDeletion", "kms:CreateAlias", "kms:DeleteAlias", "kms:DescribeKey",
        "kms:DisableKey", "kms:DisableKeyRotation", "kms:EnableKey", "kms:EnableKeyRotation",
        "kms:GetKeyPolicy", "kms:GetKeyRotationStatus", "kms:ListGrants", "kms:ListResourceTags",
        "kms:PutKeyPolicy", "kms:RevokeGrant", "kms:TagResource", "kms:UntagResource",
        "kms:UpdateAlias", "kms:UpdateKeyDescription",
      ])
    )
    error_message = "The account IAM delegation must preserve key management and recovery with an explicit non-cryptographic action list."
  }
  assert {
    condition = (
      toset(jsondecode(aws_kms_key.kernel_canary_ssm[0].policy).Statement[1].Action) == toset(["kms:Decrypt", "kms:Encrypt", "kms:GenerateDataKey"])
      && toset(jsondecode(aws_kms_key.kernel_canary_ssm[0].policy).Statement[1].Condition.ArnNotEquals["aws:PrincipalArn"]) == toset([
        for name in ["alto-execution", "api-execution", "reconciler-execution", "alto-task", "api-task", "reconciler-task", "canary-image-build"] :
        "arn:aws:iam::932542905614:role/plether-sepolia-kernel-${name}"
      ])
      && toset(jsondecode(aws_kms_key.kernel_canary_ssm[0].policy).Statement[1].Condition.StringEquals["kms:EncryptionContext:PARAMETER_ARN"]) == toset([
        for name in ["alto-executor-private-keys", "alto-utility-private-key", "aa-origin-token", "deployer-private-key"] :
        "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/kernel-canary-${name}"
      ])
      && alltrue([
        for statement in slice(jsondecode(aws_kms_key.kernel_canary_ssm[0].policy).Statement, 1, 4) :
        statement.Condition.StringEquals["kms:ViaService"] == "ssm.ap-southeast-1.amazonaws.com"
      ])
    )
    error_message = "IAM-authorized operators retain provisioning and recovery for the four exact SecureStrings only through regional SSM. Canary service and build roles cannot use this delegation."
  }
  assert {
    condition = alltrue([
      for i, role in ["alto", "api"] : (
        jsondecode(aws_kms_key.kernel_canary_ssm[0].policy).Statement[i + 2].Action == ["kms:Decrypt"]
        && jsondecode(aws_kms_key.kernel_canary_ssm[0].policy).Statement[i + 2].Condition.ArnEquals["aws:PrincipalArn"] == "arn:aws:iam::932542905614:role/plether-sepolia-kernel-${role}-execution"
        && toset(jsondecode(aws_kms_key.kernel_canary_ssm[0].policy).Statement[i + 2].Condition.StringEquals["kms:EncryptionContext:PARAMETER_ARN"]) == toset([
          for name in(role == "alto" ? toset(["alto-executor-private-keys", "alto-utility-private-key"]) : toset(["aa-origin-token"])) :
          "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/kernel-canary-${name}"
        ])
      )
    ])
    error_message = "Each workload grant is read-only and limited to the exact future execution role and its own SecureStrings."
  }
}

run "reject_noncanary_ssm_key_context_at_bootstrap" {
  command = plan
  variables {
    provision_ssm_encryption_key                        = true
    kernel_canary_alto_executor_keys_ssm_parameter_name = "/plether/sepolia/alto-executor-private-keys"
    kernel_canary_alto_utility_key_ssm_parameter_name   = "/plether/sepolia/kernel-canary-deployer-private-key"
    kernel_canary_origin_token_ssm_parameter_name       = "/plether/sepolia/aa-proxy-origin-token"
  }
  expect_failures = [
    var.kernel_canary_alto_executor_keys_ssm_parameter_name,
    var.kernel_canary_alto_utility_key_ssm_parameter_name,
    var.kernel_canary_origin_token_ssm_parameter_name,
  ]
}

run "build_role_only_pushes_to_immutable_canary_repositories" {
  command = plan
  variables { provision_image_build_resources = true }

  assert {
    condition = (aws_ecr_repository.api[0].name == "plether-kernel-api-sepolia"
      && aws_ecr_repository.alto[0].name == "plether-kernel-alto-sepolia"
      && aws_ecr_repository.api[0].image_tag_mutability == "IMMUTABLE"
      && aws_ecr_repository.alto[0].image_tag_mutability == "IMMUTABLE"
      && !aws_ecr_repository.api[0].force_delete
      && !aws_ecr_repository.alto[0].force_delete
      && length(aws_kms_key.paymaster) == 0
    )
    error_message = "Image bootstrap must own only the two retained immutable canary repositories and build role."
  }
  assert {
    condition = (jsondecode(aws_iam_role.image_build[0].assume_role_policy).Statement[0].Condition.StringEquals["token.actions.githubusercontent.com:aud"] == "sts.amazonaws.com"
      && toset(jsondecode(aws_iam_role.image_build[0].assume_role_policy).Statement[0].Condition.StringEquals["token.actions.githubusercontent.com:sub"]) == toset([
        "repo:Plether-Fi/plether-app:environment:backend-admin-sepolia",
        "repo:Plether-Fi/plether-app:environment:alto-admin-sepolia",
      ])
    )
    error_message = "The build role must trust only the two reviewed protected Sepolia environments."
  }
  assert {
    condition = (jsondecode(aws_iam_role_policy.image_build[0].policy).Statement[0].Action == "ecr:GetAuthorizationToken"
      && jsondecode(aws_iam_role_policy.image_build[0].policy).Statement[0].Resource == "*"
      && toset(jsondecode(aws_iam_role_policy.image_build[0].policy).Statement[1].Resource) == toset([
        aws_ecr_repository.api[0].arn, aws_ecr_repository.alto[0].arn,
      ])
      && contains(jsondecode(aws_iam_role_policy.image_build[0].policy).Statement[1].Action, "ecr:BatchGetImage")
      && contains(jsondecode(aws_iam_role_policy.image_build[0].policy).Statement[1].Action, "ecr:GetDownloadUrlForLayer")
      && !contains(jsondecode(aws_iam_role_policy.image_build[0].policy).Statement[1].Action, "ecr:DeleteRepository")
      && !contains(jsondecode(aws_iam_role_policy.image_build[0].policy).Statement[1].Action, "ecs:UpdateService")
    )
    error_message = "The build role must have no live service or repository deletion permission."
  }
}

run "paymaster_key_is_separate_and_can_sign_only_from_exact_api_role" {
  command = plan
  variables { provision_paymaster_key = true }

  assert {
    condition = (length(aws_ecr_repository.api) == 0
      && length(aws_iam_role.image_build) == 0
      && aws_kms_key.paymaster[0].key_usage == "SIGN_VERIFY"
      && aws_kms_key.paymaster[0].customer_master_key_spec == "ECC_SECG_P256K1"
      && aws_kms_alias.paymaster[0].name == "alias/plether-sepolia-kernel-canary-paymaster-signer"
    )
    error_message = "Key bootstrap must create only the separate canary secp256k1 signer and alias."
  }
  assert {
    condition = (jsondecode(aws_kms_key.paymaster[0].policy).Statement[2].Action == "kms:Sign"
      && jsondecode(aws_kms_key.paymaster[0].policy).Statement[2].Condition.ArnEquals["aws:PrincipalArn"] == "arn:aws:iam::932542905614:role/plether-sepolia-kernel-api-task"
      && jsondecode(aws_kms_key.paymaster[0].policy).Statement[2].Condition.StringEquals["kms:SigningAlgorithm"] == "ECDSA_SHA_256"
      && contains(jsondecode(aws_kms_key.paymaster[0].policy).Statement[0].Action, "kms:GetKeyRotationStatus")
      && !contains(jsondecode(aws_kms_key.paymaster[0].policy).Statement[0].Action, "kms:Sign")
    )
    error_message = "Only the exact future canary API task role may sign ECDSA_SHA_256 digests."
  }
}
