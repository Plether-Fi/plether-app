# Dedicated OIDC role: publishing the gateway image and bounded node operations.
# It cannot apply Terraform, change application settings, stop EC2 or run arbitrary shell.
resource "aws_ssm_document" "rpc_admin" {
  count         = local.rpc_count
  name          = "plether-sepolia-rpc-node-admin"
  document_type = "Command"
  content = jsonencode({
    schemaVersion = "2.2"
    description   = "Inspect or restart the Sepolia Nitro process without stopping its EC2 host."
    parameters    = { operation = { type = "String", default = "status", allowedValues = ["status", "restart"] } }
    mainSteps = [{
      action = "aws:runShellScript", name = "nodeOperation", inputs = {
        timeoutSeconds = "60"
        runCommand = [
          "set -eu",
          "case '{{ operation }}' in",
          "  status) systemctl show plether-nitro --property=ActiveState,SubState; findmnt /srv/nitro; df -h /srv/nitro ;;",
          "  restart) test \"$(systemctl show plether-nitro --property=SubState --value)\" = running; systemctl restart --no-block plether-nitro ;;",
          "  *) exit 2 ;;",
          "esac",
        ]
      }
    }]
  })
}

resource "aws_iam_role" "rpc_admin" {
  count                = local.rpc_count
  name                 = "plether-sepolia-rpc-admin"
  max_session_duration = 3600
  assume_role_policy = jsonencode({
    Version = "2012-10-17", Statement = [{
      Effect    = "Allow", Action = "sts:AssumeRoleWithWebIdentity",
      Principal = { Federated = aws_iam_openid_connect_provider.github_actions.arn },
      Condition = { StringEquals = {
        "token.actions.githubusercontent.com:aud" = "sts.amazonaws.com",
        "token.actions.githubusercontent.com:sub" = "repo:Plether-Fi/plether-app:environment:rpc-admin-sepolia"
      } }
    }]
  })
}

resource "aws_iam_role_policy" "rpc_admin" {
  count = local.rpc_count
  role  = aws_iam_role.rpc_admin[0].id
  policy = jsonencode({
    Version = "2012-10-17", Statement = [
      { Effect = "Allow", Action = ["ssm:SendCommand"], Resource = [aws_ssm_document.rpc_admin[0].arn, aws_instance.rpc_node[0].arn] },
      { Effect = "Allow", Action = ["ssm:GetCommandInvocation"], Resource = "*" },
      { Effect = "Allow", Action = ["ecr:GetAuthorizationToken"], Resource = "*" },
      { Effect = "Allow", Action = ["ecr:BatchCheckLayerAvailability", "ecr:BatchGetImage", "ecr:CompleteLayerUpload", "ecr:GetDownloadUrlForLayer", "ecr:InitiateLayerUpload", "ecr:PutImage", "ecr:UploadLayerPart"], Resource = aws_ecr_repository.rpc_gateway[0].arn },
    ]
  })
}
