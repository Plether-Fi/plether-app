# Shared RPC resources also support a standalone state that reads this context.
locals {
  rpc_vpc_id                   = aws_vpc.main.id
  rpc_public_subnets           = aws_subnet.public
  rpc_ecs_cluster_id           = aws_ecs_cluster.main.id
  rpc_ecs_security_group_id    = aws_security_group.ecs.id
  rpc_github_oidc_provider_arn = aws_iam_openid_connect_provider.github_actions.arn
}
