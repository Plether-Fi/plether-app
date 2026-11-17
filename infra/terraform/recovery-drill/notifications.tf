resource "aws_sns_topic" "drill_notifications" {
  name = "${local.name}-evidence"
}
resource "aws_sqs_queue" "drill_notifications" {
  name                      = "${local.name}-evidence"
  message_retention_seconds = 345600
  sqs_managed_sse_enabled   = true
}
resource "aws_sqs_queue_policy" "drill_notifications" {
  queue_url = aws_sqs_queue.drill_notifications.id
  policy = jsonencode({ Version = "2012-10-17", Statement = [{
    Effect    = "Allow", Principal = { Service = "sns.amazonaws.com" }, Action = "sqs:SendMessage", Resource = aws_sqs_queue.drill_notifications.arn
    Condition = { ArnEquals = { "aws:SourceArn" = aws_sns_topic.drill_notifications.arn }, StringEquals = { "aws:SourceAccount" = local.account_id } }
  }] })
}
resource "aws_sns_topic_subscription" "drill_notifications" {
  topic_arn            = aws_sns_topic.drill_notifications.arn
  protocol             = "sqs"
  endpoint             = aws_sqs_queue.drill_notifications.arn
  raw_message_delivery = true
  depends_on           = [aws_sqs_queue_policy.drill_notifications]
}
