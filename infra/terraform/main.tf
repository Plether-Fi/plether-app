terraform {
  required_version = "~> 1.16.0"

  backend "s3" {}

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region              = var.aws_region
  allowed_account_ids = [var.expected_aws_account_id]

  default_tags {
    tags = {
      Project     = "plether"
      Environment = var.environment
      ManagedBy   = "terraform"
    }
  }
}
