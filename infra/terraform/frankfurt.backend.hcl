# This dedicated bucket must be bootstrapped separately with encryption,
# versioning, public access blocked, and narrowly scoped access. Not created here.
bucket               = "plether-sepolia-aa-temp-tfstate-932542905614"
key                  = "plether/sepolia-aa-temp/terraform.tfstate"
region               = "eu-central-1"
encrypt              = true
use_lockfile         = true
allowed_account_ids  = ["932542905614"]
