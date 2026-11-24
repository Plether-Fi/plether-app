# Dedicated mainnet AWS account (366562432386), bootstrapped via
# PLETHER_EXPECTED_ACCOUNT_ID=366562432386 PLETHER_AWS_PROFILE=plether-mainnet PLETHER_ENVIRONMENT=mainnet ./bootstrap-state.sh
bucket       = "plether-terraform-state-366562432386"
key          = "plether/mainnet/terraform.tfstate"
region       = "ap-southeast-1"
encrypt      = true
use_lockfile = true
