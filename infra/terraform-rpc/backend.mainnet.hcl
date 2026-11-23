# Replace <MAINNET_AWS_ACCOUNT_ID> with the dedicated mainnet AWS account ID
# bootstrapped via PLETHER_EXPECTED_ACCOUNT_ID=<MAINNET_AWS_ACCOUNT_ID> ./bootstrap-state.sh
bucket       = "plether-terraform-state-<MAINNET_AWS_ACCOUNT_ID>"
key          = "plether/mainnet/rpc/terraform.tfstate"
region       = "ap-southeast-1"
encrypt      = true
use_lockfile = true
