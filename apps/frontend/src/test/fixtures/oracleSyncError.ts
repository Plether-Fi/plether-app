import { ContractFunctionExecutionError, ContractFunctionRevertedError, type Hex } from 'viem'
import { PERPS_PLETHER_ORACLE_ABI } from '../../contracts/abis'

// Arbitrum Sepolia block 311266517: publishTime=1790007647, lastMarkTime=1790007649.
export const ORACLE_SYNC_REVERT_DATA = '0x8e3e110d000000000000000000000000000000000000000000000000000000006ab1595f000000000000000000000000000000000000000000000000000000006ab15961'

export function createOracleSyncError(legacyAbi = false, data: Hex = ORACLE_SYNC_REVERT_DATA) {
  const abi = legacyAbi ? PERPS_PLETHER_ORACLE_ABI.filter(item => item.type !== 'error') : PERPS_PLETHER_ORACLE_ABI
  const cause = new ContractFunctionRevertedError({ abi, data, functionName: 'getLatestPrice' })
  return new ContractFunctionExecutionError(cause, {
    abi, args: [], functionName: 'getLatestPrice',
    contractAddress: '0x9f4d9ae736b94249b18a85a7e14092bfca0688eb',
  })
}
