// Fixed identifiers only: paths and query strings may contain account or claim IDs.
export const API_ENDPOINTS = [
  'get_protocol_status', 'get_protocol_config',
  'claim_testnet_faucet', 'claim_margin_funding', 'get_margin_funding',
  'get_perps_basket_history', 'get_perps_basket_latest',
  'get_perps_basket_candles', 'get_perps_basket_current_candle',
  'get_perps_market_stats', 'get_perps_vault_history',
  'get_perps_vault_activity', 'get_perps_vault_request_ids', 'get_perps_reveal_payload',
  'get_user_dashboard', 'get_user_balances', 'get_user_positions', 'get_user_allowances',
  'get_mint_quote', 'get_burn_quote', 'get_zap_quote', 'get_trade_quote', 'get_leverage_quote',
  'get_transaction_history', 'get_leverage_history', 'get_lending_history',
] as const;

export type ApiEndpoint = typeof API_ENDPOINTS[number];

export const API_RESPONSE_FORMATS = ['json', 'html', 'text', 'other', 'none'] as const;
export type ApiResponseFormat = typeof API_RESPONSE_FORMATS[number];

export function validCloudflareRayId(value: unknown): value is string {
  return typeof value === 'string' && /^[0-9a-f]{16}(?:-[a-z]{3})?$/i.test(value);
}
