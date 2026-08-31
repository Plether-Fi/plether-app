import assert from 'node:assert/strict';
import test from 'node:test';

import worker, {
  redirectToSepolia,
} from '../mainnet-redirect/_worker.js';

test('redirects the mainnet root to the Sepolia app permanently', async () => {
  const response = await worker.fetch(
    new Request('https://app.plether.com/')
  );

  assert.equal(response.status, 308);
  assert.equal(response.headers.get('location'), 'https://app.sepolia.plether.com/');
  assert.equal(response.headers.get('cache-control'), 'public, max-age=3600');
});

test('preserves path and query without allowing a protocol-relative redirect', () => {
  const response = redirectToSepolia(
    new Request('https://app.plether.com//attacker.example/trade?market=ETH')
  );

  assert.equal(response.status, 308);
  assert.equal(
    response.headers.get('location'),
    'https://app.sepolia.plether.com//attacker.example/trade?market=ETH'
  );
});

test('uses a method-preserving redirect for API requests', () => {
  const response = redirectToSepolia(
    new Request('https://app.plether.com/api/perps/v1/order', {
      method: 'POST',
      body: '{}',
      headers: { 'Content-Type': 'application/json' },
    })
  );

  assert.equal(response.status, 308);
  assert.equal(
    response.headers.get('location'),
    'https://app.sepolia.plether.com/api/perps/v1/order'
  );
});
