// Runs as a short-lived VPC task using the gateway's existing secret injection.
// Results contain only public block metadata and capability booleans.
const base = 'https://rpc-internal.sepolia.plether.com';
let failures = 0;
function record(test, pass, fields = {}, required = true) {
  console.log(JSON.stringify({event: 'rpc_qualification', test, pass, required, ...fields}));
  if (required && !pass) failures++;
}
async function rpc(url, token, method, params = []) {
  const response = await fetch(url, {
    method: 'POST', headers: {'Content-Type': 'application/json', ...(token ? {Authorization: `Bearer ${token}`} : {})},
    body: JSON.stringify({jsonrpc: '2.0', id: 1, method, params}),
    signal: AbortSignal.timeout(15000),
  });
  const body = await response.json();
  if (response.status !== 200 || body.id !== 1 || body.error || !Object.hasOwn(body, 'result')) throw new Error('RPC check failed');
  return body.result;
}
const primary = (method, params) => rpc(base + '/primary', process.env.RPC_GATEWAY_TOKEN, method, params);
const backup = (method, params) => rpc(process.env.RPC_BACKUP_URL, process.env.RPC_BACKUP_TOKEN, method, params);
const zero = '0x0000000000000000000000000000000000000000';
try {
  const [ownChain, referenceChain] = await Promise.all([primary('eth_chainId'), backup('eth_chainId')]);
  record('chain_ids', ownChain === '0x66eee' && referenceChain === ownChain);
  record('not_syncing', await primary('eth_syncing') === false);
  let safeHeight;
  for (const tag of ['latest', 'safe', 'finalized']) {
    const [ownHead, otherHead] = await Promise.all([primary('eth_getBlockByNumber', [tag, false]), backup('eth_getBlockByNumber', [tag, false])]);
    const height = Math.min(Number(BigInt(ownHead.number)), Number(BigInt(otherHead.number)));
    const hex = '0x' + height.toString(16);
    const [own, other] = await Promise.all([primary('eth_getBlockByNumber', [hex, false]), backup('eth_getBlockByNumber', [hex, false])]);
    record('canonical_' + tag, own?.hash === other?.hash && own?.number === hex && other?.number === hex, {height, hash: own?.hash});
    if (tag === 'safe') safeHeight = hex;
    if (tag === 'latest') {
      const ownTime = Number(BigInt(ownHead.timestamp));
      const otherTime = Number(BigInt(otherHead.timestamp));
      const now = Date.now() / 1000;
      record('fresh_head', now - ownTime <= 15 && ownTime - now <= 5 && Math.abs(ownTime - otherTime) <= 5);
    }
  }
  for (const [name, method, params] of [
    ['safe_balance', 'eth_getBalance', [zero, safeHeight]],
    ['safe_code', 'eth_getCode', [zero, safeHeight]],
    ['safe_storage', 'eth_getStorageAt', [zero, '0x0', safeHeight]],
    ['safe_call', 'eth_call', [{to: zero, data: '0x', gas: '0xf4240'}, safeHeight]],
  ]) {
    const [own, other] = await Promise.all([primary(method, params), backup(method, params)]);
    record(name, JSON.stringify(own) === JSON.stringify(other));
  }
  for (const [name, tracer] of [
    ['call_tracer', 'callTracer'],
    ['javascript_tracer', '{step:function(){},fault:function(){},result:function(ctx){return {gasUsed:ctx.gasUsed};}}'],
  ]) {
    try {
      const result = await primary('debug_traceCall', [{to: zero, data: '0x', gas: '0xf4240'}, 'latest', {tracer, timeout: '2s'}]);
      record(name, result !== null && typeof result === 'object', {}, false);
    } catch { record(name, false, {}, false); }
  }
} catch { record('transport_or_rpc', false); }
record('required_checks_complete', failures === 0);
process.exitCode = failures ? 1 : 0;
