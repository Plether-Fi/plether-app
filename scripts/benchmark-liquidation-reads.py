#!/usr/bin/env python3
"""Read-only RPC benchmark for a captured candidate set; never signs or sends transactions."""
import argparse
import json
import shutil
import subprocess
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--snapshot', required=True, help='JSON with addresses and positions[{account}]')
    parser.add_argument('--rpc-url', default='https://arbitrum-sepolia-rpc.publicnode.com')
    parser.add_argument('--batch-size', type=int, default=100)
    args = parser.parse_args()
    if not 1 <= args.batch_size <= 100:
        parser.error('batch-size must be between 1 and 100')
    cast = shutil.which('cast')
    if not cast:
        parser.error('Foundry cast is required')
    snapshot = json.load(open(args.snapshot))
    lens = snapshot['addresses']['cfdEngineAccountLens']
    accounts = list(dict.fromkeys(p['account'] for p in snapshot['positions']))

    def rpc(method, params):
        output = subprocess.check_output(['curl', '--silent', '--show-error', '--fail', '--max-time', '30',
            args.rpc_url, '-H', 'Content-Type: application/json', '--data-binary',
            json.dumps(dict(jsonrpc='2.0', id=1, method=method, params=params))])
        result = json.loads(output)
        if 'error' in result:
            raise RuntimeError(result['error'])
        return result['result']

    def encode(*values):
        return subprocess.check_output([cast, *values], text=True).strip()

    block = rpc('eth_blockNumber', [])
    selector = encode('sig', 'getAccountLedgerSnapshot(address)')
    durations = []
    started = time.monotonic()
    checked = 0
    for offset in range(0, len(accounts), args.batch_size):
        chunk = accounts[offset:offset + args.batch_size]
        tuples = '[' + ','.join('(' + lens + ',true,' + selector + a[2:].rjust(64, '0') + ')' for a in chunk) + ']'
        calldata = encode('calldata', 'aggregate3((address,bool,bytes)[])', tuples)
        before = time.monotonic()
        result = bytes.fromhex(rpc('eth_call', [dict(to='0xca11bde05977b3631167028862be2a173976ca11', data=calldata), block])[2:])
        durations.append(time.monotonic() - before)
        def word(i):
            return int.from_bytes(result[i:i+32], 'big')
        base = word(0)
        if word(base) != len(chunk):
            raise RuntimeError('Unexpected multicall result count')
        head = base + 32
        for i in range(len(chunk)):
            item = head + word(head + i*32)
            if word(item) != 1:
                raise RuntimeError('Account-lens subcall failed: ' + chunk[i])
        checked += len(chunk)
        if checked % 1000 == 0:
            print(json.dumps(dict(checked=checked, seconds=round(time.monotonic()-started, 2))), flush=True)
    elapsed = time.monotonic() - started
    print(json.dumps(dict(block=int(block, 16), accounts=checked, requests=len(durations),
        seconds=round(elapsed, 2), projected_8000_read_seconds=round(elapsed*8000/max(checked, 1), 2),
        max_rpc_seconds=round(max(durations, default=0), 2),
        scope='account-lens reads only; excludes database, risk inputs, gas simulations and transactions'), indent=2))


if __name__ == '__main__':
    main()
