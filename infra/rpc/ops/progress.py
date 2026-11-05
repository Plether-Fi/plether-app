"""Read-only, credential-free progress probe executed on the RPC host."""
import json
import os
from pathlib import Path
import shutil
import subprocess
from urllib.request import Request, urlopen

result = {}
for label, path in [('data', '/srv/nitro'), ('staging', '/srv/nitro-staging')]:
    result[label + '_mounted'] = os.path.ismount(path)
    if result[label + '_mounted']:
        usage = shutil.disk_usage(path)
        result[label + '_used_bytes'] = usage.used
        result[label + '_free_bytes'] = usage.free
result['staging_file_bytes'] = 0
for path in Path('/srv/nitro-staging').rglob('*'):
    try:
        if path.is_file(): result['staging_file_bytes'] += path.stat().st_size
    except FileNotFoundError:
        pass  # Import may remove an archive between enumeration and stat.
result['import_marker'] = Path('/srv/nitro/.plether-snapshot-complete').exists()
service = subprocess.run(['systemctl', 'show', 'plether-nitro', '--property=ActiveState,SubState'], capture_output=True, text=True)
result['service_state'] = service.stdout.strip().splitlines()
try:
    request = Request('http://127.0.0.1:8547', data=json.dumps({'jsonrpc': '2.0', 'id': 1, 'method': 'eth_blockNumber', 'params': []}).encode(), headers={'Content-Type': 'application/json'})
    with urlopen(request, timeout=3) as response:
        result['local_rpc_height'] = int(json.load(response)['result'], 16)
except Exception:
    result['local_rpc_height'] = None
print(json.dumps(result))
