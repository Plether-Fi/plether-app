#!/usr/bin/env python3
"""Mount verified disks, import a pinned snapshot, then run a bounded PathDB node.

Only ``initialize`` may format a blank, positively identified data disk. Routine
service restarts refuse an absent mount or an interrupted/unknown database.
"""
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import socket
import subprocess
import sys
import time
from urllib.parse import urlparse
from urllib.request import urlopen

DATA = Path('/srv/nitro')
STAGING = Path('/srv/nitro-staging')
RUNTIME = Path('/run/plether-rpc')
MARKER = '.plether-snapshot-complete'


def command(*args, capture=False):
    return subprocess.run(args, check=True, text=True,
                          stdout=subprocess.PIPE if capture else None).stdout


def parameter(name, region):
    raw = command('aws', 'ssm', 'get-parameter', '--region', region,
                  '--name', name, '--with-decryption', '--output', 'json', capture=True)
    return json.loads(json.loads(raw)['Parameter']['Value'])


def validate_config(config, parent):
    if not re.fullmatch(r'offchainlabs/nitro-node@sha256:[0-9a-f]{64}', config['image']):
        raise ValueError('Nitro must be pinned to an official image digest')
    if not re.fullmatch(r'https://snapshot\.arbitrum\.foundation/sepolia-rollup/[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9a-f]+/', config['snapshot_url']):
        raise ValueError('snapshot must be a pinned official Sepolia directory')
    if not re.fullmatch(r'[0-9a-f]{64}', config['snapshot_metadata_sha256']):
        raise ValueError('snapshot metadata hash is required')
    history = config['state_history_blocks']
    if isinstance(history, bool) or not isinstance(history, int) or not 16384 <= history <= 691200:
        raise ValueError('recent history must be explicitly bounded')
    keys = ['execution_url', 'beacon_url']
    if parent.get('secondary_beacon_url'):
        keys.append('secondary_beacon_url')
    for key in keys:
        url = urlparse(parent[key])
        if url.scheme != 'https' or not url.hostname or url.username or url.password or url.fragment:
            raise ValueError('parent endpoints must be HTTPS without URL userinfo')
    if parent.get('secondary_beacon_url') == parent['beacon_url']:
        raise ValueError('configure distinct primary and backup beacon endpoints')


def choose_nvme(devices):
    candidates = [d for d in devices if d.get('type') == 'disk'
                  and (d.get('model') or '').strip() == 'Amazon EC2 NVMe Instance Storage'
                  and 3_000_000_000_000 <= int(d.get('size', 0)) <= 4_000_000_000_000]
    if len(candidates) != 1:
        raise ValueError('expected exactly one 3.75 TB EC2 instance-store disk')
    return '/dev/' + candidates[0]['name']


def mount_disk(device, target, label):
    if os.path.ismount(target):
        actual = command('findmnt', '-n', '-o', 'SOURCE', '--target', str(target), capture=True).strip()
        if os.path.realpath(actual) != os.path.realpath(device):
            raise ValueError('unexpected existing mount')
        return
    filesystem = command('lsblk', '-dn', '-o', 'FSTYPE', device, capture=True).strip()
    if not filesystem:
        # Refuse disks with partition tables/signatures, even when FSTYPE is empty.
        signatures = command('wipefs', '--no-act', '--noheadings', '--output', 'TYPE', device, capture=True).strip()
        children = json.loads(command('lsblk', '--json', device, capture=True))['blockdevices'][0].get('children', [])
        if signatures or children:
            raise ValueError('refusing to format a disk with existing signatures')
        command('mkfs.xfs', '-L', label, device)
    elif filesystem != 'xfs' or command('blkid', '-s', 'LABEL', '-o', 'value', device, capture=True).strip() != label:
        raise ValueError('refusing an unrecognized data filesystem')
    target.mkdir(parents=True, exist_ok=True)
    uuid = command('blkid', '-s', 'UUID', '-o', 'value', device, capture=True).strip()
    entry = f'UUID={uuid} {target} xfs defaults,nofail,x-systemd.device-timeout=30 0 2\n'
    with open('/etc/fstab', 'a+') as fstab:
        fstab.seek(0)
        if entry not in fstab.read():
            fstab.write(entry)
    command('mount', str(target))
    os.chown(target, 1000, 1000)


def initialize(config):
    devices = json.loads(command('lsblk', '--bytes', '--json', '--nodeps', '-o', 'NAME,MODEL,SIZE,TYPE,SERIAL', capture=True))['blockdevices']
    mount_disk(choose_nvme(devices), DATA, 'plether-nitro')
    volume = config['staging_volume_id'].replace('-', '')
    if not volume:
        raise ValueError('initial import requires a staging EBS volume')
    for _ in range(120):
        devices = json.loads(command('lsblk', '--bytes', '--json', '--nodeps', '-o', 'NAME,SERIAL,TYPE', capture=True))['blockdevices']
        matches = [d for d in devices if d.get('serial') == volume and d.get('type') == 'disk']
        if len(matches) == 1:
            mount_disk('/dev/' + matches[0]['name'], STAGING, 'plether-stage')
            return
        time.sleep(5)
    raise ValueError('staging volume attachment did not appear')


def nitro_config(config, parent):
    return {
        'chain': {'id': 421614, 'name': 'sepolia-rollup'},
        'parent-chain': {
            'id': 11155111,
            'connection': {'url': parent['execution_url']},
            'blob-client': {
                'beacon-url': parent['beacon_url'],
                **({'secondary-beacon-url': parent['secondary_beacon_url']} if parent.get('secondary_beacon_url') else {}),
            },
        },
        # archive enables the historical RPC index, while this finite window
        # still prunes state online. Never use the unbounded archive default.
        'execution': {'caching': {'state-scheme': 'path', 'archive': True,
                                  'state-history': config['state_history_blocks']}},
        'persistent': {'chain': '/home/user/.arbitrum'},
        'http': {'addr': '0.0.0.0', 'port': 8547, 'api': ['net', 'web3', 'eth', 'debug'], 'vhosts': ['*']},
        'metrics': True,
        'metrics-server': {'addr': '127.0.0.1', 'port': 6070},
        'log-type': 'json',
    }


def docker(config, region, bootstrap=False):
    args = ['docker', 'run', '--rm', '--name', 'plether-nitro', '--network', 'host',
            '--read-only', '--cap-drop', 'ALL', '--security-opt', 'no-new-privileges',
            '--tmpfs', '/tmp:rw,nosuid,size=1g', '--env', 'GOMEMLIMIT=96GiB',
            '--log-driver', 'awslogs', '--log-opt', f'awslogs-region={region}',
            '--log-opt', f'awslogs-group={config["log_group"]}',
            '--log-opt', f'awslogs-stream={socket.gethostname()}',
            '--mount', f'type=bind,src={DATA},dst=/home/user/.arbitrum',
            '--mount', f'type=bind,src={RUNTIME}/nitro.json,dst=/node-config.json,readonly']
    if bootstrap:
        args += ['--mount', f'type=bind,src={STAGING},dst=/downloads']
    args += [config['image'], '--conf.file=/node-config.json']
    if bootstrap:
        args += ['--init.url=' + config['snapshot_url'], '--init.download-path=/downloads',
                 '--init.validate-checksum=true', '--init.then-quit=true']
    return args


def prepare(config, parent, region):
    if not os.path.ismount(DATA) or os.stat(DATA).st_dev == os.stat('/').st_dev:
        raise ValueError('data mount absent; refusing boot-volume initialization')
    actual = command('findmnt', '-n', '-o', 'SOURCE', '--target', str(DATA), capture=True).strip()
    devices = json.loads(command('lsblk', '--bytes', '--json', '--nodeps', '-o', 'NAME,MODEL,SIZE,TYPE', capture=True))['blockdevices']
    if os.path.realpath(actual) != os.path.realpath(choose_nvme(devices)):
        raise ValueError('data mount is not the approved instance-store disk')
    RUNTIME.mkdir(mode=0o755, parents=True, exist_ok=True)
    path = RUNTIME / 'nitro.json'
    # Create with private permissions before writing secret-bearing configuration.
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o400)
    with os.fdopen(fd, 'w') as output:
        json.dump(nitro_config(config, parent), output)
    os.chown(path, 1000, 1000)
    if (DATA / MARKER).exists():
        if not any(DATA.glob('**/CURRENT')):
            raise ValueError('bootstrap marker exists but Pebble database is absent')
        return
    if any(DATA.iterdir()):
        raise ValueError('interrupted or unknown database requires operator inspection before reimport')
    if not os.path.ismount(STAGING):
        raise ValueError('separate staging mount is required for snapshot import')
    with urlopen(config['snapshot_url'] + 'metadata.json', timeout=30) as response:
        if response.url != config['snapshot_url'] + 'metadata.json':
            raise ValueError('snapshot metadata redirect is not allowed')
        raw = response.read(1024 * 1024 + 1)
    if hashlib.sha256(raw).hexdigest() != config['snapshot_metadata_sha256']:
        raise ValueError('snapshot metadata checksum mismatch')
    metadata = json.loads(raw)
    if (metadata['state_scheme'], metadata['snapshot_kind'], metadata['chain_name']) != ('path', 'full-path', 'sepolia-rollup'):
        raise ValueError('snapshot database or chain mismatch')
    size = metadata['size']['total_bytes']
    if shutil.disk_usage(DATA).free < size * 1.3 or shutil.disk_usage(STAGING).free < size * 2.1:
        raise ValueError('insufficient extraction headroom or download/concatenation staging capacity')
    command(*docker(config, region, bootstrap=True))
    if not any(DATA.glob('**/CURRENT')):
        raise ValueError('snapshot import did not produce a Pebble database')
    (DATA / MARKER).write_text(json.dumps({'chain_id': 421614, 'scheme': 'path', 'snapshot': config['snapshot_url']}))


def main():
    mode, name, region = sys.argv[1:]
    config = parameter(name, region)
    parent = parameter(config['parent_parameter'], region)
    validate_config(config, parent)
    if mode == 'initialize':
        initialize(config)
    elif mode == 'prepare':
        prepare(config, parent, region)
    elif mode == 'run':
        os.execvp('docker', docker(config, region))
    else:
        raise ValueError('unknown command')


if __name__ == '__main__':
    try:
        main()
    except Exception:
        # Endpoint URLs and SSM values must never be included in cloud-init logs.
        print('Nitro setup failed; inspect configuration, mounts and service status.', file=sys.stderr)
        sys.exit(1)
