#!/usr/bin/env python3
"""Bounded operator-side completion of the authorized Sepolia RPC bootstrap.

Uses only the RPC state and identifiers. Credential values are never returned.
Staging cleanup requires a successful full primary canary and a saved plan
whose only mutations detach/delete the named staging disk and clear its config.
"""
import argparse
import fcntl
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import subprocess
import time
from urllib.parse import urlsplit
from urllib.request import Request, urlopen

INSTANCE = 'i-04a6af37b4583a4ee'
STAGING = 'vol-0d2eb64dc29721085'
ACCOUNT = '932542905614'
EXPECTED_DELETE = {'aws_volume_attachment.rpc_staging[0]', 'aws_ebs_volume.rpc_staging[0]'}


class PlanRejected(ValueError):
    pass


def require(condition, code='guard_failed'):
    if not condition:
        raise PlanRejected(code)


def review_cleanup(plan):
    changed = [r for r in plan['resource_changes'] if r.get('mode') == 'managed' and r['change']['actions'] != ['no-op']]
    require({r['address'] for r in changed} == EXPECTED_DELETE | {'aws_ssm_parameter.rpc_config[0]'}, 'unexpected_cleanup_changes')
    for resource in changed:
        change = resource['change']
        if resource['address'] in EXPECTED_DELETE:
            require(change['actions'] == ['delete'])
            if resource['type'] == 'aws_ebs_volume':
                require(change['before']['id'] == STAGING, 'wrong_staging_volume')
                require(change['before']['size'] == 4096 and change['before']['encrypted'])
            else:
                require(change['before']['volume_id'] == STAGING and change['before']['instance_id'] == INSTANCE, 'wrong_staging_attachment')
                require(not change['before'].get('force_detach'))
                require(not change['before'].get('stop_instance_before_detaching'))
                require(not change['before'].get('skip_destroy'))
        else:
            require(change['actions'] == ['update'])
            require(change['before']['name'] == change['after']['name'] == '/plether/sepolia/rpc-node/config')
            require(change['before']['type'] == change['after']['type'] == 'String')
            before, after = json.loads(change['before']['value']), json.loads(change['after']['value'])
            require(before.pop('staging_volume_id') == STAGING)
            require(after.pop('staging_volume_id') == '' and before == after, 'unexpected_runtime_config_change')


def main():
    import boto3  # Operator dependency; not needed by the pure guard tests.
    parser = argparse.ArgumentParser()
    parser.add_argument('--directory', required=True, type=Path)
    parser.add_argument('--terraform', required=True, type=Path)
    parser.add_argument('--hours', type=float, default=6)
    parser.add_argument('--cleanup-staging', action='store_true')
    args = parser.parse_args()
    require(0 < args.hours <= 12, 'invalid_wait_bound')
    root = Path(__file__).resolve().parents[3]
    work = args.directory.resolve()
    work.mkdir(mode=0o700, exist_ok=True)
    os.umask(0o077)
    lock = (work / 'completion.lock').open('w')
    fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    state = {'instance_id': INSTANCE, 'staging_volume_id': STAGING, 'status': 'waiting_for_import'}
    def emit(status, **fields):
        state.update(status=status, updated_at_utc=datetime.now(timezone.utc).isoformat(), **fields)
        (work / 'completion-status.json').write_text(json.dumps(state, indent=2) + '\n')
        print(json.dumps({'status': status, **fields}), flush=True)
    session = boto3.Session(profile_name='plether', region_name='ap-southeast-1')
    require(session.client('sts').get_caller_identity()['Account'] == ACCOUNT, 'wrong_account')
    ssm, ecs, logs = session.client('ssm'), session.client('ecs'), session.client('logs')
    def command(commands, timeout=120):
        result = ssm.send_command(DocumentName='AWS-RunShellScript', InstanceIds=[INSTANCE], TimeoutSeconds=timeout,
            Comment='Bounded RPC bootstrap completion; no secret output',
            Parameters={'commands': commands, 'executionTimeout': [str(timeout)]})
        deadline = time.monotonic() + timeout + 60
        while time.monotonic() < deadline:
            try:
                invocation = ssm.get_command_invocation(CommandId=result['Command']['CommandId'], InstanceId=INSTANCE)
            except ssm.exceptions.InvocationDoesNotExist:
                time.sleep(2); continue
            if invocation['Status'] == 'Success':
                return invocation['StandardOutputContent']
            if invocation['Status'] in ['Failed', 'Cancelled', 'TimedOut']:
                raise RuntimeError('Remote completion step failed; diagnostic content suppressed')
            time.sleep(3)
        raise TimeoutError('Remote completion step timed out')
    seed = json.loads((work / 'rpc-only.tfvars.json').read_text())
    progress_commands = ["python3 - <<'REMOTE'\n" + Path(__file__).with_name('progress.py').read_text() + "\nREMOTE"]
    canary_source = Path(__file__).with_name('qualify.mjs').read_text()
    # Only the existing RPC origin is read, and it stays in process memory.
    reference_url = ssm.get_parameter(Name='/plether/sepolia/rpc-node/backup-url', WithDecryption=True)['Parameter']['Value']
    parsed = urlsplit(reference_url)
    require(parsed.scheme == 'https' and parsed.hostname == 'arb-sepolia.g.alchemy.com' and not parsed.username, 'unexpected_reference_origin')
    def reference_height():
        request = Request(reference_url, data=json.dumps({'jsonrpc': '2.0', 'id': 1, 'method': 'eth_blockNumber', 'params': []}).encode(), headers={'Content-Type': 'application/json', 'User-Agent': 'plether-rpc-qualification/1'})
        try:
            with urlopen(request, timeout=15) as response:
                return int(json.loads(response.read(65536))['result'], 16)
        except Exception:
            return None
    def primary_canary():
        service = ecs.describe_services(cluster='plether-sepolia', services=['plether-sepolia-rpc-gateway'])['services'][0]
        definition = ecs.describe_task_definition(taskDefinition=service['taskDefinition'])['taskDefinition']
        require(definition['executionRoleArn'] == 'arn:aws:iam::932542905614:role/plether-sepolia-rpc-gateway-execution' and not definition.get('taskRoleArn'), 'unexpected_canary_role')
        require(len(definition['containerDefinitions']) == 1, 'unexpected_canary_containers')
        container = definition['containerDefinitions'][0]
        require(container['name'] == 'rpc-gateway' and container['image'] == seed['sepolia_rpc']['gateway_image'], 'unexpected_canary_image')
        require({s['name'] for s in container.get('secrets', [])} <= {'RPC_GATEWAY_TOKEN', 'RPC_BACKUP_URL', 'RPC_BACKUP_TOKEN', 'RPC_REFERENCE_URL', 'RPC_REFERENCE_TOKEN'}, 'unexpected_canary_secrets')
        task = {'cluster': 'plether-sepolia', 'taskDefinition': service['taskDefinition'], 'launchType': 'FARGATE', 'platformVersion': '1.4.0', 'startedBy': 'rpc-primary-qualification',
            'networkConfiguration': {'awsvpcConfiguration': {'subnets': seed['existing_public_subnet_ids'], 'securityGroups': [seed['existing_ecs_security_group_id']], 'assignPublicIp': 'ENABLED'}},
            'overrides': {'containerOverrides': [{'name': 'rpc-gateway', 'command': ['node', '--input-type=module', '-e', canary_source]}]}}
        response = ecs.run_task(**task)
        require(not response.get('failures') and len(response['tasks']) == 1, 'canary_launch_failed')
        arn = response['tasks'][0]['taskArn']
        deadline = time.monotonic() + 600
        while time.monotonic() < deadline:
            current = ecs.describe_tasks(cluster='plether-sepolia', tasks=[arn])['tasks'][0]
            if current['lastStatus'] == 'STOPPED':
                success = all(c.get('exitCode') == 0 for c in current['containers'])
                stream = 'gateway/rpc-gateway/' + arn.rsplit('/', 1)[1]
                time.sleep(5)
                results = []
                for _ in range(12):
                    try:
                        events = logs.get_log_events(logGroupName='/plether/sepolia/rpc-gateway', logStreamName=stream, startFromHead=True)['events']
                    except logs.exceptions.ResourceNotFoundException:
                        events = []
                    results = []
                    for event in events:
                        try: data = json.loads(event['message'])
                        except ValueError: continue
                        if data.get('event') == 'rpc_qualification':
                            results.append({k: data[k] for k in ['event', 'test', 'pass', 'required', 'height', 'hash'] if k in data})
                    if any(r['test'] == 'required_checks_complete' for r in results): break
                    time.sleep(5)
                (work / 'primary-qualification.json').write_text(json.dumps({'task_arn': arn, 'success': success, 'results': results}, indent=2) + '\n')
                return success and any(r['test'] == 'required_checks_complete' and r['pass'] for r in results)
            time.sleep(10)
        ecs.stop_task(cluster='plether-sepolia', task=arn, reason='Bounded RPC qualification timed out')
        return False
    try:
        deadline = time.monotonic() + args.hours * 3600
        last_canary = 0
        while time.monotonic() < deadline:
            raw = json.loads(command(progress_commands))
            progress = {k: raw[k] for k in ['data_mounted', 'data_used_bytes', 'staging_mounted', 'staging_used_bytes', 'staging_file_bytes', 'import_marker', 'local_rpc_height', 'service_state'] if k in raw}
            height = reference_height()
            emit('waiting_for_import' if not progress.get('import_marker') else 'catching_up', progress=progress, reference_height=height)
            local_height = progress.get('local_rpc_height')
            if progress.get('import_marker') and local_height is not None and height is not None and abs(height - local_height) <= 20 and time.monotonic() - last_canary >= 180:
                emit('qualifying_primary')
                last_canary = time.monotonic()
                if primary_canary(): break
            time.sleep(120)
        else:
            emit('needs_attention', reason='Import did not finish within the bounded wait; no cleanup performed')
            return 1
        emit('primary_qualified')
        if args.cleanup_staging:
            config = json.loads((work / 'rpc-only.tfvars.json').read_text())
            require(config['sepolia_rpc']['staging_volume_gib'] == 4096, 'unexpected_staging_size')
            config['sepolia_rpc']['staging_volume_gib'] = 0
            cleanup_vars = work / 'cleanup.tfvars.json'
            cleanup_vars.write_text(json.dumps(config, indent=2) + '\n')
            env = {**os.environ, 'AWS_PROFILE': 'plether', 'TF_DATA_DIR': str(work / 'tf-data')}
            cwd = root / 'infra/terraform-rpc'
            plan = work / 'cleanup.tfplan'
            with (work / 'cleanup-plan.log').open('w') as output:
                subprocess.run([str(args.terraform), 'plan', '-input=false', '-no-color', '-var-file=' + str(cleanup_vars), '-out=' + str(plan)], cwd=cwd, env=env, stdout=output, stderr=subprocess.STDOUT, check=True)
            rendered = subprocess.run([str(args.terraform), 'show', '-json', str(plan)], cwd=cwd, env=env, capture_output=True, check=True)
            review_cleanup(json.loads(rendered.stdout))
            emit('cleanup_plan_verified')
            cleanup_command = """python3 - <<'REMOTE'
import json,os,subprocess
from pathlib import Path
def require(condition):
 if not condition:raise RuntimeError('Staging cleanup guard failed')
marker=json.loads(Path('/srv/nitro/.plether-snapshot-complete').read_text())
require(marker['chain_id']==421614 and marker['snapshot']=='https://snapshot.arbitrum.foundation/sepolia-rollup/2026-10-20-dbd26a00/')
require(subprocess.check_output(['systemctl','show','plether-nitro','--property=SubState','--value'],text=True).strip()=='running')
devices=json.loads(subprocess.check_output(['lsblk','--json','--nodeps','-o','NAME,SERIAL,TYPE'],text=True))['blockdevices']
matches=[d for d in devices if d.get('serial')=='vol0d2eb64dc29721085' and d['type']=='disk']
require(len(matches)==1)
device='/dev/'+matches[0]['name']
source=subprocess.check_output(['findmnt','-n','-o','SOURCE','--target','/srv/nitro-staging'],text=True).strip()
require(os.path.realpath(source)==os.path.realpath(device))
uuid=subprocess.check_output(['blkid','-s','UUID','-o','value',device],text=True).strip()
subprocess.run(['umount','/srv/nitro-staging'],check=True)
fstab=Path('/etc/fstab');old=fstab.read_text()
Path('/etc/fstab.plether-rpc-before-staging-cleanup').write_text(old)
temporary=Path('/etc/fstab.plether-rpc.tmp')
temporary.write_text(''.join(line for line in old.splitlines(keepends=True) if not(line.split() and line.split()[0]=='UUID='+uuid and len(line.split())>1 and line.split()[1]=='/srv/nitro-staging')))
os.chmod(temporary,0o644);temporary.replace(fstab)
print('staging_unmounted')
REMOTE"""
            require('staging_unmounted' in command([cleanup_command]), 'staging_not_unmounted')
            with (work / 'cleanup-apply.log').open('w') as output:
                subprocess.run([str(args.terraform), 'apply', '-input=false', '-no-color', str(plan)], cwd=cwd, env=env, stdout=output, stderr=subprocess.STDOUT, check=True)
            (work / 'rpc-only.tfvars.json').write_text(json.dumps(config, indent=2) + '\n')
            committed = cwd / 'sepolia.tfvars.json'
            current = json.loads(committed.read_text())
            previous = json.loads(json.dumps(config)); previous['sepolia_rpc']['staging_volume_gib'] = 4096
            if current == previous:
                committed.write_text(json.dumps(config, indent=2) + '\n')
            emit('staging_removed', staging_volume_id=None, source_update_pending_commit=True)
            command(['systemctl restart --no-block plether-nitro'])
            time.sleep(30)
            for _ in range(5):
                if primary_canary(): break
                time.sleep(60)
            else:
                emit('needs_attention', reason='Post-cleanup process restart qualification failed')
                return 1
        emit('qualified', traffic_cutover=False, soak_started_at_utc=datetime.now(timezone.utc).isoformat())
        return 0
    except Exception as error:
        emit('needs_attention', exception_type=type(error).__name__, guard_code=str(error) if isinstance(error, PlanRejected) else None, reason='Completion stopped safely; inspect bounded status and saved plans')
        return 1


if __name__ == '__main__':
    try:
        raise SystemExit(main())
    except Exception as error:
        print(json.dumps({'status': 'needs_attention', 'exception_type': type(error).__name__, 'reason': 'Operator preflight failed; no secret details returned'}), flush=True)
        raise SystemExit(1)
