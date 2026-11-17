#!/usr/bin/env python3
"""Run production reconciliation against disposable loopback-only dependencies.

No live configuration, credentials, signing keys, transactions or AWS resources
are used. The PostgreSQL cluster, two independent HTTP fixture listeners, and
worker processes are created by this command and stopped in its finally block.
"""
import argparse
import hashlib
import http.server
import json
import os
import pathlib
import shutil
import signal
import socket
import subprocess
import threading
import time

ROOT = pathlib.Path(__file__).resolve().parents[1]
TIP = 36000
BARRIER = 875129401


def free_port():
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def word(value):
    return f"{value:064x}"


def block_hash(height):
    return "0x" + word(height)


class Fixture:
    def __init__(self, metadata):
        self.metadata = metadata
        self.base_epoch = int(time.time()) - TIP - 1
        self.phase = "before-interruption"
        self.outage = False
        self.calls = []
        self.lock = threading.Lock()
        self.servers = []
        self.events = [self.event(500, "a", "c", 7, 200), self.event(1500, "b", "d", 8, 300)]

    def event(self, height, operation, transaction, nonce, cost):
        m = self.metadata
        return {"address": m["entryPoint"], "topics": [m["eventTopic"], "0x" + operation * 64,
                "0x" + "0" * 24 + m["sender"][2:], "0x" + "0" * 24 + m["paymaster"][2:]],
                "data": "0x" + word(nonce) + word(1) + word(cost) + word(100),
                "transactionHash": "0x" + transaction * 64, "blockNumber": hex(height),
                "blockHash": block_hash(height), "logIndex": "0x0", "removed": False}

    def start(self, provider):
        fixture = self

        class Handler(http.server.BaseHTTPRequestHandler):
            def log_message(self, *_args):
                pass

            def do_POST(self):
                length = int(self.headers.get("Content-Length", "0"))
                assert 0 < length < 65536
                request = json.loads(self.rfile.read(length))
                method, params = request.get("method"), request.get("params", [])
                record = {"provider": provider, "method": method, "phase": fixture.phase, "at": time.time()}
                error = None
                if method == "eth_chainId":
                    result = hex(421614)
                elif method == "eth_call" and params[0]["data"].lower() == fixture.metadata["depositCall"].lower():
                    result = "0x" + "".join(word(v) for v in [10**18, 1, 10**17, 0, 0])
                elif method == "eth_getBlockByNumber":
                    height = TIP if params[0] in ["safe", "latest"] else int(params[0], 16)
                    assert 0 <= height <= TIP
                    result = {"number": hex(height), "hash": block_hash(height), "timestamp": hex(fixture.base_epoch + height)}
                elif method == "eth_getCode":
                    result = "0x" if int(params[1], 16) == 0 else "0x6000"
                elif method == "eth_getLogs":
                    query = params[0]
                    first, last = int(query["fromBlock"], 16), int(query["toBlock"], 16)
                    assert query["address"] == fixture.metadata["entryPoint"] and 1 <= first <= last <= TIP
                    record.update(first=first, last=last)
                    if fixture.outage and provider == "primary":
                        error = {"code": -32000, "message": "temporary fixture transport unavailable"}
                        record["outcome"] = "unavailable"
                    elif last - first + 1 > 200:
                        error = {"code": -32005, "message": "fixture block range exceeds 200 blocks"}
                        record["outcome"] = "range-limit"
                    else:
                        result = [e for e in fixture.events if first <= int(e["blockNumber"], 16) <= last]
                        record["outcome"] = "complete-range"
                else:
                    error = {"code": -32601, "message": "read-only fixture method refused"}
                    record["outcome"] = "unexpected-method"
                with fixture.lock:
                    fixture.calls.append(record)
                response = {"jsonrpc": "2.0", "id": request.get("id"), **({"error": error} if error else {"result": result})}
                body = json.dumps(response).encode()
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)

        server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        server.daemon_threads = True
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        self.servers.append(server)
        return f"http://127.0.0.1:{server.server_port}"

    def stop(self):
        for server in self.servers:
            server.shutdown()
            server.server_close()


def stop_process(process, hard=False):
    if process and process.poll() is None:
        process.send_signal(signal.SIGKILL if hard else signal.SIGTERM)
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait(timeout=5)


def main():
    if not __debug__:
        raise RuntimeError("Run this qualification with Python assertions enabled; -O/PYTHONOPTIMIZE is prohibited")
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence-dir", required=True, type=pathlib.Path)
    parser.add_argument("--postgres-bin", type=pathlib.Path)
    args = parser.parse_args()
    output = args.evidence_dir.resolve()
    output.mkdir(mode=0o700, parents=True, exist_ok=False)
    os.chmod(output, 0o700)
    pg = args.postgres_bin or (pathlib.Path(shutil.which("pg_ctl")).parent if shutil.which("pg_ctl") else pathlib.Path("/opt/homebrew/opt/postgresql@16/bin"))
    assert all((pg / tool).is_file() for tool in ["initdb", "pg_ctl", "createdb", "psql"]), "PostgreSQL tools required"
    env = {key: value for key, value in os.environ.items()
           if not key.startswith(("AWS_", "PG", "PERPS_", "AA_")) and key not in ["DATABASE_URL", "PRIVATE_KEY"]}
    port = free_port()
    db = f"postgresql://127.0.0.1:{port}/plether_recovery_drill"
    data = output / "postgres-data"
    harness = output / "reconciler-harness"
    processes = []
    fixture = None
    started_pg = False
    evidence = {"schema": 1, "isolation": {"database": "new disposable loopback PostgreSQL", "rpc": "two loopback read-only synthetic providers", "liveResourcesChanged": False, "keysUsed": False},
                "startedAt": time.time(), "sourceRevision": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
                "sourceFiles": {name: hashlib.sha256((ROOT / name).read_bytes()).hexdigest() for name in [
                    "apps/backend/src/Plether/AA/Reconciler.hs", "apps/backend/src/Plether/Database/AaSponsorship.hs",
                    "scripts/fixtures/ReconcilerRuntimeDrill.hs", "scripts/run-reconciler-recovery-drill.py"]}, "checks": {}}

    def run(command, **kwargs):
        result = subprocess.run(command, env=env, text=True, check=False, **kwargs)
        if result.returncode:
            diagnostic = {"command": command, "returncode": result.returncode,
                          "stdout": result.stdout, "stderr": result.stderr}
            (output / "failed-command.json").write_text(json.dumps(diagnostic, indent=2) + "\n")
            raise RuntimeError(f"isolated command failed ({result.returncode}); see {output / 'failed-command.json'}")
        return result

    def sql(statement):
        return run([str(pg / "psql"), "-X", "--no-psqlrc", "--tuples-only", "--no-align", db, "-c", statement], capture_output=True).stdout.strip()

    def snapshot():
        return json.loads(sql("""SELECT json_build_object(
          'cursor',(SELECT safe_block FROM aa_reconciler_cursor),
          'fresh',EXISTS(SELECT 1 FROM aa_reconciler_health h JOIN aa_reconciler_cursor c USING(chain_id,paymaster)
            WHERE h.safe_block=c.safe_block AND h.safe_block_hash=c.safe_block_hash
              AND h.last_success_at BETWEEN clock_timestamp()-interval '120 seconds' AND clock_timestamp()),
          'events',(SELECT count(*) FROM aa_user_operation_events),
          'actualCharge',(SELECT COALESCE(sum(amount_wei),0)::bigint FROM aa_sponsorship_ledger WHERE entry_type='actual_charge'),
          'release',(SELECT COALESCE(sum(amount_wei),0)::bigint FROM aa_sponsorship_ledger WHERE entry_type='release'),
          'reserve',(SELECT COALESCE(sum(amount_wei),0)::bigint FROM aa_sponsorship_ledger WHERE entry_type='reserve'),
          'ledgerRows',(SELECT count(*) FROM aa_sponsorship_ledger),
          'settled',(SELECT count(*) FROM aa_sponsorship_authorizations WHERE state='settled'),
          'paused',(SELECT issuance_paused FROM aa_sponsorship_control),
          'pauseReason',(SELECT paused_reason FROM aa_sponsorship_control),
          'resumes',(SELECT count(*) FROM aa_sponsorship_control_events WHERE action='resume'))"""))

    def worker(name, first, second):
        log = (output / (name + ".log")).open("w")
        process = subprocess.Popen([str(harness), "worker", db, first, second], env=env, stdout=log, stderr=subprocess.STDOUT, text=True)
        log.close()
        processes.append(process)
        return process

    def wait_until(label, predicate, timeout=60, process=None):
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if process and process.poll() is not None:
                raise AssertionError(f"worker exited while waiting for {label}: {process.returncode}")
            result = predicate()
            if result:
                return result
            time.sleep(0.15)
        raise AssertionError(f"timed out waiting for {label}")

    try:
        with (output / "build.log").open("w") as log:
            run(["cabal", "exec", "--", "ghc", "-threaded", "-package", "plether-api", "-odir", str(output), "-hidir", str(output), "-o", str(harness), str(ROOT / "scripts/fixtures/ReconcilerRuntimeDrill.hs")], cwd=ROOT / "apps/backend", stdout=log, stderr=subprocess.STDOUT)
        with (output / "postgres-init.log").open("w") as log:
            run([str(pg / "initdb"), "-D", str(data), "-A", "trust", "--no-locale", "--encoding=UTF8"], stdout=log, stderr=subprocess.STDOUT)
        run([str(pg / "pg_ctl"), "-D", str(data), "-l", str(output / "postgres.log"), "-o", f"-h 127.0.0.1 -p {port} -k {output} -c client_connection_check_interval=1000", "start", "-w"], capture_output=True)
        started_pg = True
        run([str(pg / "createdb"), "-h", "127.0.0.1", "-p", str(port), "plether_recovery_drill"], capture_output=True)
        setup = run([str(harness), "setup", db], cwd=ROOT / "apps/backend", capture_output=True)
        metadata = json.loads(setup.stdout.strip().splitlines()[-1])
        fixture = Fixture(metadata)
        first, second = fixture.start("primary"), fixture.start("secondary")
        initial = snapshot()
        assert initial["reserve"] == 2000 and initial["events"] == 0 and not initial["fresh"]
        sql(f"""CREATE FUNCTION drill_before_cursor() RETURNS TRIGGER LANGUAGE plpgsql AS $$ BEGIN
          IF NEW.safe_block=1000 THEN PERFORM pg_advisory_lock({BARRIER}); END IF; RETURN NEW; END $$;
          CREATE TRIGGER drill_cursor_barrier BEFORE UPDATE ON aa_reconciler_cursor
          FOR EACH ROW EXECUTE FUNCTION drill_before_cursor()""")
        with (output / "barrier.log").open("w") as log:
            barrier = subprocess.Popen([str(harness), "barrier", db], env=env, stdout=log, stderr=subprocess.STDOUT)
        processes.append(barrier)
        wait_until("isolated publication barrier", lambda: sql(f"SELECT count(*) FROM pg_locks WHERE locktype='advisory' AND objid={BARRIER} AND granted") == "1")
        before_crash = worker("worker-before-sigkill", first, second)
        wait_until("settlement committed before cursor publication", lambda: snapshot()["events"] == 1 and sql("SELECT count(*) FROM pg_stat_activity WHERE datname='plether_recovery_drill' AND wait_event='advisory'") == "1", process=before_crash)
        interrupted = snapshot()
        assert interrupted["cursor"] == 0 and interrupted["actualCharge"] == 200 and interrupted["release"] == 800 and not interrupted["fresh"]
        stop_process(before_crash, hard=True)
        stop_process(barrier)
        sql("SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='plether_recovery_drill' AND pid<>pg_backend_pid()")
        sql("DROP TRIGGER drill_cursor_barrier ON aa_reconciler_cursor; DROP FUNCTION drill_before_cursor()")
        assert snapshot() == interrupted
        evidence["checks"]["hardInterruptionAfterSettlementBeforePublication"] = {"passed": True, "snapshot": interrupted, "workerExit": before_crash.returncode}
        fixture.phase = "restart-transient-outage"
        fixture.outage = True
        restarted = worker("worker-after-restart", first, second)

        def threshold_pause():
            state = snapshot()
            assert state["cursor"] == 0 and not state["fresh"] and state["actualCharge"] == 200
            return state if state["paused"] else None

        paused = wait_until("durable failure-threshold pause", threshold_pause, process=restarted)
        assert paused["pauseReason"] == "dual-provider reconciliation unavailable beyond configured threshold"
        evidence["checks"]["transientOutageDoesNotAdvanceOrRefresh"] = {"passed": True, "snapshot": paused}
        fixture.phase = "restart-catch-up"
        fixture.outage = False
        progress = []

        def caught_up():
            state = snapshot()
            if not progress or state["cursor"] != progress[-1]["cursor"]:
                progress.append({"at": time.time(), **state})
            if state["cursor"] < TIP:
                assert not state["fresh"], "partial backlog incorrectly published healthy reconciliation"
            assert state["actualCharge"] <= 500 and state["events"] <= 2 and state["ledgerRows"] <= 6
            return state if state["cursor"] == TIP and state["fresh"] else None

        complete = wait_until("complete 10-hour synthetic backlog", caught_up, timeout=150, process=restarted)
        assert complete["events"] == complete["settled"] == 2 and complete["actualCharge"] == 500
        assert complete["reserve"] == 2000 and complete["release"] == 1500 and complete["ledgerRows"] == 6
        assert complete["paused"], "recovery must not automatically resume issuance"
        stop_process(restarted)
        sql("SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='plether_recovery_drill' AND pid<>pg_backend_pid()")
        for provider in ["primary", "secondary"]:
            ranges = [(c["first"], c["last"]) for c in fixture.calls if c["provider"] == provider and c.get("outcome") == "complete-range" and c["phase"] == "restart-catch-up"]
            covered = set()
            for lo, hi in ranges:
                assert hi-lo+1 <= 200
                covered.update(range(lo, hi+1))
            assert covered == set(range(1, TIP+1)), f"{provider}: skipped canonical history"
        assert any(c.get("outcome") == "range-limit" for c in fixture.calls)
        assert any(c.get("outcome") == "unavailable" for c in fixture.calls)
        assert not any(c.get("outcome") == "unexpected-method" for c in fixture.calls)
        evidence["checks"]["multiHourBacklogNoSkippedRangesOrDuplicateCharges"] = {"passed": True, "blocks": TIP, "historySeconds": TIP, "providerRangeCap": 200, "configuredBatch": 1000, "snapshot": complete, "progress": progress}
        resume = run([str(harness), "resume-checks", db], capture_output=True)
        resume_result = json.loads(resume.stdout.strip().splitlines()[-1])
        after_resume = snapshot()
        for field in ["events", "actualCharge", "reserve", "release", "ledgerRows", "settled"]:
            assert after_resume[field] == complete[field], f"resume altered economic state: {field}"
        assert after_resume["resumes"] == complete["resumes"] + 2 and not after_resume["paused"]
        evidence["checks"]["exactPauseAndFreshReconciliationResumeGuards"] = resume_result
        evidence["healthy"] = True
        evidence["limits"] = ["Synthetic chain and synthetic authorizations in disposable PostgreSQL; no live chain transaction or signing was performed.", "Production runAaReconciler and database operations are used; the harness intentionally supplies loopback clients rather than weakening production CLI external-HTTPS guards."]
    finally:
        for process in reversed(processes):
            stop_process(process)
        if fixture:
            fixture.stop()
            (output / "rpc-reads.json").write_text(json.dumps(fixture.calls, indent=2) + "\n")
        # A failed/timed-out start may still have created the owned server.
        if started_pg or (data / "postmaster.pid").exists():
            subprocess.run([str(pg / "pg_ctl"), "-D", str(data), "stop", "-m", "immediate", "-w"], env=env, capture_output=True, text=True, check=False)
        evidence["finishedAt"] = time.time()
        evidence["cleanup"] = {"workerProcessesStopped": all(p.poll() is not None for p in processes), "postgresStopped": not (data / "postmaster.pid").exists()}
        cleanup_ok = all(evidence["cleanup"].values())
        if not cleanup_ok:
            evidence["healthy"] = False
        (output / "evidence.json").write_text(json.dumps(evidence, indent=2) + "\n")
        if not cleanup_ok:
            raise RuntimeError(f"Isolated drill cleanup did not complete; inspect {output / 'evidence.json'}")
    print(json.dumps({"healthy": evidence["healthy"], "checks": list(evidence["checks"]), "evidence": str(output / "evidence.json"), "cleanup": evidence["cleanup"]}))


if __name__ == "__main__":
    os.umask(0o077)
    main()
