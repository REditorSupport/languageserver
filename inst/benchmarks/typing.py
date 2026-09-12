# End-to-end typing latency. Requires Python 3 and an installed languageserver.
# Usage: python3 inst/benchmarks/typing.py /tmp/languageserver-after --wait-parse --providers
import argparse
import json
import os
from pathlib import Path
import subprocess
import shutil
import tempfile
import threading
import time

parser = argparse.ArgumentParser()
parser.add_argument("library")
parser.add_argument("--lines", type=int, default=20000)
parser.add_argument("--providers", action="store_true")
parser.add_argument("--wait-parse", action="store_true")
parser.add_argument("--index", choices=("off", "auto"), default="off")
parser.add_argument("--rounds", type=int, default=1)
parser.add_argument("--pause", type=float, default=0.25)
parser.add_argument("--open-delay", type=float, default=0)
parser.add_argument("--keep-fixture", action="store_true")
parser.add_argument("--profile")
args = parser.parse_args()
if args.lines < 1 or args.rounds < 1 or min(args.pause, args.open_delay) < 0:
    parser.error("lines/rounds must be positive and delays nonnegative")

root = Path(tempfile.mkdtemp(prefix="completion-fixture-"))
file = root / "script.R"
lines = [f"value_{i:05d} <- sum(c(1, 2, 3))" for i in range(args.lines)] + [""]
file.write_text("\n".join(lines), encoding="utf-8")
env = os.environ.copy()
env["R_LIBS"] = os.pathsep.join(filter(None, (str(Path(args.library).resolve()), env.get("R_LIBS", ""))))
program = ('options(languageserver.diagnostics=FALSE, '
           'languageserver.index_persistent_cache=FALSE, '
           f'languageserver.index_mode="{args.index}"); library(languageserver); ')
if args.profile:
    program += f'Rprof({json.dumps(str(Path(args.profile).resolve()))}, interval=0.001); '
program += 'languageserver::run(); Rprof(NULL)'
stderr = open(root / "stderr.log", "wb")
process = subprocess.Popen(["Rscript", "--vanilla", "-e", program], env=env,
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=stderr, cwd=root)
next_id = 0
condition = threading.Condition()
requests = {}
responses = {}
background_responses = {}
reader_failure = None
reader_finished = False
bytes_received = 0
closing = threading.Event()


def check_reader(allow_exit=False):
    # The caller holds condition, keeping error/EOF checks atomic with waits.
    if reader_failure is not None:
        raise RuntimeError("Language server response reader failed") from reader_failure
    if reader_finished and not allow_exit:
        raise RuntimeError("Language server output closed:\n" +
            (root / "stderr.log").read_text(errors="replace"))


def read_responses():
    global reader_failure, reader_finished, bytes_received
    buffer = bytearray()
    expected = None
    try:
        while True:
            chunk = os.read(process.stdout.fileno(), 1048576)
            if not chunk:
                if buffer or expected is not None:
                    raise RuntimeError("Incomplete JSON-RPC frame at end of server output")
                break
            bytes_received += len(chunk)
            buffer.extend(chunk)
            while True:
                if expected is None:
                    header_end = buffer.find(b"\r\n\r\n")
                    if header_end < 0:
                        break
                    headers = {}
                    for line in bytes(buffer[:header_end]).split(b"\r\n"):
                        name, value = line.split(b":", 1)
                        headers[name.strip().lower()] = value.strip()
                    expected = int(headers[b"content-length"])
                    if expected < 0:
                        raise RuntimeError("Negative JSON-RPC Content-Length")
                    del buffer[:header_end + 4]
                if len(buffer) < expected:
                    break
                body_size = expected
                payload = json.loads(buffer[:expected])
                del buffer[:expected]
                expected = None
                if "method" in payload:
                    if "id" in payload:
                        raise RuntimeError(f"Unexpected server request: {payload}")
                    continue  # Notifications need no response in this fixture.
                with condition:
                    response_id = payload.get("id")
                    request = requests.pop(response_id, None)
                    if request is None:
                        raise RuntimeError(f"Response to unknown request: {payload}")
                    method, background = request
                    error = payload.get("error")
                    # Only fire-and-forget provider requests can legitimately
                    # be cancelled after the next edit supersedes their version.
                    allowed_cancellation = background and error is not None and \
                        error.get("code") == -32800
                    if error is not None and not allowed_cancellation:
                        raise RuntimeError(f"{method} failed: {payload}")
                    if background:
                        # Keep errors and counts, without retaining enormous
                        # symbol/token results for every benchmark round.
                        background_responses[response_id] = {
                            "method": method, "error": error, "bytes": body_size
                        }
                    else:
                        responses[response_id] = payload.get("result")
                    condition.notify_all()
    except Exception as error:
        if not closing.is_set():
            with condition:
                reader_failure = error
                condition.notify_all()
    finally:
        with condition:
            reader_finished = True
            condition.notify_all()


def send(method, params=None, request=True, background=False):
    global next_id
    payload = {"jsonrpc": "2.0", "method": method}
    if params is not None:
        payload["params"] = params
    with condition:
        check_reader()
        if request:
            next_id += 1
            payload["id"] = next_id
            requests[next_id] = (method, background)
    body = json.dumps(payload, separators=(",", ":")).encode("utf-8")
    process.stdin.write(f"Content-Length: {len(body)}\r\n\r\n".encode() + body)
    process.stdin.flush()
    return next_id if request else None


def receive(target, timeout=60):
    deadline = time.monotonic() + timeout
    with condition:
        while True:
            check_reader(allow_exit=True)
            if target in responses:
                return responses.pop(target)
            check_reader()
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError(f"No response to request {target} within {timeout}s")
            condition.wait(remaining)


def pause(duration):
    with condition:
        condition.wait_for(lambda: reader_failure is not None or reader_finished, duration)
        check_reader()


# Editors keep consuming stdout between keystrokes. Continuously drain the
# pipe here too, otherwise large background responses block the server during
# the pauses and artificially inflate the following completion measurement.
reader = threading.Thread(target=read_responses, name="lsp-response-reader", daemon=True)
reader.start()

try:
    receive(send("initialize", {"rootUri": root.as_uri(), "capabilities": {
        "textDocument": {"completion": {"completionItem": {"snippetSupport": True}}}}}))
    send("initialized", {}, False)
    uri = file.as_uri()
    send("textDocument/didOpen", {"textDocument": {"uri": uri, "languageId": "r",
        "version": 1, "text": "\n".join(lines)}}, False)
    doc = {"textDocument": {"uri": uri}}
    previous = ""
    version = 1
    pause(args.open_delay)
    for round_number in range(1, args.rounds + 1):
        if args.wait_parse or round_number > 1:
            started = time.monotonic()
            receive(send("textDocument/documentSymbol", doc))
            print(json.dumps({"round": round_number, "prepare_ms": round(
                (time.monotonic() - started) * 1000, 2)}), flush=True)
            pause(0.1)
        if args.providers:
            send("textDocument/documentSymbol", doc, background=True)
            send("textDocument/semanticTokens/full", doc, background=True)
            send("textDocument/foldingRange", doc, background=True)
        for token in ["v", "va", "val", "valu", "value", "value_", "value_0", "value_00"]:
            pause(args.pause)
            version += 1
            point = {"line": args.lines, "character": len(token)}
            start = time.monotonic()
            send("textDocument/didChange", {"textDocument": {"uri": uri, "version": version},
                "contentChanges": [{"range": {"start": {"line": args.lines, "character": 0},
                    "end": {"line": args.lines, "character": len(previous)}}, "text": token}]}, False)
            result = receive(send("textDocument/completion", {**doc, "position": point}))
            print(json.dumps({"round": round_number, "token": token,
                "elapsed_ms": round((time.monotonic() - start) * 1000, 2),
                "items": len(result.get("items", []))}), flush=True)
            previous = token
    receive(send("shutdown", {}))
    exit_code = process.wait(timeout=10)
    reader.join(timeout=10)
    if reader.is_alive():
        raise RuntimeError("Response reader did not finish after server shutdown")
    with condition:
        check_reader(allow_exit=True)
        if exit_code:
            raise RuntimeError(f"Language server exited with status {exit_code}:\n" +
                (root / "stderr.log").read_text(errors="replace"))
        print(json.dumps({"transport": {
            "bytes_received": bytes_received,
            "providers_completed": sum(item["error"] is None for item in background_responses.values()),
            "providers_cancelled": sum(item["error"] is not None for item in background_responses.values()),
            "providers_pending": sum(background for _, background in requests.values())
        }}), flush=True)
finally:
    closing.set()
    if process.poll() is None:
        process.kill()
        process.wait()
    try:
        process.stdin.close()
    except BrokenPipeError:
        pass
    reader.join(timeout=5)
    process.stdout.close()
    stderr.close()
    if args.keep_fixture:
        print("fixture", root)
    else:
        shutil.rmtree(root)
