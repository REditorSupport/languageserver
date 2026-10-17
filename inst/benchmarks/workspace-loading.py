# Workspace startup latency. Requires Python 3 and an installed languageserver.
import argparse
import json
import os
from pathlib import Path
import shutil
import statistics
import subprocess
import tempfile
import threading
import time

parser = argparse.ArgumentParser()
parser.add_argument('library')
parser.add_argument('--root', type=Path)
parser.add_argument('--files', type=int, default=3000)
parser.add_argument('--lines', type=int, default=1000)
parser.add_argument('--symbols', type=int, default=50)
parser.add_argument('--requests', type=int, default=100)
parser.add_argument('--cached', action='store_true')
parser.add_argument('--package', action='store_true')
parser.add_argument('--keep-fixture', action='store_true')
args = parser.parse_args()
if min(args.files, args.lines, args.symbols, args.requests) < 1:
    parser.error('files, lines, symbols and requests must be positive')
root = args.root.resolve() if args.root else Path(tempfile.mkdtemp(prefix='workspace-loading-'))
root.mkdir(parents=True, exist_ok=True)
source_dir = root / 'R' if args.package else root
source_dir.mkdir(exist_ok=True)
if args.package:
    (root / 'DESCRIPTION').write_text('Package: loadingfixture\nVersion: 0.0.1\n')
content = '\n'.join(f'value_{i % args.symbols:04d} <- sum(c(1, 2, 3))'
                    for i in range(args.lines)) + '\n'
for i in range(args.files):
    (source_dir / f'file-{i:05d}.R').write_text(content, encoding='utf-8')
env = os.environ.copy()
env['R_LIBS'] = os.pathsep.join(filter(None, (
    str(Path(args.library).resolve()), env.get('R_LIBS', ''))))
env['R_USER_CACHE_DIR'] = str(root / '.cache')
env['LANGSVR_BENCH_ROOT'] = str(root)
env['LANGSVR_BENCH_SOURCES'] = str(source_dir)
if args.cached:
    # All files share content. Parse it once to construct an exact warm cache,
    # without waiting for thousands of asynchronous indexing tasks to finish.
    subprocess.run(['Rscript', '--vanilla', '-e', '''
        library(languageserver)
        root <- Sys.getenv("LANGSVR_BENCH_ROOT")
        paths <- list.files(Sys.getenv("LANGSVR_BENCH_SOURCES"),
            pattern = "^file-.*[.]R$", full.names = TRUE)
        index <- languageserver:::WorkspaceIndex$new(root)
        template <- languageserver:::index_shallow_summary(paths[[1L]],
            stringi::stri_read_lines(paths[[1L]]), root)
        metadata <- file.info(paths)
        summaries <- lapply(seq_along(paths), function(i) {
            summary <- template
            summary$path <- paths[[i]]
            summary$uri <- languageserver:::path_to_uri(paths[[i]])
            summary$size <- as.numeric(metadata$size[[i]])
            summary$mtime <- as.numeric(metadata$mtime[[i]])
            summary
        })
        dir.create(dirname(index$cache_file()), recursive = TRUE, showWarnings = FALSE)
        saveRDS(list(version = 1L, root = index$root, summaries = summaries), index$cache_file())
    '''], env=env, check=True)
program = ('options(languageserver.diagnostics=FALSE, '
           f'languageserver.index_persistent_cache={"TRUE" if args.cached else "FALSE"}); '
           'library(languageserver); Rprof("startup.Rprof", interval=0.001); '
           'languageserver::run(); Rprof(NULL)')
condition = threading.Condition()
responses = {}
reader_errors = []
reader_finished = False
next_id = 0
stderr = (root / 'stderr.log').open('wb')
process = subprocess.Popen(['Rscript', '--vanilla', '-e', program], env=env, cwd=root,
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=stderr)


def read_responses():
    global reader_finished
    try:
        while True:
            headers = {}
            while True:
                line = process.stdout.readline()
                if not line:
                    return
                if line == b'\r\n':
                    break
                key, value = line.decode().split(':', 1)
                headers[key.lower()] = value.strip()
            payload = json.loads(process.stdout.read(int(headers['content-length'])))
            if 'id' in payload and 'method' not in payload:
                with condition:
                    responses[payload['id']] = payload
                    condition.notify_all()
    except Exception as error:
        with condition:
            reader_errors.append(error)
    finally:
        with condition:
            reader_finished = True
            condition.notify_all()


def send(method, params=None, notification=False):
    global next_id
    payload = dict(jsonrpc='2.0', method=method, params=params or {})
    if not notification:
        next_id += 1
        payload['id'] = next_id
    body = json.dumps(payload, separators=(',', ':')).encode()
    process.stdin.write(f'Content-Length: {len(body)}\r\n\r\n'.encode() + body)
    process.stdin.flush()
    return next_id


def receive(request_id):
    with condition:
        if not condition.wait_for(lambda: request_id in responses or reader_errors or reader_finished,
                                  timeout=120):
            raise TimeoutError(f'No response to request {request_id}')
        if reader_errors:
            raise reader_errors[0]
        if request_id not in responses:
            raise RuntimeError('Server output closed: ' + (root / 'stderr.log').read_text())
        response = responses.pop(request_id)
    if 'error' in response:
        raise RuntimeError(response)
    return response.get('result')


reader = threading.Thread(target=read_responses, daemon=True)
reader.start()
try:
    receive(send('initialize', dict(rootUri=root.as_uri(), capabilities={})))
    send('initialized', notification=True)
    timings = []
    for i in range(args.requests):
        started = time.monotonic()
        result = receive(send('completionItem/resolve', dict(label='startup-probe')))
        if result.get('label') != 'startup-probe':
            raise RuntimeError(f'Unexpected probe result: {result}')
        elapsed = (time.monotonic() - started) * 1000
        timings.append(elapsed)
        print(json.dumps(dict(request=i + 1, elapsed_ms=round(elapsed, 3))), flush=True)
        time.sleep(0.1)
    print(json.dumps(dict(root=str(root), files=args.files, lines=args.files * args.lines,
        cached=args.cached, package=args.package, first_ms=round(timings[0], 3),
        median_ms=round(statistics.median(timings), 3), max_ms=round(max(timings), 3))), flush=True)
    receive(send('shutdown'))
    process.wait(timeout=10)
    if process.returncode:
        raise RuntimeError(f'Server exited with status {process.returncode}')
finally:
    if process.poll() is None:
        process.kill()
        process.wait()
    reader.join(timeout=10)
    stderr.close()
    if args.root is None and not args.keep_fixture:
        shutil.rmtree(root)
