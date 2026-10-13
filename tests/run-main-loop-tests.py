#!/usr/bin/env python3
"""Exercise native GUI scheduling only in disposable Emacs processes."""
import argparse
import os
from pathlib import Path
import resource
import shlex
import signal
import subprocess
import tempfile


def no_core():
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))


def run(args, env, expected=0, timeout=30):
    process = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                               text=True, env=env, start_new_session=True,
                               preexec_fn=no_core)
    try:
        output, _ = process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        output, _ = process.communicate()
        raise RuntimeError(f"Isolated Emacs timed out:\n{output}")
    accepted = expected if isinstance(expected, tuple) else (expected,)
    if process.returncode not in accepted:
        raise RuntimeError(f"Unexpected exit {process.returncode}:\n{output}")
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--emacs', required=True)
    parser.add_argument('--webkit-directory', required=True)
    parser.add_argument('--module-include', required=True)
    parser.add_argument('--cc', default='cc')
    parser.add_argument('--pkg-config', default='pkg-config')
    parser.add_argument('--glib-flags', help='Explicit GLib compiler/linker flags (optional)')
    args = parser.parse_args()
    here = Path(__file__).resolve().parent
    flags = shlex.split(args.glib_flags if args.glib_flags is not None else
                        subprocess.check_output(
                            [args.pkg_config, '--cflags', '--libs', 'glib-2.0'], text=True))
    with tempfile.TemporaryDirectory(prefix='gnus-main-loop-test-') as temp:
        directory = Path(temp)
        fixture = directory / 'ready-source.so'
        probe = directory / 'main-thread-probe.so'
        subprocess.run([args.cc, '-shared', '-fPIC', '-Wall', '-Wextra',
                        '-I' + args.module_include, str(here / 'glib-ready-source.c'),
                        *flags, '-o', str(fixture)], check=True)
        subprocess.run([args.cc, '-shared', '-fPIC', '-Wall', '-Wextra',
                        str(here / 'glib-main-thread-probe.c'), '-ldl', '-pthread',
                        '-o', str(probe)], check=True)
        env = dict(os.environ, XDG_STATE_HOME=str(directory / 'state'))
        output = run([args.emacs, '--batch', '-Q', '-l',
                      str(here / 'main-thread-timer-test.el'), '-f',
                      'ert-run-tests-batch-and-exit'], env)
        print(output, end='')
        env['LD_PRELOAD'] = str(probe)
        for name in ('glib-ready-source-test', 'glib-cold-preview-test', 'glib-worker-gui-test'):
            # Catch startup errors explicitly; a debugger in a graphical
            # process would wait for user input and obscure test failures.
            source = str(here / (name + '.el'))
            expression = (
                '(condition-case err (load "' + source + '" nil t) '
                '(error (princ (format "TEST-FAIL: %s\\n" (error-message-string err)) '
                '(quote external-debugging-output)) (kill-emacs 1)))')
            output = run([args.emacs, '-Q', '-L', args.webkit_directory,
                          '-l', str(fixture), '--eval', expression], env)
            if name + ': PASS' not in output or 'main-thread dispatches' not in output:
                raise RuntimeError('Missing native test evidence:\n' + output)
            print('\n'.join(line for line in output.splitlines()
                            if ': PASS' in line or 'glib-main-thread-probe:' in line))
        # A fatal error during GUI activity must finish cleanup, rather than
        # recursively draining WebKit callbacks in the signal handler.
        run([args.emacs, '-Q', '-l', str(fixture), '--eval',
             '(progn (make-process :name "fatal-cleanup-fixture" '
             ':command (list "sleep" "10") :noquery t) '
             '(glib-test-start) (glib-test-fail))'], env,
            expected=(-signal.SIGABRT, 128 + signal.SIGABRT), timeout=5)
        print('glib-fatal-cleanup-test: PASS')


if __name__ == '__main__':
    main()
