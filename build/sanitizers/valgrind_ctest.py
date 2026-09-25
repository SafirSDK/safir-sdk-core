#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2026 (http://safirsdkcore.com)
#
###############################################################################
#
# This file is part of Safir SDK Core.
#
# Safir SDK Core is free software: you can redistribute it and/or modify
# it under the terms of version 3 of the GNU General Public License as
# published by the Free Software Foundation.
#
# Safir SDK Core is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################
"""Run the ctest suite of a build tree under valgrind memcheck, one test at a time.

The test list comes from `ctest --show-only=json-v1`, so every test runs with the
command, working directory and environment that ctest itself would use. The command
is prefixed with valgrind and --trace-children=yes, so the binaries that the python
test drivers start are checked too. Python, java, mono and system binaries are not
traced.

Output, under --out:
  logs/<test>.<pid>.log   one valgrind report per checked process
  <test>.output.txt       the test's own stdout and stderr
  summary.tsv             test name, return code (or TIMEOUT), seconds

Between tests, every process started from the build tree is killed and the Safir
shared memory is removed, so a test that hangs or crashes cannot poison the next.
Which failures are expected under valgrind is described in AGENTS.md, under
"Valgrind memcheck". Group the reports with group_reports.py.
"""
import argparse
import glob
import json
import os
import re
import signal
import subprocess
import time

VALGRIND = [
    "valgrind",
    "--tool=memcheck",
    "--trace-children=yes",
    "--trace-children-skip=*python*,*/java,*/mono,/bin/*,/usr/bin/*,/usr/lib/*",
    "--track-origins=yes",
    "--leak-check=full",
    "--show-leak-kinds=definite",
    "--errors-for-leak-kinds=definite",
    "--num-callers=40",
    "--fullpath-after=",
    "--child-silent-after-fork=yes",
]


def kill_leftovers(build):
    """Kill everything started from the build tree, then remove the Safir shared memory.

    Matches on the executable (or, for processes running under valgrind, the command
    line), not on a pkill pattern, since some Safir processes have a bare argv[0].
    """
    for proc in glob.glob("/proc/[0-9]*"):
        try:
            exe = os.readlink(proc + "/exe")
            with open(proc + "/cmdline", "rb") as cmdline_file:
                cmdline = cmdline_file.read().decode(errors="replace")
        except OSError:
            continue
        under_valgrind = "valgrind" in exe or "memcheck" in exe
        if exe.startswith(build) or (under_valgrind and build in cmdline):
            try:
                os.kill(int(os.path.basename(proc)), signal.SIGKILL)
            except OSError:
                pass
    time.sleep(1)
    for path in glob.glob("/dev/shm/SAFIR_*") + glob.glob("/dev/shm/sem.*SAFIR*"):
        try:
            os.remove(path)
        except OSError:
            pass


def run_test(test, args, build, out):
    """Run one ctest test under valgrind and return its return code, or "TIMEOUT"."""
    name = test["name"]
    props = {p["name"]: p["value"] for p in test.get("properties", [])}
    env = dict(os.environ)
    for keyval in props.get("ENVIRONMENT", []):
        key, value = keyval.split("=", 1)
        env[key] = value
    # Let valgrind see python's allocations as plain mallocs, not pymalloc arenas.
    env["PYTHONMALLOC"] = "malloc"
    timeout = min(props.get("TIMEOUT", 1500.0) * args.factor, args.max_timeout)
    cmd = VALGRIND + [f"--log-file={out}/logs/{name}.%p.log"] + test["command"]
    with open(os.path.join(out, name + ".output.txt"), "w", encoding="utf-8") as output:
        with subprocess.Popen(cmd,
                              cwd=props.get("WORKING_DIRECTORY", build),
                              env=env,
                              stdout=output,
                              stderr=subprocess.STDOUT,
                              start_new_session=True) as proc:
            try:
                return proc.wait(timeout=timeout)
            except subprocess.TimeoutExpired:
                os.killpg(proc.pid, signal.SIGKILL)
                proc.wait()
                return "TIMEOUT"


def main():
    """Parse the arguments and run the selected tests one at a time."""
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--build", required=True, help="the ctest build tree (a plain, unsanitized build)")
    parser.add_argument("--out", required=True, help="directory for reports, output and summary.tsv")
    parser.add_argument("-R", default=None, help="only run tests matching this regex")
    parser.add_argument("-E",
                        default="java|dotnet",
                        help="exclude tests matching this regex (default: %(default)s, which only check "
                        "managed code under valgrind)")
    parser.add_argument("--factor", type=float, default=20.0, help="multiplier for each test's ctest timeout")
    parser.add_argument("--max-timeout", type=float, default=2400.0, help="upper limit for any test's timeout")
    args = parser.parse_args()

    build = os.path.abspath(args.build)
    out = os.path.abspath(args.out)
    os.makedirs(os.path.join(out, "logs"), exist_ok=True)

    tests = json.loads(subprocess.check_output(["ctest", "--show-only=json-v1"], cwd=build))["tests"]
    kill_leftovers(build)
    with open(os.path.join(out, "summary.tsv"), "a", encoding="utf-8") as summary:
        for test in tests:
            name = test["name"]
            if args.R and not re.search(args.R, name):
                continue
            if args.E and re.search(args.E, name):
                continue
            start = time.time()
            returncode = run_test(test, args, build, out)
            kill_leftovers(build)
            line = f"{name}\t{returncode}\t{time.time() - start:.0f}"
            print(line, flush=True)
            summary.write(line + "\n")
            summary.flush()


if __name__ == "__main__":
    main()
