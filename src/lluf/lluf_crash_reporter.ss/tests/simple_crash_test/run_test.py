#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2012-2013,2023, 2026 (http://safirsdkcore.com)
#
# Created by: Lars Hagstrom (lars.hagstrom@consoden.se)
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
import subprocess, os, time, sys, re
import argparse

def parse_arguments():
    parser = argparse.ArgumentParser(description='unit test script')
    parser.add_argument("--crasher-exe", help="The test executable", required=True)
    return parser.parse_args()

args = parse_arguments()

# The crasher is meant to die of a crash signal. AddressSanitizer installs its own
# handler for those and turns the death into exit code 1 with a report, which is
# not what is under test here, so tell it to leave the crash signals alone. In a
# build without sanitizers the variable is simply ignored.
child_env = dict(os.environ)
child_env["ASAN_OPTIONS"] = ":".join(
    filter(None, [child_env.get("ASAN_OPTIONS"), "handle_segv=0:handle_sigfpe=0:handle_sigill=0:handle_abort=0"]))

# The crasher provokes its signals by committing deliberate undefined behaviour: a
# store through a null pointer for SIGSEGV, a division by zero for SIGFPE. UBSan
# reports those correctly, and under halt_on_error=1 it aborts at the report - before
# the signal that is actually under test is ever raised. Force it off for the child;
# the last assignment wins in the sanitizer flag parser, so this overrides whatever
# the suite was run with.
child_env["UBSAN_OPTIONS"] = ":".join(
    filter(None, [child_env.get("UBSAN_OPTIONS"), "halt_on_error=0"]))


def run_crasher(reason):
    crasher = subprocess.Popen((args.crasher_exe, reason), stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                               env=child_env)
    result = crasher.communicate()[0].decode("ascii")
    print("Testing signal", reason)
    if result.find("callback") == -1:
        print("CrashReporter did not call callback!")
        sys.exit(1)
    if crasher.returncode == 0:
        print("Crasher program exited successfully (it is meant to crash!), exit code = ", crasher.returncode)
        sys.exit(1)

    match = re.search(r"dumpPath = '(.*)'", result)
    if match is None:
        print("Failed to find dumpPath in output")
        print(result)
        sys.exit(1)

    dumpPath = match.group(1)

    if not os.path.isfile(dumpPath):
        print("No dumpfile appears to have been generated")
        print("expected to find", dumpPath)
        sys.exit(1)
    os.remove(dumpPath)


run_crasher("SIGSEGV")
run_crasher("SIGFPE")
run_crasher("SIGILL")
if sys.platform != "win32":
    run_crasher("SIGABRT")

print("success")
sys.exit(0)
