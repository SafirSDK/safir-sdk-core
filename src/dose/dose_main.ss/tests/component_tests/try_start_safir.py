#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011-2013,2015 (http://safirsdkcore.com)
#
# Created by: Lars Hagstrom (lars@foldspace.nu)
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
import subprocess, os, time, sys, signal, argparse, re


def log(*args, **kwargs):
    print(*args, **kwargs)
    sys.stdout.flush()


def string_in_output(string, output):
    for line in output:
        if string in line:
            return True
    return False


parser = argparse.ArgumentParser("test script")
parser.add_argument("--safir-control", required=True)
parser.add_argument("--dose-main", required=True)
arguments = parser.parse_args()

proc = subprocess.Popen([arguments.safir_control, "--dose-main-path", arguments.dose_main],
                        stdout=subprocess.PIPE,
                        stderr=subprocess.STDOUT,
                        universal_newlines=True,
                        creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if sys.platform == "win32" else 0)

#first we read two lines, that should indicate that dose_main got started reasonably well
lines = list()
for i in range(2):
    lines.append(proc.stdout.readline().rstrip("\n\r"))

#give it one second to output any spurious stuff...
time.sleep(1)

if sys.platform == "win32":
    #can't send CTRL_C_EVENT to processes started with subprocess, unfortunately
    proc.send_signal(signal.CTRL_BREAK_EVENT)
else:
    proc.terminate()

for i in range(100):
    if proc.poll() is not None:
        break
    time.sleep(0.1)
if proc.poll() is None:
    try:
        proc.kill()
        log("Unable to stop Control and/or dose_main! OS resources might be locked, preventing further tests from running."
            )
        sys.exit(1)
    except OSError:
        pass
    proc.wait()

lines += proc.communicate()[0].splitlines()

expected_lines = [
    "MAIN: dose_main running...",
    "MAIN: dose_main is waiting for persistence data!",
    "CTRL: Starting system with incarnation id -?[0-9]+",
    "CTRL: This node has id [0-9]+",
    "MAIN: DOSE_MAIN: Exiting...",
    "CTRL: Exiting..."
]

if sys.platform == "win32":
    expected_lines.append("CTRL: Got signal 21 ... stop sequence initiated.")
else:
    expected_lines.append("CTRL: Got signal 15 ... stop sequence initiated.")

found = list()

for expected in expected_lines:
    for line in lines:
        if re.match(expected, line):
            found.append(line)
            break

if len(set(lines) - set(found)) != 0:
    log("Got unexpected output:")
    log(set(lines) - set(found))
    sys.exit(1)

if len(expected_lines) != len(lines):
    log("Missing expected output!")
    log("expected:", expected_lines)
    log("got:", lines)

log("success")
sys.exit(0)
