#!/usr/bin/env python
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
from __future__ import print_function
import subprocess, os, time, sys, signal, argparse

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

log("This test program expects to be killed off after about two minutes unless it has finished successfully before then.")

proc = subprocess.Popen([arguments.safir_control, "--dose-main-path" , arguments.dose_main],
                        stdout = subprocess.PIPE,
                        stderr = subprocess.STDOUT,
                        universal_newlines=True,
                        creationflags = subprocess.CREATE_NEW_PROCESS_GROUP if sys.platform == "win32" else 0)

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

for i in range (100):
    if proc.poll() is not None:
        break
    time.sleep(0.1)
if proc.poll() is None:
    try:
        proc.kill()
        log("Unable to stop Control and/or dose_main! OS resources might be locked, preventing further tests from running.")
        sys.exit(1)
    except OSError:
        pass
    proc.wait()

lines += proc.communicate()[0].splitlines()

for i in range(len(lines)):
    log("Line", i, ": '" + lines[i] + "'")

expected_lines = set(["dose_main running...",
                      "dose_main is waiting for persistence data!",
                      "DOSE_MAIN: Exiting...",
                      "CTRL: Exiting..."])

if sys.platform == "win32":
    expected_lines.add("CTRL: Got signal 21 ... stop sequence initiated.")
else:
    expected_lines.add("CTRL: Got signal 15 ... stop sequence initiated.")


if set(lines) - expected_lines:
    log("Got unexpected output:")
    log (set(lines) - expected_lines)
    sys.exit(1)

if expected_lines - set(lines):
    log("Missing expected output:")
    log (expected_lines - set(lines))

if set(lines) != expected_lines:
    log("something is wrong in the test script...")
    sys.exit(1)

log("success")
sys.exit(0)
