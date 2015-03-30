#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
# Copyright Consoden AB, 2015 (http://www.consoden.se)
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

lines = list()
for i in range(2):
    lines.append(proc.stdout.readline().rstrip("\n\r"))
    log("Line", i, ": '" + lines[-1] + "'")

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



for i in range(2):
    lines.append(proc.stdout.readline().rstrip("\n\r"))
    log("Line", len(lines) - 1, ": '" + lines[-1] + "'")

res = proc.communicate()[0]

if len(res) != 0:
    log("More than four lines output! Trailing data is\n'"+res + "'")
    sys.exit(1)

running_str = "dose_main running ("
waiting_str = "dose_main is waiting for persistence data!"
ready_str = "dose_main persistence data is ready!"
exiting_str = "Exiting..."

if running_str not in lines[0] and running_str not in lines[1]:
    log("Failed to find string '" + running_str + "'")
    sys.exit(1)
if waiting_str not in lines[0] and waiting_str not in lines[1]:
    log("Failed to find string '" + waiting_str + "'")
    sys.exit(1)
#TODO stewart: remove this line since dope is not started by this test
if ready_str not in lines[2]:
    log("Failed to find string '" + ready_str + "'")
    sys.exit(1)
if exiting_str not in lines[3]:
    log("Failed to find string '" + exiting_str + "'")
    sys.exit(1)

log("success")
sys.exit(0)
