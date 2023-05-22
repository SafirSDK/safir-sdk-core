#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011-2013, 2023 (http://safirsdkcore.com)
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
import subprocess
import os
import sys
import argparse

def parse_arguments():
    parser = argparse.ArgumentParser(description='unit test script')
    parser.add_argument("--test-exe", help="The test executable", required=True)
    parser.add_argument("--sleeper-exe", help="The sleeper executable", required=True)
    return parser.parse_args()

args = parse_arguments()

ProcessMonitor_test2 = args.test_exe
Sleeper = args.sleeper_exe

#start a bunch of sleepers
sleepers = list()
pids = list()
for which in range(0, 100):
    proc = subprocess.Popen((Sleeper, "120"))
    pids.append(str(proc.pid))
    sleepers.append(proc)

listener = subprocess.Popen(list((ProcessMonitor_test2, )) + pids,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT,
                            universal_newlines=True)
result = listener.communicate()[0]
for sleeper in sleepers:
    sleeper.kill()

if result != "":
    print("ProcessMonitor_test2 exited with non-empty output!")
    print(result)
    sys.exit(1)

print("success")
sys.exit(0)
