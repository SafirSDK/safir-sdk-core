#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011 (http://www.safirsdk.com)
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
import subprocess, os, time, sys

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

ss_test = os.path.join(exe_path,"ss_test")
procs = list()

#Start a bunch of processes
for i in range(100):
    proc = subprocess.Popen(ss_test, 
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT,
                            stdin=subprocess.PIPE,
                            universal_newlines=True)

    procs.append(proc)

#wait for them to print something that indicates they're started
for proc in procs:
    proc.stdout.readline()

print("have read a line from all")

outputs = list()
#tell them it is okay to exit
for proc in procs:
    out = proc.communicate("\n")[0].decode("ascii")
    outputs.append(out)

num_3 = 0
num_2 = 0
num_other = 0
for proc in procs:
    if proc.returncode == 3:
        num_3 += 1
    elif proc.returncode == 2:
        num_2 += 1
    else:
        num_other += 1

#check exit codes 
#see bit field in ss_test.cpp
if num_3 == 1 and num_2 == len(procs) - 1 and num_other == 1:
    print("success")
    sys.exit(0)
else:
    print("failure!")
    for i in range(len(outputs)):
        print("Output from instance", i)
        print(outputs[i])
    sys.exit(1)
