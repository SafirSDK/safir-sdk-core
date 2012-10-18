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
import subprocess, os, time, sys, re

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

ss_test = os.path.join(exe_path,"ss_processes_and_threads_test")
procs = list()

try:
    #Start a bunch of processes
    for i in range(100):
        proc = subprocess.Popen(ss_test, 
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,
                                stdin=subprocess.PIPE,
                                universal_newlines=True)
        
        procs.append(proc)
except OSError as e:
    print ("Failed to launch processes. maybe you need to increase ulimit -u? I need at least 10000")
    print (e)
    for proc in procs:
        proc.kill()
    sys.exit(1)

#wait for them to print something that indicates they're started
for proc in procs:
    proc.stdout.readline()

print("have read a line from all")

outputs = list()
#tell them it is okay to exit
for proc in procs:
    #this is a workaround where pyton 3.3 and 3.2.3 require different argument types, so we
    #try both
    try:
        out = proc.communicate("\n".encode())[0]
    except TypeError:
        out = proc.communicate("\n")[0]
    outputs.append(out)

for proc in procs:
    if proc.returncode != 0:
        print("unexpected return code",proc.returncode)
        for i in range(len(outputs)):
            print("Output from instance (first line has been removed)", i)
            print(outputs[i])
        sys.exit(1)

num_created = 0
num_used = 0
num_destroyed = 0

created_pattern = re.compile(r"num_created: ([0-9]+)")
used_pattern = re.compile(r"num_used: ([0-9]+)")
destroyed_pattern = re.compile(r"num_destroyed: ([0-9]+)")

for output in outputs:
    num_created += int(created_pattern.search(output).group(1))
    num_used += int(used_pattern.search(output).group(1))
    num_destroyed += int(destroyed_pattern.search(output).group(1))
    
print ("num_created:", num_created)
print ("num_used:", num_used)
print ("num_destroyed:", num_destroyed)


if num_created == 1 and num_used == len(procs) * 100 and num_destroyed == 1:
    print("success")
    sys.exit(0)
else:
    print("failure!")
    for i in range(len(outputs)):
        print("Output from instance", i)
        print(outputs[i])
    sys.exit(1)
