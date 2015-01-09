#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
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
import subprocess, os, time, sys, random

#start two sleepers
#start the listener
#wait for sleepers to exit
#check output of listener


if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

ProcessMonitor_test = os.path.join(exe_path,"ProcessMonitor_test")
Sleeper = os.path.join(exe_path,"ProcessMonitorSleeper")

#start a bunch of sleepers
sleepers = list()
pids = list()
for which in range(0,100):
    proc = subprocess.Popen((Sleeper,str(random.random() + 0.5)))
    pids.append(str(proc.pid))
    sleepers.append(proc)

listener = subprocess.Popen(list((ProcessMonitor_test,))+ pids,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT,
                            universal_newlines=True)
for sleeper in sleepers:
    sleeper.wait()

result = listener.communicate()[0]
print("listener should be waiting for pids", pids)
print(result)

errors = 0
for sleeper in sleepers:
    if result.find("Process with pid " + str(sleeper.pid) + " exited.") == -1:
        print("ProcessMonitor appears to have missed termination of process with pid", sleeper.pid)
        errors += 1

if result.count("Process with pid") != len(sleepers):
    print("Wrong number of terminated processes! Expected", len(sleepers),"got",result.count("Process with pid"))
    errors += 1

if listener.returncode != 0:
    print("Unexpected return code from listener:",listener.returncode)
    errors += 1

if errors == 0:
    print("success")
else:
    print("failed")
sys.exit(errors)
