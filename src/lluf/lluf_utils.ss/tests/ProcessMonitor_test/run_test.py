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
import subprocess, os, time, sys

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

#start two sleepers
sleeper1 = subprocess.Popen((Sleeper,"1"))
sleeper2 = subprocess.Popen((Sleeper,"1"))

listener = subprocess.Popen((ProcessMonitor_test,
                             str(sleeper1.pid),
                             str(sleeper2.pid)),
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT,
                            universal_newlines=True)
sleeper1.wait()
sleeper2.wait()
result = listener.communicate()[0]
print("listener should be waiting for pids", sleeper1.pid, sleeper2.pid)
print(result)

if result.find("Process with pid " + str(sleeper1.pid) + " exited.") == -1:
    print("ProcessMonitor appears to have missed termination of process with pid", sleeper1.pid)
    sys.exit(1)

if result.find("Process with pid " + str(sleeper2.pid) + " exited.") == -1:
    print("ProcessMonitor appears to have missed termination of process with pid", sleeper2.pid)
    sys.exit(1)

if result.count("Process with pid") != 2:
    print("Too many terminated processes! Expected two!")
    sys.exit(1)

if listener.returncode != 0:
    print("Unexpected return code from listener:",listener.returncode)
    sys.exit(1)

print("success")
sys.exit(0)
