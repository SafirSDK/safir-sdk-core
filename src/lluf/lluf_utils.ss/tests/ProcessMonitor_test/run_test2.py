#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011-2013 (http://safirsdkcore.com)
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
import subprocess, os, time, sys

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

ProcessMonitor_test2 = os.path.join(exe_path, "ProcessMonitor_test2")
Sleeper = os.path.join(exe_path, "ProcessMonitorSleeper")

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
