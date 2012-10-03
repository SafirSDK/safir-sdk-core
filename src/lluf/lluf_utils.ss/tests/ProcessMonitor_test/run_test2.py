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
    
ProcessMonitor_test2 = os.path.join(exe_path,"ProcessMonitor_test2")
Sleeper = os.path.join(exe_path,"Sleeper")

#start two sleepers
sleeper1 = subprocess.Popen(Sleeper)
sleeper2 = subprocess.Popen(Sleeper)

listener = subprocess.Popen((ProcessMonitor_test2,
                             str(sleeper1.pid),
                             str(sleeper2.pid)),
                            stdout=subprocess.PIPE, 
                            stderr=subprocess.STDOUT,
                            universal_newlines=True)
sleeper1.wait()
sleeper2.wait()
result = listener.communicate()[0]

if result != "":
    print("ProcessMonitor_test2 exited with non-empty output!")
    print(result)
    sys.exit(1)

print("success")
sys.exit(0)
