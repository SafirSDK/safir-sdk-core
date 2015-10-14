#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2014 (http://safirsdkcore.com)
#
# Created by: Anders Widén (anders.widen@consoden.se)
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
    
ProcessInfo_test = os.path.join(exe_path,"ProcessInfo_test")
Sleeper = os.path.join(exe_path,"ProcessInfoSleeper")

#start sleeper
sleeper = subprocess.Popen((Sleeper,"120"))

listener = subprocess.Popen((ProcessInfo_test,
                             "ProcessInfoSleeper",
                             str(sleeper.pid)),
                            stdout=subprocess.PIPE, 
                            stderr=subprocess.STDOUT,
                            universal_newlines=True)
result = listener.communicate()[0]
sleeper.kill()

if result != "":
    print("ProcessInfo_test exited with non-empty output!")
    print(result)
    sys.exit(1)

print("success")
sys.exit(0)
