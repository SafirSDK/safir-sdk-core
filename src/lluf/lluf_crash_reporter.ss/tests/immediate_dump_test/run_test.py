#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2012 (http://www.safirsdk.com)
#
# Created by: Lars Hagstrom (lars.hagstrom@consoden.se)
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

dumper_exe = os.path.join(exe_path,"dumper")


dumper = subprocess.Popen(dumper_exe,
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
result = dumper.communicate()[0].decode("ascii")

if result.find("callback") == -1:
    print("CrashReporter did not call callback!")
    sys.exit(1)
if dumper.returncode != 0:
    print("Dumper program exited with unexpected returncode", dumper.returncode)
    sys.exit(1)

match = re.search(r"dumpPath = '(.*)'",result)
if match is None:
    print("Failed to find dumpPath in output")
    print(result)
    sys.exit(1)
    
dumpPath = match.group(1)

if not os.path.isfile(dumpPath):
    print("No dumpfile appears to have been generated")
    print("expected to find", dumpPath)
    sys.exit(1)

print("success")
sys.exit(0)
