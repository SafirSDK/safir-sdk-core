#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
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

crasher_exe = os.path.join(exe_path,"crasher")


def run_crasher(reason):
    crasher = subprocess.Popen((crasher_exe,reason),
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    result = crasher.communicate()[0].decode("ascii")
    print("Testing signal", reason)
    if result.find("callback") == -1:
        print("CrashReporter did not call callback!")
        sys.exit(1)
    if crasher.returncode == 0:
        print("Crasher program exited successfully (it is meant to crash!), exit code = ", crasher.returncode)
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
    os.remove(dumpPath)

run_crasher("SIGSEGV")
run_crasher("SIGFPE")
run_crasher("SIGILL")
if sys.platform != "win32":
    run_crasher("SIGABRT")

print("success")
sys.exit(0)
