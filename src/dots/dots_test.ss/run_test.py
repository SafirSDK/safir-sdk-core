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
import sys
import subprocess
import os
import shutil
import difflib
import argparse
import re
import platform

from syslog_server import SyslogServer

parser = argparse.ArgumentParser("test script")
parser.add_argument("--show-safir-config", required=True)
parser.add_argument("--language", required=True)
parser.add_argument("--output", required=True)

#cpp and dotnet
parser.add_argument("--binary")

#for java
parser.add_argument("--jar")

arguments = parser.parse_args()

syslog = SyslogServer(arguments.show_safir_config)

if arguments.language == "cpp":
    command = (arguments.binary, "--detect_memory_leaks=0") #disable boost.test memory leak check which detects spurious leak.
elif arguments.language == "java":
    command = ("java", "-jar", arguments.jar)
elif arguments.language == "dotnet":
    if platform.system() == "Windows":
        command = (arguments.binary,)
    else:
        command = ("mono", arguments.binary)
else:
    print("Not implemented")
    sys.exit(1)

print("Test suite command is '" + " ".join(command) + "'")

proc = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)
res = proc.communicate()[0].replace("\r", "")

#Boost.test prints some status messages that we need to suppress.
res = re.sub("\n.*\*\*\* No errors detected\n.*", "", res)
res = re.sub(r"^Running [0-9]+ test cases\.\.\.\n", "", res)

#java sometimes outputs some stuff
res = re.sub(r"^Picked up _JAVA_OPTIONS: .*\n", "", res)

res = res.splitlines(1)  #fix any DOS newlines

with open(arguments.output) as expected_file:
    expected_output = expected_file.read().replace("\r", "").splitlines(1)  #fix any DOS newlines
diff = list(difflib.unified_diff(expected_output, res))

if len(diff) != 0:
    print("Unexpected output! Unified diff of expected output and actual output is:")
    print("".join(diff))
    sys.exit(1)

logs = syslog.get_data(0.5)  #we wait for a very short while for any logs to propagate.
if len(logs) > 0:
    print("Unexpected logs in system log!")
    print(logs)
if proc.returncode != 0:
    print("Unexpected returncode", proc.returncode)
    sys.exit(1)

print("Expected output achieved")
sys.exit(0)
