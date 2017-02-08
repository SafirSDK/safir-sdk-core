 #!/usr/bin/env python
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
from __future__ import print_function
import sys, subprocess, os, shutil, difflib, argparse, re
from syslog_server import SyslogServer

parser = argparse.ArgumentParser("test script")
parser.add_argument("--show-safir-config", required=True)
parser.add_argument("--language", required=True)
parser.add_argument("--output", required=True)
parser.add_argument("--safir-generated-paths", required=True)

#cpp and dotnet
parser.add_argument("--binary")

#for java and dotnet
parser.add_argument("--dependencies")

#for java
parser.add_argument("--jar")

arguments = parser.parse_args()

#add all the environment variables. passed on format A=10;B=20
for pair in arguments.safir_generated_paths.split(";"):
    (name,value) = pair.split("=")
    print("Setting environment variable", name, "to", value)
    os.environ[name] = value

syslog = SyslogServer(arguments.show_safir_config)

if arguments.dependencies is not None:
    dependencies = arguments.dependencies.split(",")

    for dep in dependencies:
        shutil.copy2(dep,".")

if arguments.language == "cpp":
    command = (arguments.binary,"--detect_memory_leaks=0")
elif arguments.language == "java":
    #-Xfuture gives weird errors on RPi and BBone (both on debian jessie)
    command = ("java",) + \
      (("-Xfuture",) if not os.uname()[4].startswith("arm") else tuple()) + \
      ("-jar", arguments.jar)
elif arguments.language == "dotnet":
    command = (arguments.binary,)
else:
    print("Not implemented")
    sys.exit(1)

print("Test suite command is '" + " ".join(command) + "'")

proc = subprocess.Popen(command, stdout = subprocess.PIPE, stderr = subprocess.STDOUT, universal_newlines=True)
res = proc.communicate()[0].replace("\r","")

#Boost.test prints some status messages that we need to suppress.
res = res.replace("\n*** No errors detected\n","")
res =re.sub(r"^Running [0-9]+ test cases\.\.\.\n", "", res)

#java sometimes outputs some stuff
res =re.sub(r"^Picked up _JAVA_OPTIONS: .*\n", "", res)

res = res.splitlines(1) #fix any DOS newlines


with open(arguments.output) as expected_file:
    expected_output = expected_file.read().replace("\r","").splitlines(1) #fix any DOS newlines
diff = list(difflib.unified_diff(expected_output,res))

if len(diff) != 0:
    print("Unexpected output! Unified diff of expected output and actual output is:")
    print("".join(diff))
    sys.exit(1)

logs = syslog.get_data(0.5) #we wait for a very short while for any logs to propagate.
if len(logs) > 0:
    print("Unexpected logs in system log!")
    print(logs)
if proc.returncode != 0:
    print ("Unexpected returncode", proc.returncode)
    sys.exit(1)

print("Expected output achieved")
sys.exit(0)
