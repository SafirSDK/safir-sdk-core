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
import sys, subprocess, os, shutil, difflib
from syslog_server import SyslogServer

syslog = SyslogServer()

SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")

if len(sys.argv) != 2:
    print("Need one argument!")
    sys.exit(1)
if sys.argv[1] not in ("cpp","dotnet","java","ada"):
    print("Need argument cpp, dotnet, java or ada")
    sys.exit(1)
lang = sys.argv[1]

dependencies = list()

if lang == "cpp":
    command = (os.path.join("cpp", "dots_test_cpp"),)
elif lang == "ada":
    command = (os.path.join("ada", "obj", "dots_test_ada"),)
elif lang == "dotnet":
    command = (os.path.join("dotnet", "dots_test_dotnet.csexe"),)
    dependencies = ("dots_generated-dotnet.dll", "Safir.Dob.Typesystem.dll")
elif lang == "java":
    command = ("java","-jar", os.path.join("java","dots_test_java.jar"))
    dependencies = ("dots_generated-java.jar", "dots_java.jar")
print("Test suite command is '" + " ".join(command) + "'")

for dep in dependencies:
    shutil.copy2(os.path.join(SAFIR_RUNTIME,"bin",dep),
                 lang)

proc = subprocess.Popen(command, stdout = subprocess.PIPE, stderr = subprocess.STDOUT, universal_newlines=True)
res = proc.communicate()[0].replace("\r","").splitlines(1) #fix any DOS newlines

with open("output.txt") as expected_file:
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
    
print("Expected output achieved")
sys.exit(0)
