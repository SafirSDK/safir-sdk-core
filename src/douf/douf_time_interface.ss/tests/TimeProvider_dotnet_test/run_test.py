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
import subprocess, sys, os, shutil, time

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

tester_path_base = os.path.join(exe_path,"dotnet_tester")
tester_csexe = tester_path_base+".csexe"
tester_exe = tester_path_base+".exe"
shutil.copy2(tester_csexe,tester_exe)

SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
dependencies = ("Safir.Dob.Typesystem.dll",
                "Safir.Time.dll",)

for dep in dependencies:
    shutil.copy2(os.path.join(SAFIR_RUNTIME,"bin",dep),
                 ".")

result = subprocess.call(tester_exe)

time.sleep(0.1)

os.remove(tester_exe)
for dep in dependencies:
    os.remove(dep)

print("Result =", result)

sys.exit(result)
