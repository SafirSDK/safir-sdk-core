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
import subprocess, os, time, sys, signal, re

sys.path.append("../testutil")
from testenv import TestEnv, TestEnvStopper


if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."
    
sender_path = os.path.join(exe_path,"tracer_sender")

env = TestEnv()
with TestEnvStopper(env):
    subprocess.call(sender_path)
    subprocess.call(sender_path)
    subprocess.call(sender_path)

if not env.ReturnCodesOk():
    print("Some process exited with an unexpected value")
    sys.exit(1)

output = env.Syslog()

if output.count("blahonga") == 15:
    print("Found all expected output!")
    sys.exit(0)
else:
    print("no match! (Received output written failed_test_output.txt.)")
    with open("failed_test_output.txt","w") as expected:
        expected.write(output)

    print(output)
    sys.exit(1)

