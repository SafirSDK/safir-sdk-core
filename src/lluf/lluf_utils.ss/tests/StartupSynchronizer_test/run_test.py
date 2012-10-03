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

ss_test = os.path.join(exe_path,"ss_test")
#start first instance
proc1 = subprocess.Popen(ss_test, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
#print proc1.communicate()
time.sleep(0.5) #let the process get the create callback

#start second instance
proc2 = subprocess.Popen(ss_test, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

#wait for them to exit
out1 = proc1.communicate()
out2 = proc2.communicate()

res1 = proc1.returncode
res2 = proc2.returncode

#check exit codes 
#res1 should be "Created" | "Used", and res2 should be "Used" as per bit field in ss_test.cpp
if res1 == 3 and res2 == 2: 
    print("success")
    sys.exit(0)
else:
    print("failure! res1 =", res1, "and res2 =", res2)
    print("Output from instance 1:")
    print(out1[0])
    print("Output from instance 2:")
    print(out2[0])
    sys.exit(1)
