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
from testenv import TestEnv, TestEnvStopper, swre_logger_cmd

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."
    
sender_path = os.path.join(exe_path,"send_something")


env = TestEnv()
with TestEnvStopper(env):
    for i in range(95):
        env.launchProcess("swre_logger_" + str(i),swre_logger_cmd)
    while True:
        print("checking if all have started yet")
        subprocess.call(sender_path)
        done = True
        for i in range(95):
            if env.Output("swre_logger_" + str(i)).find("Fatal") == -1:
                print("Found one that has not started yet (" + str(i) + ")")
                done = False
                break
        if done:
            break;
        time.sleep(1)

    
if not env.ReturnCodesOk():
    print("Some process exited with an unexpected value")
    sys.exit(1)

sys.exit(0)
