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
from testenv import TestEnv, TestEnvStopper

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."
    
stoppee_path = os.path.join(exe_path,"stoppee")


env = TestEnv()
with TestEnvStopper(env):
    for i in range(95):
        env.launchProcess("stoppee_" + str(i),stoppee_path)
    while True:
        print("checking if all have started yet")
        done = True
        for i in range(95):
            if env.Output("stoppee_" + str(i)).find("Connected") == -1:
                print("Found one that has not started yet (" + str(i) + "), will keep checking")
                done = False
                break
        if done:
            break;
        time.sleep(1)

    
if not env.ReturnCodesOk():
    print("Some process exited with an unexpected value")
    sys.exit(1)

sys.exit(0)
