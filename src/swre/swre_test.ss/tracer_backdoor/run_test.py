#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://www.safirsdk.com)
#
# Created by: Lars Hagström / lars.hagstrom@consoden.se
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
import syslog_server
from safe_print import *
from testenv import TestEnv, TestEnvStopper

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."
    
sender_path = os.path.join(exe_path,"tracer_backdoor_sender")

#in this test we dont check syslog output at all, we trust that it works, since we've tested
#that elsewhere.

def fail(message, output):
    print("Failed!",message)
    print ("OUTPUT:")
    safe_print(output)
    sys.exit(1)


env = TestEnv()
with TestEnvStopper(env):
    env.launchProcess("sender", sender_path)
    output = env.WaitForOutput("sender", "Have logged 10 times")
    if output.count("blahonga") != 0 or output.count("foobar") != 0:
        fail("Unexpected logs before enabling prefixes", output)

    #turn the prefixes on
    subprocess.call(("backdoor", "all", "on"))
    env.WaitForOutput("sender", "blahonga, blahonga, blahonga")
    env.WaitForOutput("sender", "foobar")
    #if these outputs don't arrive we'll time out.

    
    subprocess.call(("backdoor", "all", "off"))

    #reset the output so we don't trigger on the output from the start of the test
    env.ResetOutput("sender")
    #this is two of the wcout logs in a row, meaning the tracers are off
    env.WaitForOutput("sender","times.\nHave")


#test turning individual prefix on
env = TestEnv()
with TestEnvStopper(env):
    env.launchProcess("sender", sender_path)
    output = env.WaitForOutput("sender", "Have logged 10 times")
    if output.count("blahonga") != 0 or output.count("foobar") != 0:
        fail("Unexpected logs before enabling prefixes", output)

    #turn the prefixes on
    subprocess.call(("backdoor", "Razor", "on"))

    #wait for (at least) two razor outputs
    output = env.WaitForOutput("sender", "foobar")
    env.ResetOutput("sender")
    output += env.WaitForOutput("sender", "foobar")

    #check that we don't have any Rymdbörje output
    if output.count("blahonga") != 0:
        fail("unexpected got logs that should not be on", output)
    
    subprocess.call(("backdoor", "Razor", "off"))

    #no need to reset, since we did it already.
    #this is two of the wcout logs in a row, meaning the tracers are off
    env.WaitForOutput("sender","times.\nHave")


    
print("success")
sys.exit(0)
