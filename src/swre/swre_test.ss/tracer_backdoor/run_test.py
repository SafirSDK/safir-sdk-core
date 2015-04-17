#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
import subprocess, os, time, sys, signal, re, argparse
import syslog_server
from safe_print import *
from testenv import TestEnv, TestEnvStopper


parser = argparse.ArgumentParser("test script")
parser.add_argument("--sender", required=True)
parser.add_argument("--backdoor", required=True)
parser.add_argument("--safir-control", required=True)
parser.add_argument("--dose-main", required=True)
parser.add_argument("--dope-main", required=True)
parser.add_argument("--safir-show-config", required=True)
parser.add_argument("--safir-generated-paths", required=True)

arguments = parser.parse_args()

sender_path = arguments.sender

backdoor = arguments.backdoor

#add all the environment variables. passed on format A=10;B=20
for pair in arguments.safir_generated_paths.split(";"):
    (name,value) = pair.split("=")
    print("Setting environment variable", name, "to", value)
    os.environ[name] = value

#in this test we dont check syslog output at all, we trust that it works, since we've tested
#that elsewhere.

def fail(message, output):
    print("Failed!",message)
    print ("OUTPUT:")
    safe_print(output)
    sys.exit(1)


env = TestEnv(safir_control = arguments.safir_control,
              dose_main = arguments.dose_main,
              safir_show_config = arguments.safir_show_config,
              dope_main = arguments.dope_main)
with TestEnvStopper(env):
    env.launchProcess("sender", sender_path)
    output = env.WaitForOutput("sender", "Have logged 10 times")
    if output.count("blahonga") != 0 or output.count("foobar") != 0:
        fail("Unexpected logs before enabling prefixes", output)

    #turn the prefixes on
    subprocess.call((backdoor, "all", "on"))
    env.WaitForOutput("sender", "blahonga, blahonga, blahonga")
    env.WaitForOutput("sender", "foobar")
    #if these outputs don't arrive we'll time out.


    subprocess.call((backdoor, "all", "off"))

    #reset the output so we don't trigger on the output from the start of the test
    env.ResetOutput("sender")
    #this is two of the wcout logs in a row, meaning the tracers are off
    env.WaitForOutput("sender","times.\nHave")


#test turning individual prefix on
env = TestEnv(safir_control = arguments.safir_control,
              dose_main = arguments.dose_main,
              safir_show_config = arguments.safir_show_config,
              dope_main = arguments.dope_main)
with TestEnvStopper(env):
    env.launchProcess("sender", sender_path)
    output = env.WaitForOutput("sender", "Have logged 10 times")
    if output.count("blahonga") != 0 or output.count("foobar") != 0:
        fail("Unexpected logs before enabling prefixes", output)

    #turn the prefixes on
    subprocess.call((backdoor, "Razor", "on"))

    #wait for (at least) two razor outputs
    output = env.WaitForOutput("sender", "foobar")
    env.ResetOutput("sender")
    output += env.WaitForOutput("sender", "foobar")

    #check that we don't have any Rymdbörje output
    if output.count("blahonga") != 0:
        fail("unexpected got logs that should not be on", output)

    subprocess.call((backdoor, "Razor", "off"))

    #no need to reset, since we did it already.
    #this is two of the wcout logs in a row, meaning the tracers are off
    env.WaitForOutput("sender","times.\nHave")



print("success")
sys.exit(0)
