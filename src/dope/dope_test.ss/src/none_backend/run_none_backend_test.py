#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
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
import subprocess, os, time, sys, shutil, glob, argparse, re
from testenv import TestEnv, TestEnvStopper

def log(data):
    print(data)
    sys.stdout.flush()

env = None

try:
    parser = argparse.ArgumentParser("test script")
    parser.add_argument("--safir-show-config", required=True)
    parser.add_argument("--safir-control", required=True)
    parser.add_argument("--dose-main", required=True)
    parser.add_argument("--dope-main", required=True)
    parser.add_argument("--entity-owner", required=True)
    parser.add_argument("--safir-generated-paths", required=True)

    arguments = parser.parse_args()


    #add all the environment variables. passed on format A=10;B=20
    for pair in arguments.safir_generated_paths.split(";"):
        (name,value) = pair.split("=")
        print("Setting environment variable", name, "to", value)
        os.environ[name] = value


    """
    log("Find out how many entities entity_owner will set")
    num_str = subprocess.check_output((arguments.entity_owner,"num"), universal_newlines=True)
    NUM_SMALL = int(re.search(r"NUM_SMALL = ([0-9]+)",num_str).group(1))
    NUM_BIG = int(re.search(r"NUM_BIG = ([0-9]+)",num_str).group(1))
    log("NUM_SMALL = " + str(NUM_SMALL) + " and NUM_BIG = " + str(NUM_BIG))
    """

    log("Set a bunch of entities")
    global env = TestEnv(arguments.safir_control,
                         arguments.dose_main,
                         arguments.dope_main,
                         arguments.safir_show_config)
    with TestEnvStopper(env):
        env.launchProcess("entity_owner", (arguments.entity_owner,"set")).wait()
        env.WaitForOutput("entity_owner", "Exiting")

    syslog_output = env.Syslog()
    if len(syslog_output) != 0:
        log("Unexpected syslog output:\n" + syslog_output)
        sys.exit(1)

    if not env.ReturnCodesOk():
        log("Some process exited with an unexpected value")
        sys.exit(1)

    log("See if dope loads them at startup")
    global env = TestEnv(arguments.safir_control,
                         arguments.dose_main,
                         arguments.dope_main,
                         arguments.safir_show_config)
    with TestEnvStopper(env):
        env.launchProcess("entity_owner", (arguments.entity_owner,"accept")).wait()

    syslog_output = env.Syslog()
    if len(syslog_output) != 0:
        log("Unexpected syslog output:\n" + syslog_output)
        sys.exit(1)

    if not env.ReturnCodesOk():
        log("Some process exited with an unexpected value")
        sys.exit(1)

    output = env.Output("entity_owner")
    if output.count("OnInjectedNewEntity") != 0:
        log("could not find the right number of 'OnInjectedNewEntity' in output")
        sys.exit(1)

    if output.count("<DopeTest.SmallEntity>") != 0:
        log("could not find the right number of 'DopeTest.SmallEntity' in output")
        sys.exit(1)

    if output.count("<DopeTest.BigEntity>") != 0:
        log("could not find the right number of 'DopeTest.BigEntity' in output")
        sys.exit(1)

except:
    print("Unexpected exception!")
    if env is not None:
        syslog_output = env.Syslog()
        if len(syslog_output) != 0:
            print("syslog output:\n" + syslog_output)
    raise

log("Success")
sys.exit(0)
