#!/usr/bin/env python3
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
import os
import sys
import argparse
from testenv import TestEnv, TestEnvStopper


def log(*args, **kwargs):
    """Logging to stdout with flushing"""
    print(*args, **kwargs)
    sys.stdout.flush()


parser = argparse.ArgumentParser("test script")
parser.add_argument("--safir-control", required=True)
parser.add_argument("--dose_main", required=True)
parser.add_argument("--dope_main", required=True)
parser.add_argument("--safir-websocket", required=True)
parser.add_argument("--websocket-test-client", required=True)
parser.add_argument("--safir-show-config", required=True)

arguments = parser.parse_args()

env = TestEnv(safir_control=arguments.safir_control,
              dose_main=arguments.dose_main,
              dope_main=arguments.dope_main,
              safir_show_config=arguments.safir_show_config)
with TestEnvStopper(env):
    server = env.launchProcess("safir_websocket", arguments.safir_websocket)
    log("Waiting for safir_websocket to start")
    env.WaitForOutput("safir_websocket", "Running ws server on")
    client = env.launchProcess("safir_websocket_test_client", arguments.websocket_test_client, collect_output=False)
    log("Waiting for test client to exit")
    client.wait()
    log("Waiting for safir_websocket to exit")
    server.wait()
    log("Exited, will now exit testenv.")

syslog_output = env.Syslog()
if len(syslog_output) != 0:
    log("Unexpected syslog output:\n" + syslog_output)
    sys.exit(1)

if not env.ReturnCodesOk():
    log("Some process exited with an unexpected value")
    sys.exit(1)

sys.exit(0)
