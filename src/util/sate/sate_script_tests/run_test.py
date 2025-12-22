#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011-2025 (http://safirsdkcore.com)
#
# Created by: Joel Ottosson (joel.ottosson@gmail.com)
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
import os, sys, argparse, socket, glob, logging, uuid, time
from testenv import TestEnv, TestEnvStopper

def log(*args, **kwargs):
    """Logging to stdout with flushing"""
    print(*args, **kwargs)
    sys.stdout.flush()

log_dir = os.path.normpath(os.path.join(os.getcwd(), "sate_output"))
for f in glob.glob(os.path.join(log_dir, "*")): os.remove(f)
os.environ["LLL_LOGDIR"] = log_dir

parser = argparse.ArgumentParser("test script")
parser.add_argument("--safir-control", required=True)
parser.add_argument("--dose_main", required=True)
parser.add_argument("--dope_main", required=True)
parser.add_argument("--sate", required=True)
parser.add_argument("--script1", required=True)
parser.add_argument("--script2", required=True)
parser.add_argument("--safir-show-config", required=True)
arguments = parser.parse_args()

env = TestEnv(safir_control=arguments.safir_control,
              dose_main=arguments.dose_main,
              dope_main=arguments.dope_main,
              safir_show_config=arguments.safir_show_config)
with TestEnvStopper(env):
    sate1 = env.launchProcess("sate1", [arguments.sate, "-s", arguments.script1, "-c", "sate1", "-v"], collect_output=True)    
    sate2 = env.launchProcess("sate2", [arguments.sate, "-s", arguments.script2, "-c", "sate2", "-v"], collect_output=True)
    
    log("Wait for sate 1")
    sate1.wait()
    log("Wait for sate 2")
    sate2.wait()

    out1 = env.Output("sate1")
    out2 = env.Output("sate2")

    expected1 = '''
Opening DOB connection: sate1
Starting script execution...
Register entity handler: Safir.Control.Status, handler=DEFAULT_HANDLER, RequestorDecidesInstanceId
Connected to DOB!
Register service handler: Safir.Control.Command, handler=321
Set all: Safir.Control.Status, instance=1, handler=DEFAULT_HANDLER
Set all: Safir.Control.Status, instance=2, handler=DEFAULT_HANDLER
Set all: Safir.Control.Status, instance=3, handler=DEFAULT_HANDLER
Send message: Safir.Application.BackdoorCommand, channel=123
OnServiceRequest: Safir.Control.Command
Script execution completed successfully.
Disconnected from DOB!
'''

    expected2 = '''
Opening DOB connection: sate2
Starting script execution...
Subscribe entity: Safir.Control.Status, recursive=1
Connected to DOB!
Subscribe message: Safir.Application.BackdoorCommand, channel=123, recursive=1
OnEntity: New Safir.Control.Status, instance=1
OnEntity: New Safir.Control.Status, instance=2
OnEntity: New Safir.Control.Status, instance=3
OnMessage: Safir.Application.BackdoorCommand on channel 123
Send service request: Safir.Control.Command, handler=321
OnResponse: Safir.Dob.SuccessResponse
OnEntity: Delete Safir.Control.Status, instance=1
OnEntity: Delete Safir.Control.Status, instance=2
OnEntity: Delete Safir.Control.Status, instance=3
Script execution completed successfully.
Disconnected from DOB!
'''

    if out1.find(expected1.strip()) == -1:
        log("SATE1 output did not match expected output! Output was:")
        log(out1)
        sys.exit(1)

    if out2.find(expected2.strip()) == -1:
        log("SATE2 output did not match expected output! Output was:")
        log(out2)
        sys.exit(1)

    log("Exited, will now exit testenv.")

sys.exit(0)
