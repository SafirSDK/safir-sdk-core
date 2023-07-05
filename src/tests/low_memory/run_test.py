#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2023 (http://safirsdkcore.com)
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
import time
import sys
import argparse
from testenv import TestEnv, TestEnvStopper

parser = argparse.ArgumentParser("test script")
parser.add_argument("--memory_eater", required=True)
parser.add_argument("--safir-control", required=True)
parser.add_argument("--dose_main", required=True)
parser.add_argument("--dope_main", required=True)
parser.add_argument("--dobexplorer", required=True)
parser.add_argument("--safir-show-config", required=True)

arguments = parser.parse_args()

env = TestEnv(safir_control=arguments.safir_control,
              dose_main=arguments.dose_main,
              dope_main=arguments.dope_main,
              safir_show_config=arguments.safir_show_config)

with TestEnvStopper(env):
    #env.launchProcess("dobexplorer", arguments.dobexplorer)
    #time.sleep(10)
    eater = env.launchProcess("memory_eater", arguments.memory_eater)
    env.WaitForOutput("memory_eater", "Done")


if not env.ReturnCodesOk():
    print("Some process exited with an unexpected value")
    sys.exit(1)

sys.exit(0)
