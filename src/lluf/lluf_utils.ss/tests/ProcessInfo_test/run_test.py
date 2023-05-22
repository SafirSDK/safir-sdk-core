#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2014, 2023 (http://safirsdkcore.com)
#
# Created by: Anders Widén (anders.widen@consoden.se)
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
import subprocess, os, time, sys
import argparse

def parse_arguments():
    parser = argparse.ArgumentParser(description='unit test script')
    parser.add_argument("--test-exe", help="The test executable", required=True)
    parser.add_argument("--sleeper-exe", help="The sleeper executable", required=True)
    return parser.parse_args()

args = parse_arguments()

#start sleeper
sleeper = subprocess.Popen((args.sleeper_exe, "120"))

listener = subprocess.Popen((args.test_exe, "ProcessInfoSleeper", str(sleeper.pid)),
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT,
                            universal_newlines=True)
result = listener.communicate()[0]
sleeper.kill()

if result != "":
    print(args.test_exe, "exited with non-empty output!")
    print(result)
    sys.exit(1)

print("success")
sys.exit(0)
