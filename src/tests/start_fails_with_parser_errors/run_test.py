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
import subprocess
import sys
import argparse
from testenv import log

def parse_arguments():
    parser = argparse.ArgumentParser("test script")
    parser.add_argument("--safir-control", required=True)
    parser.add_argument("--dose_main", required=True)
    parser.add_argument("--dots_configuration_check", required=True)

    arguments = parser.parse_args()

    return arguments

def main():
    args = parse_arguments()
    fails = 0
    try:
        output = subprocess.check_output(args.dots_configuration_check,
                                         stderr=subprocess.STDOUT,
                                         text=True)
        log("ERROR: dots_configuration_check exited successfully even though it should fail! Here is the output:")
        log(output)
        fails += 1
    except subprocess.CalledProcessError as exc:
        log("dots_configuration_check exited with an error, as expected.")
        if "Unexpected Element" not in exc.output:
            fails += 1
            log("Expected error is not in output:")
            log(exc.output)

    try:
        subprocess.check_output((args.safir_control,"--dose-main-path", args.dose_main),
                                stderr=subprocess.STDOUT,
                                text=True)
        fails+=1
        #we will never get here on failure, instead the test will time out...
    except subprocess.CalledProcessError as exc:
        log("safir_control exited with an error, as expected.")
        if "Unexpected Element" not in exc.output:
            fails += 1
            log("Expected error is not in output:")
            log(exc.output)
    return fails


sys.exit(main())
