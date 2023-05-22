#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2014 (http://safirsdkcore.com)
#
# Created by: Anders Widén
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
import socket
import shutil
import re
import argparse

def parse_arguments():
    parser = argparse.ArgumentParser(description='unit test script')
    parser.add_argument("test_conf_dir", help="Test configuration directory")
    parser.add_argument("--test-exe", help="The test executable", required=True)
    return parser.parse_args()

args = parse_arguments()

def run_test(test_case):
    proc = subprocess.Popen((args.test_exe, test_case),
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT,
                            universal_newlines=True)
    stdout, stderr = proc.communicate()

    if proc.returncode != 0:
        print("Test failed! " + test_case)
        print(stdout)
        sys.exit(1)


os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = args.test_conf_dir

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc1")
run_test("tc1")

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc2")
run_test("tc2")

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc3")
run_test("tc3")

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc4")
run_test("tc4")

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc5")
run_test("tc5")

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc6")
run_test("tc6")

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc7")
run_test("tc7")

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc8")
run_test("tc8")

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc9")
run_test("tc9")

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc10")
run_test("tc10")


print("Success!")
sys.exit(0)
