#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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
from __future__ import print_function
import subprocess, os, time, sys
try:
    import ConfigParser
except ImportError:
    import configparser as ConfigParser
import socket
import shutil
import re
import argparse

def run_test(test_case):
    proc = subprocess.Popen((test_path, test_case), stdout=subprocess.PIPE, stderr=subprocess.STDOUT,universal_newlines=True)
    stdout, stderr = proc.communicate()

    if proc.returncode != 0:
        print("Test failed!")
        print (stdout)
        sys.exit(1)
    
parser = argparse.ArgumentParser()
parser.add_argument("test_conf_dir", help="Test configuration directory")
args = parser.parse_args()

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

test_pgm = "Config_test"  
test_path = os.path.join(exe_path, test_pgm)

os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = args.test_conf_dir

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc1")
run_test("tc1")    

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc2")
run_test("tc2")

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc3")
run_test("tc3")

os.environ["DOU_TEST_DIR"] = os.path.join(args.test_conf_dir, "tc4")
run_test("tc4")

print ("Success!")
sys.exit(0)
