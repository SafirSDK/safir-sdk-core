#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
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
import subprocess, os, time, sys, xml.dom.minidom, shutil, argparse

parser = argparse.ArgumentParser("test script for LowLevelLogger")
parser.add_argument("--test-exe-1", required=True)
parser.add_argument("--test-exe-2", required=True)
parser.add_argument("--test-exe-3", required=True)
parser.add_argument("--test-exe-4", required=True)

arguments = parser.parse_args()

result = subprocess.call(arguments.test_exe_1)

if result != 0:
    print ("test1 Failure")
    sys.exit(1)

result = subprocess.call(arguments.test_exe_2)


if result != -11 and result != 1234: #SIGSEGV and ACCESSVIOL exit codes
    print ("test2 Failure")
    print ("Got returncode", result)
    sys.exit(1)


result = subprocess.call(arguments.test_exe_3)

if result != 0:
    print ("test3 Failure")
    sys.exit(1)


result = subprocess.call(arguments.test_exe_4)

if result != 0:
    print ("test4 Failure")
    sys.exit(1)

sys.exit(0)
