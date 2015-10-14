#!/usr/bin/env python
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
from __future__ import print_function
import subprocess, sys, os, shutil, time, argparse

parser = argparse.ArgumentParser("test script")
parser.add_argument("--test-exe", required=True)
parser.add_argument("--dependencies", required=True)

arguments = parser.parse_args()

dependencies = arguments.dependencies.split(",")

for dep in dependencies:
    shutil.copy2(dep,
                 ".")

result = subprocess.call(arguments.test_exe)

print("Result =", result)

sys.exit(result)
