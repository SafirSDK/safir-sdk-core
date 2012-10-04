#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011 (http://www.safirsdk.com)
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
import subprocess, os, time, sys, xml.dom.minidom, shutil

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    libpath = os.environ.get("LD_LIBRARY_PATH")
    os.environ["LD_LIBRARY_PATH"] = libpath + ":" + os.getcwd()
    exe_path = "."


result = subprocess.call(os.path.join(exe_path,"DynamicLibraryLoader_test1"))

if result != 0:
    print ("test1 Failure")
    sys.exit(1)

result = subprocess.call(os.path.join(exe_path,"DynamicLibraryLoader_test2"))


if result != -11 and result != -1073741819: #SIGSEGV and ACCESSVIOL exit codes
    print ("test2 Failure")
    print ("Got returncode", result)
    sys.exit(1)


result = subprocess.call(os.path.join(exe_path,"DynamicLibraryLoader_test3"))

if result != 0:
    print ("test3 Failure")
    sys.exit(1)


result = subprocess.call(os.path.join(exe_path,"DynamicLibraryLoader_test4"))

if result != 0:
    print ("test4 Failure")
    sys.exit(1)

sys.exit(0)
