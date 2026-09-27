#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011-2013, 2026 (http://safirsdkcore.com)
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
import subprocess, os, time, sys, xml.dom.minidom, shutil, argparse

parser = argparse.ArgumentParser("test script")
parser.add_argument("--test-exe-1", required=True)
parser.add_argument("--test-exe-2", required=True)
parser.add_argument("--test-exe-3", required=True)
parser.add_argument("--test-exe-4", required=True)

arguments = parser.parse_args()

# These children are meant to die of a crash signal. AddressSanitizer and
# ThreadSanitizer install their own handlers for those and turn the death into exit
# code 1 or 66 with a report, which is not what is under test here, so tell them to
# leave the crash signals alone. In a build without sanitizers the variables are
# simply ignored.
child_env = dict(os.environ)
for sanitizer_options in ("ASAN_OPTIONS", "TSAN_OPTIONS"):
    child_env[sanitizer_options] = ":".join(
        filter(None, [child_env.get(sanitizer_options), "handle_segv=0:handle_sigfpe=0:handle_sigill=0:handle_abort=0"]))

result = subprocess.call(arguments.test_exe_1, env=child_env)

if result != 0:
    print("test1 Failure")
    sys.exit(1)

result = subprocess.call(arguments.test_exe_2, env=child_env)

if result != -11 and result != 1234:  #SIGSEGV and ACCESSVIOL exit codes
    print("test2 Failure")
    print("Got returncode", result)
    sys.exit(1)

result = subprocess.call(arguments.test_exe_3, env=child_env)

if result != 0:
    print("test3 Failure")
    sys.exit(1)

result = subprocess.call(arguments.test_exe_4, env=child_env)

if result != 0:
    print("test4 Failure")
    sys.exit(1)

sys.exit(0)
