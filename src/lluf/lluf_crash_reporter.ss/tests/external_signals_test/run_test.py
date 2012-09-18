#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2012 (http://www.safirsdk.com)
#
# Created by: Lars Hagstrom (lars.hagstrom@consoden.se)
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

import subprocess, os, time, sys, re, signal

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

sleeper_exe = os.path.join(exe_path,"sleeper")


def test_signal(reason, expectCallback = False):
    cf = subprocess.CREATE_NEW_PROCESS_GROUP if sys.platform == "win32" else 0
    sleeper = subprocess.Popen(sleeper_exe,
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.STDOUT,
                               creationflags = cf)
    line = sleeper.stdout.readline()
    if not line.startswith("Started"):
        print "Strange starting line:", line
        sys.exit(1)

    print "Testing signal", str(reason)
    sleeper.send_signal(reason)

    result = sleeper.communicate()[0]
    if (result.find("callback") != -1) != expectCallback:
        print "CrashReporter", "didn't call callback" if expectCallback else "called callback!"
        sys.exit(1)
    if sleeper.returncode != -reason:
        print "Sleeper program exited successfully (it is meant to exit with a signal!), exit code = ", sleeper.returncode
        sys.exit(1)

test_signal(signal.SIGHUP)
test_signal(signal.SIGINT)
test_signal(signal.SIGQUIT)
test_signal(signal.SIGKILL)
test_signal(signal.SIGALRM)
test_signal(signal.SIGTERM)
test_signal(signal.SIGUSR1)
test_signal(signal.SIGUSR2)

test_signal(signal.SIGILL, True)
test_signal(signal.SIGSEGV, True)
test_signal(signal.SIGFPE, True)
test_signal(signal.SIGABRT, True)


print "success"
sys.exit(0)
