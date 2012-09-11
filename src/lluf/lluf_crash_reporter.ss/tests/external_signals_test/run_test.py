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


def run_sleeper(reason):
    sleeper = subprocess.Popen(sleeper_exe,
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    print "Testing signal", str(reason)
    os.kill(sleeper.pid, reason)

    result = sleeper.communicate()[0]
    if result.find("callback") != -1:
        print "CrashReporter called callback!"
        sys.exit(1)
    if sleeper.returncode != -reason:
        print "Sleeper program exited successfully (it is meant to exit with a signal!), exit code = ", sleeper.returncode
        sys.exit(1)

run_sleeper(signal.SIGHUP)
run_sleeper(signal.SIGINT)
run_sleeper(signal.SIGQUIT)
run_sleeper(signal.SIGKILL)
run_sleeper(signal.SIGALRM)
run_sleeper(signal.SIGTERM)
run_sleeper(signal.SIGUSR1)
run_sleeper(signal.SIGUSR2)


print "success"
sys.exit(0)
