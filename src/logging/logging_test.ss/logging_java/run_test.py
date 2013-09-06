#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://www.safirsdk.com)
#
# Created by: Anders Widén <anders.widen@consoden.se>
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
import syslog_server

SAFIR_RUNTIME = os.environ["SAFIR_RUNTIME"]

sender_cmd = ("java",
              "-Xcheck:jni",
              "-Xfuture",
              "-cp", 
              os.path.join(SAFIR_RUNTIME, "bin", "logging_java.jar") +
              os.pathsep +
              "log_sender_java.jar",
              "Sender")

log_server = syslog_server.SyslogServer()

#Run the program that sends system logs
proc = subprocess.Popen(sender_cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,universal_newlines=True)
proc.communicate() # wait until sender program exits
time.sleep(1)

output = log_server.get_data(1)

if "This is an emergency log" in output and \
   "This is an alert log" in output and \
   "This is a critical log" in output and \
   "This is an error log" in output and \
   "This is a warning log" in output and \
   "This is a notice log" in output and \
   "This is an informational log" in output and \
   "This is a debug log" in output:   
    print("Found all expected output!")
    sys.exit(0)
else:
    print("Did not found all expected output!")
    print(output)
    sys.exit(1)    


