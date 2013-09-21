#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://www.safirsdk.com)
#
# Created by: Lars Hagström / lars.hagstrom@consoden.se
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
import subprocess, os, time, sys, signal, re
import syslog_server
from safe_print import *

#TODO remove this when we drop python 2.6 support
if "check_output" not in dir( subprocess ): # duck punch it in!
    def f(*popenargs, **kwargs):
        if 'stdout' in kwargs:
            raise ValueError('stdout argument not allowed, it will be overridden.')
        process = subprocess.Popen(stdout=subprocess.PIPE, *popenargs, **kwargs)
        output, unused_err = process.communicate()
        retcode = process.poll()
        if retcode:
            cmd = kwargs.get("args")
            if cmd is None:
                cmd = popenargs[0]
            raise subprocess.CalledProcessError(retcode, cmd)
        return output
    subprocess.check_output = f

SAFIR_RUNTIME = os.environ["SAFIR_RUNTIME"]

sender_cmd = ("java",
              "-Xcheck:jni",
              "-Xfuture",
              "-cp", 
              os.path.join(SAFIR_RUNTIME, "bin", "swre_reports_java.jar") +
              os.pathsep +
              "swreport_sender_java.jar",
              "Sender")

syslog = syslog_server.SyslogServer()

o1 = subprocess.check_output(sender_cmd, stderr=subprocess.STDOUT)
o2 = subprocess.check_output(sender_cmd, stderr=subprocess.STDOUT)
o3 = subprocess.check_output(sender_cmd, stderr=subprocess.STDOUT)

#We expect first chars to be CR, if they arent we try to decode it differently...
#this is due to strange windows/java behaviour
encoding = "utf-8"
if bytearray(o1)[0] != ord('C') or bytearray(o1)[1] != ord('R'):
    encoding = "utf-16"

stdout_output = (o1 + o2 + o3).decode(encoding).replace("\r","")
syslog_output = syslog.get_data(1)

def fail(message):
    print("Failed! Wrong number of",message)
    print ("STDOUT OUTPUT:")
    safe_print(stdout_output)
    print ("SYSLOG OUTPUT:")
    safe_print(syslog_output)
    sys.exit(1)

if stdout_output.count("\n") != 18 or syslog_output.count("\n") != 24:
    fail("lines")

if stdout_output.count(u"CRITICAL: FatalError FatalError|here|Testing SendFatalErrorReport") != 3 or syslog_output.count(u"FatalError FatalError|here|Testing SendFatalErrorReport") != 3:
    fail("SendFatalErrorReport")

if stdout_output.count(u"ERROR: Error Error|there|Testing SendErrorReport") != 3 or syslog_output.count(u"Error Error|there|Testing SendErrorReport") != 3:
    fail("SendErrorReport")

if stdout_output.count(u"SendResourceReport") != 3 or syslog_output.count(u"SendResourceReport") != 6:
    fail("SendResourceReport")

if stdout_output.count(u"Resource ResourceReport is allocated") != 0 or syslog_output.count(u"Resource ResourceReport is allocated") != 3:
    fail("Resource ResourceReport is allocated")

if stdout_output.count(u"Resource ResourceReport is not allocated") != 3 or syslog_output.count(u"Resource ResourceReport is not allocated") != 3:
    fail("Resource ResourceReport is not allocated")

if stdout_output.count(u"CRITICAL: ProgrammingError ProgrammingError|everywhere|Testing SendProgrammingErrorReport") != 3 or syslog_output.count(u"ProgrammingError ProgrammingError|everywhere|Testing SendProgrammingErrorReport") != 3:
    fail("SendProgrammingErrorReport")

if stdout_output.count(u"SendProgramInfoReport") != 0 or syslog_output.count(u"SendProgramInfoReport") != 3:
    fail("SendProgramInfoReport")

if stdout_output.count(u"brynanuppafj@ssasponken|Don't know@|Testing\nfunny characters") != 3 or syslog_output.count(u"brynanuppafjässasponken|Don't know\u203d|Testing funny characters") != 3:
    fail("brynanuppa")

print("success")
sys.exit(0)
