#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
import subprocess, os, time, sys, signal, re, argparse
import syslog_server
from safe_print import *

parser = argparse.ArgumentParser("test script")
parser.add_argument("--binary", required=True)
parser.add_argument("--safir-show-config", required=True)

arguments = parser.parse_args()

sender_path = arguments.binary

syslog = syslog_server.SyslogServer(arguments.safir_show_config)

o1 = subprocess.check_output(sender_path, stderr=subprocess.STDOUT)
o2 = subprocess.check_output(sender_path, stderr=subprocess.STDOUT)
o3 = subprocess.check_output(sender_path, stderr=subprocess.STDOUT)

stdout_output = o1 + o2 + o3
syslog_output = syslog.get_data(1)

def fail(message):
    print("Failed! Wrong number of",message)
    print ("STDOUT OUTPUT:")
    safe_print(stdout_output)
    print ("SYSLOG OUTPUT:")
    safe_print(syslog_output)
    sys.exit(1)

if syslog_output.count("\n") != 24:
    fail("lines")

if syslog_output.count(u"FatalError FatalError|here|Testing SendFatalErrorReport") != 3:
    fail("SendFatalErrorReport")

if syslog_output.count(u"Error Error|there|Testing SendErrorReport") != 3:
    fail("SendErrorReport")

if syslog_output.count(u"SendResourceReport") != 6:
    fail("SendResourceReport")

if syslog_output.count(u"Resource ResourceReport is allocated") != 3:
    fail("Resource ResourceReport is allocated")

if syslog_output.count(u"Resource ResourceReport is not allocated") != 3:
    fail("Resource ResourceReport is not allocated")

if syslog_output.count(u"ProgrammingError ProgrammingError|everywhere|Testing SendProgrammingErrorReport") != 3:
    fail("SendProgrammingErrorReport")

if syslog_output.count(u"SendProgramInfoReport") != 3:
    fail("SendProgramInfoReport")

if syslog_output.count(u"brynanuppafjässasponken|Don't know\u203d|Testing funny characters") != 3:
    fail("brynanuppa")


if len(stdout_output) != 0:
    fail("Unexpected output on stdout")

print("success")
sys.exit(0)
