#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://safirsdkcore.com)
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
import subprocess, os, time, sys, signal, re, argparse
import syslog_server
from safe_print import *

parser = argparse.ArgumentParser("test script")
parser.add_argument("--binary", required=True)
parser.add_argument("--safir-show-config", required=True)

arguments = parser.parse_args()

sender_path = arguments.binary

syslog = syslog_server.SyslogServer(arguments.safir_show_config)

o1 = subprocess.check_output((sender_path, "enable"))
o2 = subprocess.check_output((sender_path, "enable"))
o3 = subprocess.check_output((sender_path, "enable"))

stdout_output = (o1 + o2 + o3).decode("utf-8").replace("\r","")
syslog_output = syslog.get_data(1)

def fail(message):
    print("Failed! Wrong number of",message)
    print ("STDOUT OUTPUT:")
    safe_print(stdout_output)
    print ("SYSLOG OUTPUT:")
    safe_print(syslog_output)
    sys.exit(1)

if stdout_output.count("\n") != 36 or syslog_output.count("\n") != 36:
    fail("lines")

if stdout_output.count(u"Rymd-B@rje: blahonga") != 6 or syslog_output.count(u"Rymd-Börje: blahonga") != 6:
    fail("blahonga")

if stdout_output.count(u"Rymd-B@rje: blahonga\n") != 3 or syslog_output.count(u"Rymd-Börje: blahonga\n") != 3:
    fail("blahonga newlines")

if stdout_output.count(u"Razor: brynanuppafj@ssasponken\n") != 3 or syslog_output.count(u"Razor: brynanuppafjässasponken\n") != 3:
    fail("brynanuppa")

if stdout_output.count(u"Rymd-B@rje: blahong@a\n") != 3 or syslog_output.count(u"Rymd-Börje: blahong®a\n") != 3:
    fail("blahong®a")

if stdout_output.count(u"Rymd-B@rje: blahonga@@@\n") != 3 or syslog_output.count(u"Rymd-Börje: blahongaåäö\n") != 3:
    fail("åäö")

if stdout_output.count(u"Razor: 123.1\n") != 3 or syslog_output.count(u"Razor: 123.1\n") != 3:
    fail("123.1")

if stdout_output.count(u"Razor: foobar\n") != 3 or syslog_output.count(u"Razor: foobar\n") != 3:
    fail("foobar")

if stdout_output.count(u"Razor: this is the end\n") != 3 or syslog_output.count(u"Razor: this is the end\n") != 3:
    fail("this is the end")

if stdout_output.count(u"Razor: my only friend, the end\n") != 3 or syslog_output.count(u"Razor: my only friend, the end\n") != 3:
    fail("my only friend, the end")

if stdout_output.count(u"the end\nRymd-B@rje: of our elaborate plans\n") != 3:
    fail("elaborate plans")

if stdout_output.count(u"crossbones: @\n") != 3 or syslog_output.count(u"crossbones: \u2620\n") != 3:
    fail("crossbones")

if stdout_output.count(u"interrobang: @\n") != 3 or syslog_output.count(u"interrobang: \u203d\n") != 3:
    fail("interrobang")

if stdout_output.count(u"@reversed\n") != 3 or syslog_output.count(u"\u202ereversed\n") != 3:
    fail("reversed")


#check that there is no output when we don't "enable"
stdout_output = subprocess.check_output(sender_path).decode("utf-8").replace("\r","")
syslog_output = syslog.get_data(1)

if stdout_output.count("\n") != 0 or syslog_output.count("\n") != 0:
    fail("empty")

#check that FORCE_LOG all works
os.environ["FORCE_LOG"] = "all"
stdout_output = subprocess.check_output(sender_path).decode("utf-8").replace("\r","")
syslog_output = syslog.get_data(1)

if stdout_output.count("\n") != 12 or syslog_output.count("\n") != 12:
    fail("all lines")

#check that FORCE_LOG works
os.environ["FORCE_LOG"] = "Razor"
#check that there is no output when we don't "enable"
stdout_output = subprocess.check_output(sender_path).decode("utf-8").replace("\r","")
syslog_output = syslog.get_data(1)

if stdout_output.count("\n") != 6 or syslog_output.count("\n") != 6:
    fail("Razor lines")

if stdout_output.count(u"Razor: ") != 6 or syslog_output.count(u"Razor: ") != 6:
    fail("Razor")

if stdout_output.count(u"Rymd-B@rje: ") != 0 or syslog_output.count(u"Rymd-Börje: ") != 0:
    fail("Rymd-Borje")

print("success")
sys.exit(0)
