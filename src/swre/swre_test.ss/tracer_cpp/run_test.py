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
from output import out

parser = argparse.ArgumentParser("test script")
parser.add_argument("--binary", required=True)
parser.add_argument("--safir-show-config", required=True)

arguments = parser.parse_args()

sender_path = arguments.binary

syslog = syslog_server.SyslogServer(arguments.safir_show_config)

o1 = subprocess.check_output((sender_path, "enable"))
o2 = subprocess.check_output((sender_path, "enable"))
o3 = subprocess.check_output((sender_path, "enable"))

stdout_output = (o1 + o2 + o3).decode("utf-8").replace("\r", "")
syslog_output = syslog.get_data(1)


def fail(message):
    out("Failed! Wrong number of", message)
    out("STDOUT OUTPUT:")
    out(stdout_output)
    out("SYSLOG OUTPUT:")
    out(syslog_output)
    sys.exit(1)


if stdout_output.count("\n") != 39 or syslog_output.count("\n") != 39:
    fail("lines")

# Check that all tracer syslog messages use Local1 facility (17)
# Tracer uses Debug severity (7), so PRI = 17 * 8 + 7 = 143
if len(syslog.facilities) != 39:
    out("Failed! Expected 39 syslog messages with parsed facilities, got", len(syslog.facilities))
    out("Facilities:", syslog.facilities)
    sys.exit(1)

for facility in syslog.facilities:
    if facility != 17:  # Local1
        out("Failed! Expected all tracer messages to use Local1 facility (17), but got facility", facility)
        out("All facilities:", syslog.facilities)
        sys.exit(1)

if stdout_output.count(u"Rymd-B@rje: blahonga") != 6 or syslog_output.count(u"Rymd-Börje: blahonga") != 6:
    fail("blahonga")

if stdout_output.count(u"Rymd-B@rje: blahonga\n") != 3 or syslog_output.count(u"Rymd-Börje: blahonga\n") != 3:
    fail("blahonga newlines")

if stdout_output.count(u"Razor: brynanuppafj@ssasponken\n") != 3 or syslog_output.count(
        u"Razor: brynanuppafjässasponken\n") != 3:
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

if stdout_output.count(u"Razor: my only friend, the end\n") != 3 or syslog_output.count(
        u"Razor: my only friend, the end\n") != 3:
    fail("my only friend, the end")

if stdout_output.count(u"the end\nRymd-B@rje: of our elaborate plans\n") != 3:
    fail("elaborate plans")

if stdout_output.count(u"crossbones: @\n") != 3 or syslog_output.count(u"crossbones: \u2620\n") != 3:
    fail("crossbones")

if stdout_output.count(u"interrobang: @\n") != 3 or syslog_output.count(u"interrobang: \u203d\n") != 3:
    fail("interrobang")

if stdout_output.count(u"@reversed\n") != 3 or syslog_output.count(u"\u202ereversed\n") != 3:
    fail("reversed")

if stdout_output.count("No c++20 format support") == 3:
    pass
else:
    if stdout_output.count("I l0ve the smell of std::format in the morning, it smells like v1ct0ry.") != 3:
        fail("Napalm")

#check that there is no output when we don't "enable"
stdout_output = subprocess.check_output(sender_path).decode("utf-8").replace("\r", "")
syslog_output = syslog.get_data(1)

if stdout_output.count("\n") != 0 or syslog_output.count("\n") != 0:
    fail("empty")

#check that FORCE_LOG all works
os.environ["FORCE_LOG"] = "all"
stdout_output = subprocess.check_output(sender_path).decode("utf-8").replace("\r", "")
syslog_output = syslog.get_data(1)

if stdout_output.count("\n") != 13 or syslog_output.count("\n") != 13:
    fail("all lines")

# Check facility for FORCE_LOG all
for facility in syslog.facilities:
    if facility != 17:  # Local1
        out("Failed! FORCE_LOG all: Expected facility Local1 (17), but got", facility)
        sys.exit(1)

#check that FORCE_LOG works
os.environ["FORCE_LOG"] = "Razor"
#check that there is no output when we don't "enable"
stdout_output = subprocess.check_output(sender_path).decode("utf-8").replace("\r", "")
syslog_output = syslog.get_data(1)

if stdout_output.count("\n") != 7 or syslog_output.count("\n") != 7:
    fail("Razor lines")

if stdout_output.count(u"Razor: ") != 7 or syslog_output.count(u"Razor: ") != 7:
    fail("Razor")

if stdout_output.count(u"Rymd-B@rje: ") != 0 or syslog_output.count(u"Rymd-Börje: ") != 0:
    fail("Rymd-Borje")

# Check facility for FORCE_LOG Razor
for facility in syslog.facilities:
    if facility != 17:  # Local1
        out("Failed! FORCE_LOG Razor: Expected facility Local1 (17), but got", facility)
        sys.exit(1)

out("success")
sys.exit(0)
