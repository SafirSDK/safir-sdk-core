#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://safirsdkcore.com)
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
"""Run tests for logging_cpp"""
import subprocess
import sys
import argparse
import syslog_server
from safe_print import safe_print

parser = argparse.ArgumentParser("test script for logging")
parser.add_argument("--sender", required=True)
parser.add_argument("--safir-show-config", required=True)

arguments = parser.parse_args()

sender_path = arguments.sender

log_server = syslog_server.SyslogServer(arguments.safir_show_config)

o1 = subprocess.check_output(sender_path, stderr=subprocess.STDOUT)
o2 = subprocess.check_output(sender_path, stderr=subprocess.STDOUT)
o3 = subprocess.check_output(sender_path, stderr=subprocess.STDOUT)

syslog_output = log_server.get_data(1)
stdout_output = (o1 + o2 + o3)


def fail(message):
    """Utility function for printing errors and then exiting"""
    print("Failed! Wrong number of ", message)
    print("STDOUT OUTPUT:")
    safe_print(stdout_output)
    print("SYSLOG OUTPUT:")
    safe_print(syslog_output)
    sys.exit(1)


if syslog_output.count("This is an emergency log. Bryn\u00e4s \u00e4r b\u00e4st!\u2620") != 3:
    fail("Brynas ar bast")

if syslog_output.count("This is an alert log") != 3:
    fail("Alert log")

if syslog_output.count("This is a critical log") != 3:
    fail("Critical log")

if syslog_output.count("This is an error log") != 3:
    fail("Error log")

if syslog_output.count("This is a warning log") != 3:
    fail("Warning log")

if syslog_output.count("This is a notice log") != 3:
    fail("Notice log")

if syslog_output.count("This is an informational log") != 3:
    fail("Informational log")

if syslog_output.count("This is a debug log") != 3:
    fail("Debug log")
    
if syslog_output.count("No c++20 format support") == 3:
    pass
else:
    if syslog_output.count("This is another emergency log. Parameters: 10 and bertil") != 3:
        fail("Brynas ar bast")

    if syslog_output.count("This is another alert log. Parameters: 10 and bertil") != 3:
        fail("Alert log")

    if syslog_output.count("This is another critical log. Parameters: 10 and bertil") != 3:
        fail("Critical log")

    if syslog_output.count("This is another error log. Parameters: 10 and bertil") != 3:
        fail("Error log")

    if syslog_output.count("This is another warning log. Parameters: 10 and bertil") != 3:
        fail("Warning log")

    if syslog_output.count("This is another notice log. Parameters: 10 and bertil") != 3:
        fail("Notice log")

    if syslog_output.count("This is another informational log. Parameters: 10 and bertil") != 3:
        fail("Informational log")

    if syslog_output.count("This is another debug log. Parameters: 10 and bertil") != 3:
        fail("Debug log")

if len(stdout_output) != 0:
    fail("Unexpected output on stdout")

print("Found all expected output!")
sys.exit(0)
