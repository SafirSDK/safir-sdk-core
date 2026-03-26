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
"""Run tests for logging_java"""
import subprocess
import sys
import argparse
import shutil
import re
import syslog_server
from output import out

parser = argparse.ArgumentParser("test script for logging")
parser.add_argument("--safir-show-config", required=True)
parser.add_argument("--jar", required=True)

arguments = parser.parse_args()

sender_cmd = ("java", "-jar", arguments.jar)

log_server = syslog_server.SyslogServer(arguments.safir_show_config)

o1 = subprocess.check_output(sender_cmd, stderr=subprocess.STDOUT, universal_newlines=True)
o2 = subprocess.check_output(sender_cmd, stderr=subprocess.STDOUT, universal_newlines=True)
o3 = subprocess.check_output(sender_cmd, stderr=subprocess.STDOUT, universal_newlines=True)

syslog_output = log_server.get_data(1)
stdout_output = (o1 + o2 + o3)

#java sometimes outputs some stuff
stdout_output = re.sub(r"Picked up _JAVA_OPTIONS: .*\n", "", stdout_output)


def fail(message):
    """Utility function for printing errors and then exiting"""
    out("Failed! Wrong number of ", message)
    out("STDOUT OUTPUT:")
    out(stdout_output)
    out("SYSLOG OUTPUT:")
    out(syslog_output)
    sys.exit(1)


def extract_facility_severity(syslog_line):
    """Extract facility and severity from a syslog message.

    Syslog messages start with <PRI> where PRI = facility * 8 + severity.
    Returns (facility, severity) tuple or None if not found.
    """
    match = re.match(r'^<(\d+)>', syslog_line)
    if match:
        pri = int(match.group(1))
        facility = pri // 8
        severity = pri % 8
        return (facility, severity)
    return None


def verify_log_message(syslog_output, expected_facility, message_text, expected_severity, expected_count):
    """Verify that a message appears with the correct facility, severity, and count."""
    count = 0
    for line in syslog_output.split('\n'):
        if message_text in line:
            result = extract_facility_severity(line)
            if result is None:
                fail(f"Could not parse PRI from line: {line}")
            facility, severity = result
            if facility != expected_facility:
                fail(f"Wrong facility for '{message_text}': expected {expected_facility}, got {facility}")
            if severity != expected_severity:
                fail(f"Wrong severity for '{message_text}': expected {expected_severity}, got {severity}")
            count += 1

    if count != expected_count:
        fail(f"Wrong count for '{message_text}': expected {expected_count}, got {count}")


# Verify facility (Local0 = 16) and severity for all messages
verify_log_message(syslog_output, 16, "This is an emergency log. Bryn\u00e4s \u00e4r b\u00e4st!\u2620", 0, 3)
verify_log_message(syslog_output, 16, "This is an alert log", 1, 3)
verify_log_message(syslog_output, 16, "This is a critical log", 2, 3)
verify_log_message(syslog_output, 16, "This is an error log", 3, 3)
verify_log_message(syslog_output, 16, "This is a warning log", 4, 3)
verify_log_message(syslog_output, 16, "This is a notice log", 5, 3)
verify_log_message(syslog_output, 16, "This is an informational log", 6, 3)
verify_log_message(syslog_output, 16, "This is a debug log", 7, 3)

# Verify convenience methods also use Local0 facility
verify_log_message(syslog_output, 16, "This is another emergency log using convenience method", 0, 3)
verify_log_message(syslog_output, 16, "This is another alert log using convenience method", 1, 3)
verify_log_message(syslog_output, 16, "This is another critical log using convenience method", 2, 3)
verify_log_message(syslog_output, 16, "This is another error log using convenience method", 3, 3)
verify_log_message(syslog_output, 16, "This is another warning log using convenience method", 4, 3)
verify_log_message(syslog_output, 16, "This is another notice log using convenience method", 5, 3)
verify_log_message(syslog_output, 16, "This is another informational log using convenience method", 6, 3)
verify_log_message(syslog_output, 16, "This is another debug log using convenience method", 7, 3)

if len(stdout_output) != 0:
    fail("Unexpected output on stdout")

out("Found all expected output!")
sys.exit(0)
