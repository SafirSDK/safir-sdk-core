#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2025 (http://safirsdkcore.com)
#
# Created by: Lars Hagstrom
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
"""Test that syslog messages are forwarded to tracer output when a prefix is enabled."""

import subprocess
import sys
import argparse

parser = argparse.ArgumentParser("test script")
parser.add_argument("--binary", required=True)

arguments = parser.parse_args()

sender_path = arguments.binary


def fail(message):
    print("FAILED:", message)
    sys.exit(1)


# Test 1: With prefix enabled, syslog messages should appear in stdout
print("Test 1: Syslog messages should appear when prefix is enabled")
output = subprocess.check_output((sender_path, "enable")).decode("utf-8").replace("\r", "")
print("Output:")
print(output)

# Check that tracer output is present
if "TestPrefix: before syslog" not in output:
    fail("Missing 'TestPrefix: before syslog' in output")

if "TestPrefix: after syslog" not in output:
    fail("Missing 'TestPrefix: after syslog' in output")

# Check that syslog messages are forwarded with correct format
if "syslog: [ERROR] This is an error message" not in output:
    fail("Missing forwarded ERROR syslog message")

if "syslog: [WARNING] This is a warning message" not in output:
    fail("Missing forwarded WARNING syslog message")

if "syslog: [CRITICAL] This is a critical message" not in output:
    fail("Missing forwarded CRITICAL syslog message")

# Check ordering: syslog messages should appear between "before" and "after"
before_pos = output.find("TestPrefix: before syslog")
after_pos = output.find("TestPrefix: after syslog")
error_pos = output.find("syslog: [ERROR]")
warning_pos = output.find("syslog: [WARNING]")
critical_pos = output.find("syslog: [CRITICAL]")

if not (before_pos < error_pos < after_pos):
    fail("ERROR syslog message not in expected position")

if not (before_pos < warning_pos < after_pos):
    fail("WARNING syslog message not in expected position")

if not (before_pos < critical_pos < after_pos):
    fail("CRITICAL syslog message not in expected position")

print("Test 1 passed!")

# Test 2: Without prefix enabled, syslog messages should NOT appear in stdout
print("\nTest 2: Syslog messages should NOT appear when no prefix is enabled")
output = subprocess.check_output(sender_path).decode("utf-8").replace("\r", "")
print("Output:")
print(output)

# There should be no tracer output at all (prefix not enabled)
if "TestPrefix:" in output:
    fail("Tracer output should not appear when prefix is disabled")

# There should be no forwarded syslog messages
if "syslog:" in output:
    fail("Syslog messages should not be forwarded when no prefix is enabled")

# Output should be empty
if output.strip() != "":
    fail("Expected empty output when prefix is disabled, got: " + repr(output))

print("Test 2 passed!")

# Test 3: Mid-line syslog insertion should cleanly separate output
print("\nTest 3: Mid-line syslog insertion should insert newline first")
output = subprocess.check_output((sender_path, "midline")).decode("utf-8").replace("\r", "")
print("Output:")
print(output)

# The output should be:
# TestPrefix: partial
# syslog: [ERROR] midline error
# TestPrefix: after midline

# Check that partial line is present
if "TestPrefix: partial" not in output:
    fail("Missing 'TestPrefix: partial' in output")

# Check that syslog message is present
if "syslog: [ERROR] midline error" not in output:
    fail("Missing forwarded midline ERROR syslog message")

# Check that continuation line has prefix (proving m_prefixPendingTracer was set)
if "TestPrefix: after midline" not in output:
    fail("Missing 'TestPrefix: after midline' in output")

# Verify the partial line ends with newline (syslog inserted one)
# and that syslog message is on its own line
lines = output.strip().split("\n")
if len(lines) != 3:
    fail(f"Expected 3 lines, got {len(lines)}: {lines}")

if lines[0] != "TestPrefix: partial":
    fail(f"Line 0 should be 'TestPrefix: partial', got: {lines[0]}")

if lines[1] != "syslog: [ERROR] midline error":
    fail(f"Line 1 should be 'syslog: [ERROR] midline error', got: {lines[1]}")

if lines[2] != "TestPrefix: after midline":
    fail(f"Line 2 should be 'TestPrefix: after midline', got: {lines[2]}")

print("Test 3 passed!")

# Test 4: Messages using tracer's own facility should NOT be forwarded
print("\nTest 4: Syslog messages using tracer facility should NOT be forwarded")
output = subprocess.check_output((sender_path, "tracer_facility")).decode("utf-8").replace("\r", "")
print("Output:")
print(output)

# Check that tracer output is present
if "TestPrefix: before tracer_facility" not in output:
    fail("Missing 'TestPrefix: before tracer_facility' in output")

if "TestPrefix: after tracer_facility" not in output:
    fail("Missing 'TestPrefix: after tracer_facility' in output")

# The message sent with tracer facility should NOT appear
if "THIS SHOULD NOT APPEAR" in output:
    fail("Message with tracer facility should NOT be forwarded")

# The message sent with different facility should appear
if "syslog: [ERROR] this should appear" not in output:
    fail("Message with non-tracer facility should be forwarded")

print("Test 4 passed!")

# Test 5: Syslog while holding lock should not deadlock (overflow test)
print("\nTest 5: Syslog while holding lock should not deadlock")
# Use a timeout to detect deadlock
try:
    result = subprocess.run((sender_path, "overflow"), capture_output=True, text=True, timeout=5)
    output = result.stdout.replace("\r", "")
    print("Output (truncated):")
    # Only print first and last few lines since output is huge
    lines = output.strip().split("\n")
    if len(lines) > 10:
        print("\n".join(lines[:3]))
        print(f"... ({len(lines) - 6} lines omitted) ...")
        print("\n".join(lines[-3:]))
    else:
        print(output)

    # Check that we completed without deadlock
    if "TestPrefix: start" not in output:
        fail("Missing 'TestPrefix: start' in output")

    if "TestPrefix: end" not in output:
        fail("Missing 'TestPrefix: end' in output - possible deadlock?")

    print("Test 5 passed!")

except subprocess.TimeoutExpired:
    fail("Test timed out - deadlock detected!")

print("\nAll tests passed!")
sys.exit(0)
