#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011-2025 (http://safirsdkcore.com)
#
# Created by: Joel Ottosson (joel.ottosson@gmail.com)
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
import os, sys, argparse, subprocess, glob, logging, uuid, time
from testenv import TestEnv, TestEnvStopper, log

log_dir = os.path.normpath(os.path.join(os.getcwd(), "sate_output"))
for f in glob.glob(os.path.join(log_dir, "*")): os.remove(f)
os.environ["LLL_LOGDIR"] = log_dir

parser = argparse.ArgumentParser("test script")
parser.add_argument("--safir-control", required=True)
parser.add_argument("--dose_main", required=True)
parser.add_argument("--dope_main", required=True)
parser.add_argument("--sate", required=True)
parser.add_argument("--script1", required=True)
parser.add_argument("--script2", required=True)
parser.add_argument("--invalid-script", required=True)
parser.add_argument("--safir-show-config", required=True)
arguments = parser.parse_args()

env = TestEnv(safir_control=arguments.safir_control,
              dose_main=arguments.dose_main,
              dope_main=arguments.dope_main,
              safir_show_config=arguments.safir_show_config)
with TestEnvStopper(env):
    sate1 = env.launchProcess("sate1", [arguments.sate, "-s", arguments.script1, "-c", "sate1", "-v"], collect_output=True)    
    sate2 = env.launchProcess("sate2", [arguments.sate, "-s", arguments.script2, "-c", "sate2", "-v"], collect_output=True)
    
    log("Wait for sate 1")
    sate1.wait()
    log("Wait for sate 2")
    sate2.wait()

    out1 = env.Output("sate1")
    out2 = env.Output("sate2")

    # Helper function to verify output with tolerance for out-of-order messages
    # Each line can be up to 'window_size' positions out of order
    def verify_output_with_sliding_window(output, expected_lines, window_size=3):
        """
        Verify output contains all expected lines in roughly the right order.
        Each line can be up to window_size positions ahead of its expected position.

        Returns (success, error_message or None)
        """
        output_lines = [line.strip() for line in output.strip().split('\n') if line.strip()]
        remaining_expected = list(expected_lines)  # Copy the list
        output_idx = 0

        while remaining_expected and output_idx < len(output_lines):
            actual_line = output_lines[output_idx]

            # Check if this actual line matches any of the next 'window_size' expected lines
            found = False
            for i in range(min(window_size, len(remaining_expected))):
                if actual_line == remaining_expected[i]:
                    # Found a match - remove it from expected list
                    remaining_expected.pop(i)
                    found = True
                    break

            if not found:
                # This line wasn't in our expected window, skip it
                pass

            output_idx += 1

        if remaining_expected:
            missing = '\n  '.join(remaining_expected)
            return False, f"Missing expected lines:\n  {missing}"

        return True, None

    # Expected output lines for sate1 (in roughly expected order, but with some flexibility)
    expected1 = [
        "Opening DOB connection: sate1",
        "Starting script execution...",
        "Register entity handler: Safir.Control.Status, handler=DEFAULT_HANDLER, RequestorDecidesInstanceId",
        "Connected to DOB!",
        "Register service handler: Safir.Control.Command, handler=321",
        "Set all: Safir.Control.Status, instance=1, handler=DEFAULT_HANDLER",
        "Set all: Safir.Control.Status, instance=2, handler=DEFAULT_HANDLER",
        "Set all: Safir.Control.Status, instance=3, handler=DEFAULT_HANDLER",
        "Send message: Safir.Application.BackdoorCommand, channel=123",
        "OnServiceRequest: Safir.Control.Command",
        "Script execution completed successfully.",
        "Disconnected from DOB!"
    ]

    success, error = verify_output_with_sliding_window(out1, expected1, window_size=3)
    if not success:
        log(f"SATE1 verification failed: {error}")
        log("SATE1 output was:")
        log(out1)
        sys.exit(1)

    # Expected output lines for sate2 (in roughly expected order, but with some flexibility)
    expected2 = [
        "Opening DOB connection: sate2",
        "Starting script execution...",
        "Subscribe entity: Safir.Control.Status, recursive=1",
        "Connected to DOB!",
        "Subscribe message: Safir.Application.BackdoorCommand, channel=123, recursive=1",
        "OnEntity: New Safir.Control.Status, instance=1",
        "OnEntity: New Safir.Control.Status, instance=2",
        "OnEntity: New Safir.Control.Status, instance=3",
        "OnMessage: Safir.Application.BackdoorCommand on channel 123",
        "Send service request: Safir.Control.Command, handler=321",
        "OnResponse: Safir.Dob.SuccessResponse",
        "OnEntity: Delete Safir.Control.Status, instance=1",
        "OnEntity: Delete Safir.Control.Status, instance=2",
        "OnEntity: Delete Safir.Control.Status, instance=3",
        "Script execution completed successfully.",
        "Disconnected from DOB!"
    ]

    success, error = verify_output_with_sliding_window(out2, expected2, window_size=3)
    if not success:
        log(f"SATE2 verification failed: {error}")
        log("SATE2 output was:")
        log(out2)
        sys.exit(1)

    # Test that sate exits with a non-zero error code for an invalid script
    log("Testing invalid script error handling")
    result = subprocess.run([arguments.sate, "-s", arguments.invalid_script],
                            capture_output=True, timeout=10)
    if result.returncode == 0:
        log("FAIL: sate exited with 0 for an invalid script, expected a non-zero error code")
        log("stderr: " + result.stderr.decode())
        sys.exit(1)
    stderr = result.stderr.decode()
    if "Error" not in stderr:
        log("FAIL: expected an error message on stderr for invalid script, got: " + repr(stderr))
        sys.exit(1)
    log("OK: sate correctly exited with error code " + str(result.returncode) + " for invalid script")

    log("Exited, will now exit testenv.")

sys.exit(0)
