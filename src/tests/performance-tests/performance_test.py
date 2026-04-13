#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2026 (http://safirsdkcore.com)
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
"""
Performance test driver script.

Starts the Safir infrastructure (safir_control + dose_main), then launches the
producer and consumer executables, waits for them to finish, and reports the
overall result.

TODO: Add actual pass/fail criteria based on measured throughput/latency.
"""

import os
import sys
import argparse
import time
import glob
from testenv import TestEnv, log

failed_tests = set()


def parse_arguments():
    parser = argparse.ArgumentParser("performance_test")
    parser.add_argument("--safir-control", required=True)
    parser.add_argument("--dose_main", required=True)
    parser.add_argument("--dope_main", required=True)
    parser.add_argument("--safir-show-config", required=True)
    parser.add_argument("--producer", required=True)
    parser.add_argument("--consumer", required=True)
    parser.add_argument("--num-instances", type=int, default=10)
    parser.add_argument("--num-updates", type=int, default=100)
    return parser.parse_args()


def run_test(name, args):
    """Run a single named performance test case."""
    log(f"=== Start: {name} ===")

    log_dir = os.path.normpath(os.path.join(os.getcwd(), "test_output", name))
    os.makedirs(log_dir, exist_ok=True)
    for f in glob.glob(os.path.join(log_dir, "*")):
        os.remove(f)
    os.environ["LLL_LOGDIR"] = log_dir
    os.environ["SAFIR_INSTANCE"] = "0"

    env = None
    producer_proc = None
    consumer_proc = None
    try:
        env = TestEnv(args.safir_control,
                      args.dose_main,
                      args.dope_main,
                      args.safir_show_config,
                      start_syslog_server=True,
                      ignore_control_cmd=False,
                      wait_for_persistence=True)

        log("Launching consumer")
        consumer_proc = env.launchProcess("consumer", [args.consumer,
                                                        "--num-instances", str(args.num_instances),
                                                        "--num-updates", str(args.num_updates)])

        log("Waiting for consumer to be ready")
        env.WaitForOutput("consumer", "Consumer ready")

        log("Launching producer")
        producer_proc = env.launchProcess("producer", [args.producer,
                                                        "--num-instances", str(args.num_instances),
                                                        "--num-updates", str(args.num_updates)])

        time.sleep(40)
        env.WaitForOutput("consumer", "Consumer finished")

    except Exception as exc:  # pylint: disable=broad-except
        failed_tests.add(name)
        log(f"*** Exception in test '{name}': {exc}")
        import traceback
        traceback.print_exc()
    finally:
        # Shut down the Safir infrastructure first.  safir_control termination
        # causes dose_main to send stop orders to connected apps (producer and
        # consumer), allowing them to exit cleanly.  We must do this BEFORE
        # calling communicate() — otherwise we deadlock waiting for processes
        # that are themselves waiting for a stop order.
        if env is not None:
            time.sleep(1.0)
            env.killprocs()

            for proc_name in ("consumer", "producer"):
                output = env.Output(proc_name).strip()
                if output:
                    log(f"--- {proc_name} output ---\n{output}")

        for proc_name, proc in [("Producer", producer_proc), ("Consumer", consumer_proc)]:
            if proc is None:
                continue
            if proc.returncode != 0:
                log(f"*** {proc_name} exited with code {proc.returncode}")
                failed_tests.add(name)

            # TODO: Parse and validate performance results from stdout.

    log(f"--- Finished: {name} ---")


def main():
    args = parse_arguments()

    # --- Test cases ---
    # TODO: Add more test cases (different entity counts, message sizes, etc.)
    run_test("basic_producer_consumer", args)

    if failed_tests:
        log(f"*** {len(failed_tests)} test(s) FAILED: {sorted(failed_tests)}")
        sys.exit(1)

    log("All performance tests passed")
    sys.exit(0)


if __name__ == "__main__":
    main()
