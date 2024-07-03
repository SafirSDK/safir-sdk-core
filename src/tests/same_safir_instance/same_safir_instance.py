#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2024 (http://safirsdkcore.com)
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
import os, sys, argparse, socket, glob, logging, uuid, time
import asyncio, json, websockets
from contextlib import contextmanager
from testenv import TestEnv, TestEnvStopper, log

failed_tests = set()
# ===================================================================
# Helpers
# ===================================================================
@contextmanager
def test_case(name):
    global failed_tests
    log_dir = os.path.normpath(os.path.join(os.getcwd(), "test_output", name))
    for f in glob.glob(os.path.join(log_dir, "*")): os.remove(f)
    os.environ["LLL_LOGDIR"] = log_dir

    try:
        log("=== Start: " + name + " ===")
        yield
    except Exception as e:
        failed_tests.add(name)
        logging.exception('Got exception')
        log(e)
        log("*** Test failed: " + name)
    finally:
        log("--- Finished: " + name + " ---")

@contextmanager
def launch_node(args, safir_instance, node_id, first_node):
    try:
        session_id = str(uuid.uuid4())
        # os.environ["SAFIR_COM_NETWORK_SIMULATION"] = session_id # Enable network simulation
        os.environ["SAFIR_INSTANCE"] = str(safir_instance)
        print("--- Launching node", str(node_id))
        env = TestEnv(args.safir_control,
                    args.dose_main,
                    args.dope_main if first_node else None,
                    args.safir_show_config,
                    start_syslog_server = first_node,
                    ignore_control_cmd = first_node,
                    wait_for_persistence = first_node,
                    force_node_id = node_id)
        env.session_id = session_id
        # if first_node:
        #     env.launchProcess("safir_websocket", args.safir_websocket)
        yield env
    finally:
        log("--- kill node" + str(node_id))
        env.killprocs()

def parse_arguments():
    parser = argparse.ArgumentParser("test script")
    parser.add_argument("--safir-control", required=True)
    parser.add_argument("--dose_main", required=True)
    parser.add_argument("--dope_main", required=True)
    parser.add_argument("--safir-show-config", required=True)
    parser.add_argument("--safir_websocket", required=True)
    arguments = parser.parse_args()
    return arguments

# ===================================================================
# Test cases
# ===================================================================
async def start_two_nodes_as_same_instance(args):
    with test_case("start_two_nodes_as_same_instance"),\
        launch_node(args, safir_instance=1, node_id=1, first_node=True) as node1:
        print(" node1 is started")
        
        with launch_node(args, safir_instance=1, node_id=2, first_node=False) as node2:
            print(" node2 is started with the same SAFIR_INSTANCE as node1")
            await asyncio.sleep(5)
            if node2.SafirControlRunning():
                print("  node2 is still alive but was expected to terminate")
                raise AssertionError("node2 is still alive but was expected to terminate, test failed!")
            else:
                print("  node2 has terminated just as expected, test succeeded!")

# ===========================================
# main
# ===========================================
async def main(args):
    await start_two_nodes_as_same_instance(args)

if __name__ == "__main__":
    asyncio.run(main(parse_arguments()))
    for failed in failed_tests:
        log("*** Failed: ", failed)
    sys.exit(1 if len(failed_tests) > 0 else 0)
