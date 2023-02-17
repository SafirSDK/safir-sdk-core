#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011-2013 (http://safirsdkcore.com)
#
# Created by: Lars Hagstrom (lars@foldspace.nu)
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
import os, time, sys, argparse, re
from testenv import TestEnv, TestEnvStopper, log


def makeaddr(port):
    return "127.0.0.1:" + str(port)


def launch_node(args, instance):
    log("Launching node", instance)
    os.environ["SAFIR_INSTANCE"] = str(instance)
    os.environ["SAFIR_NODE_NAME"] = "Node_" + str(instance)
    os.environ["SAFIR_CONTROL_ADDRESS"] = makeaddr(30000 + instance)
    os.environ["SAFIR_DATA_ADDRESS"] = makeaddr(40000 + instance)
    if instance == 0:
        #We need a space in the address, since otherwise windows will ignore the variable...
        os.environ["SAFIR_SEED_ADDRESS"] = " "
    else:
        os.environ["SAFIR_SEED_ADDRESS"] = makeaddr(30000)
    os.environ["SYSLOG_SERVER_PORT"] = str(20000 + instance)
    env = TestEnv(args.safir_control,
                  args.dose_main,
                  args.dope_main if instance == 0 else None,
                  args.safir_show_config,
                  wait_for_persistence=instance == 0)

    if instance == 0:
        #env.launchProcess("Dobexplorer", args.dobexplorer)
        pass

    for handler in range(args.handlers):
        if instance == 0:
            cmd = (args.owner, "--handler", str(handler))
            if args.entity_updates:
                cmd += ( "--update", "--update-period", str(args.update_period))
            env.launchProcess(f"WaitingStatesOwner_{handler}", cmd)
        elif args.entity_requests:
            env.launchProcess(f"RequestSender_{handler}", (args.sender, "--handler",  str(handler)))
    return env


def parse_arguments():
    parser = argparse.ArgumentParser("test script")
    parser.add_argument("--owner", required=True)
    parser.add_argument("--sender", required=True)
    parser.add_argument("--safir-control", required=True)
    parser.add_argument("--dobexplorer", required=True)
    parser.add_argument("--dose_main", required=True)
    parser.add_argument("--dope_main", required=True)
    parser.add_argument("--safir-show-config", required=True)
    parser.add_argument("--clients", type=int, default=10)
    parser.add_argument("--handlers", type=int, default=1, help="number of handlers used. Means more owners and more requestors are started")
    parser.add_argument("--update-period", type=int, default=10, help="Number of ms between updates of all entities")
    parser.add_argument('--entity-updates', action="store_true", default=False,
                        help="Produce lots of entity updates on node 0")
    parser.add_argument('--entity-requests', action="store_true", default=False,
                        help="Produce lots of entity requests on all nodes except 0. Owner always deny requests, so this only tests request mechanism.")
    arguments = parser.parse_args()

    return arguments


def main():
    args = parse_arguments()
    server = launch_node(args, 0)
    output = server.Output("safir_control")
    incarnation = int(re.search(r"Starting system with incarnation id (.*)", output).group(1))
    log("This system has incarnation", incarnation)

    with TestEnvStopper(server):
        env = dict()
        try:
            for _ in range(3):
                for i in range(1, args.clients + 1):
                    env[i] = launch_node(args, i)

                for i in range(1, args.clients + 1):
                    env[i].WaitForPersistence()
                    output = env[i].Output("safir_control")
                    res = re.search(r"Joined system with incarnation id (.*)", output)
                    if res is None:
                        log("Failed to find join statement in:")
                        log(output)
                        log("Sleeping a while, to see if that will let us join")
                        time.sleep(10)
                        log(env[i].Output("safir_control"))
                        return 1
                    inc = int(res.group(1))
                    if inc != incarnation:
                        log("Joined invalid incarnation!!!!")
                        return 1

                if args.entity_requests:
                    for num in range(args.handlers):
                        for i in range(1, args.clients + 1):
                            log(f"Waiting for instance {i} to have completed sending requests")
                            env[i].WaitForOutput(f"RequestSender_{num}", "Have sent 1000 requests and gotten responses to them")

                for i in range(1, args.clients + 1):
                    log(f"Terminating instance {i}")
                    env[i].killprocs()
                    syslog_output = env[i].Syslog()
                    if len(syslog_output) != 0:
                        log("Unexpected syslog output:\n" + syslog_output)
                        return 1
                    if not env[i].ReturnCodesOk():
                        log("Some process failed")
                        return 1
            log("exiting normally")
        finally:
            log ("cleaning up")
            for i, e in env.items():
                e.killprocs()

    if not server.ReturnCodesOk():
        log("Some process exited with an unexpected value")
        return 1

    return 0


sys.exit(main())
