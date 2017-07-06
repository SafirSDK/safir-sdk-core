#!/usr/bin/env python
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
from __future__ import print_function
import os, time, sys, argparse, re
from testenv import TestEnv, TestEnvStopper

def makeaddr(port):
    return "127.0.0.1:" + str(port)

def launch_node(args, instance):
    print("Launching node", instance)
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
                  wait_for_persistence = instance == 0)

    if instance == 0:
        env.launchProcess("WaitingStatesOwner",args.owner)
    return env

def parse_arguments():
    parser = argparse.ArgumentParser("test script")
    parser.add_argument("--owner", required=True)
    parser.add_argument("--safir-control", required=True)
    parser.add_argument("--dobexplorer", required=True)
    parser.add_argument("--dose_main", required=True)
    parser.add_argument("--dope_main", required=True)
    parser.add_argument("--safir-show-config", required=True)
    parser.add_argument("--safir-generated-paths", required=True)
    parser.add_argument("--clients", default=10)
    arguments = parser.parse_args()

    #add all the environment variables. passed on format A=10;B=20
    for pair in arguments.safir_generated_paths.split(";"):
        (name,value) = pair.split("=")
        print("Setting environment variable", name, "to", value)
        os.environ[name] = value

    return arguments

def main():
    args = parse_arguments()
    server = launch_node(args, 0)
    output = server.Output("safir_control")
    incarnation = int(re.search(r"Starting system with incarnation id (.*)",output).group(1))
    print ("This system has incarnation", incarnation)

    with TestEnvStopper(server):
        env = dict()
        try:
            for _ in range(3):
                for i in range(1,args.clients+1):
                    env[i] = launch_node(args,i)

                for i in range(1,args.clients+1):
                    env[i].WaitForPersistence()
                    output = env[i].Output("safir_control")
                    res = re.search(r"Joined system with incarnation id (.*)",output)
                    if res is None:
                        print("Failed to find join statement in:")
                        print(output)
                        print("Sleeping a while, to see if that will let us join")
                        time.sleep(10)
                        print(env[i].Output("safir_control"))
                        return 1
                    inc = int(res.group(1))
                    if inc != incarnation:
                        print ("Joined invalid incarnation!!!!")
                        return 1

                for i in range(1,args.clients+1):
                    env[i].killprocs()
                    if not env[i].ReturnCodesOk():
                        print("Some process failed")
                        return
        finally:
            for i,e in env.items():
                e.killprocs()


    if not server.ReturnCodesOk():
        print("Some process exited with an unexpected value")
        return 1

    return 0

sys.exit(main())
