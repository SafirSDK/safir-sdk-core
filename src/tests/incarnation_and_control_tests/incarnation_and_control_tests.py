#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2015 (http://safirsdkcore.com)
#
# Created by: Samuel Waxin (samuel@waxin.se)
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
import subprocess, os, time, sys, re, argparse
from testenv import TestEnv, TestEnvStopper


#starts a node in the given instance and adds the env to the given list
def startNode(safir_instance):
    os.environ["SAFIR_INSTANCE"] = safir_instance

    environ = TestEnv(safir_control=arguments.safir_control,
                      dose_main=arguments.dose_main,
                      dope_main=arguments.dope_main,
                      safir_show_config=arguments.safir_show_config,
                      start_syslog_server=True if safir_instance == "0" else False,
                      ignore_control_cmd=True if safir_instance == "0" else False)

    del os.environ["SAFIR_INSTANCE"]

    return environ


#kills all nodes by terminating their processes
def killNodeProcesses():

    try:
        if envs[0] is not None:
            syslog_output = envs[0].Syslog()
            if len(syslog_output) != 0:
                print("SYSLOG OUPUT:\n" + syslog_output)
    except KeyError:
        pass

    for i in envs:
        envs[i].killprocs()


#returns a list of nodenames to which the given node (safir_instance) is connected
#by calling system_picture_listener
def readConnectedNodes(safir_instance):

    os.environ["SAFIR_INSTANCE"] = safir_instance

    #launch the system_picture_listener and read the output
    output = subprocess.check_output([arguments.system_picture_listener, "-o"], universal_newlines=True)

    del os.environ["SAFIR_INSTANCE"]

    return re.findall(r'(?:\sE\s|\s\s\s)(\S+)@', output)


#returns a list of nodeIds to which the given node (safir_instance) is connected
#by calling system_picture_listener
def readconnectedNodeIds(safir_instance):

    os.environ["SAFIR_INSTANCE"] = safir_instance

    #launch the system_picture_listener and read the output
    try:
        output = subprocess.check_output([arguments.system_picture_listener, "-o"], universal_newlines=True)
    except subprocess.CalledProcessError:
        print("Error: Failed to read connected nodeIds " + safir_instance)
        killNodeProcesses()
        sys.exit(1)

    del os.environ["SAFIR_INSTANCE"]

    return re.findall(r'(?:\sE\s|\s\s\s)S.+\(id = (\S+),', output)


def readconnectedNodeId(safir_instance, nodeName):

    os.environ["SAFIR_INSTANCE"] = safir_instance

    #launch the system_picture_listener and read the output
    try:
        output = subprocess.check_output([arguments.system_picture_listener, "-o"], universal_newlines=True)
    except subprocess.CalledProcessError:
        print("Error: Failed read nodeId for " + nodeName + " from instance" + safir_instance)
        killNodeProcesses()
        sys.exit(1)

    del os.environ["SAFIR_INSTANCE"]

    matchList = re.findall(r"(?:\sE\s|\s\s\s)" + nodeName + r".+id = (\S+),", output)

    if len(matchList) == 0:
        print("Did not find NodeID: for node with name " + nodeName)

    if len(matchList) == 0:
        return ""
    else:
        return str(matchList[0])


#stops the whole system from the instance given by the safir_instace
def stopSystem(safir_instance):

    os.environ["SAFIR_INSTANCE"] = safir_instance

    print("Trying to stop the system from instance " + safir_instance)

    #launch the safir_control_cli and wait until it exists
    try:
        subprocess.check_call([arguments.safir_control_cli, "-a", "STOP"], universal_newlines=True)
    except subprocess.CalledProcessError:
        print("Error: Failed to stop the system from instance " + safir_instance)
        killNodeProcesses()
        sys.exit(1)

    del os.environ["SAFIR_INSTANCE"]


#stops the node given nodeId via the safir_control_cli ran at node given by safir_instance
def stopNode(safir_instance, nodeId):

    os.environ["SAFIR_INSTANCE"] = safir_instance

    print("Trying to stop node " + nodeId + " from instance " + safir_instance)

    #launch the safir_control_cli and wait until it exists
    try:
        subprocess.check_call([arguments.safir_control_cli, "-a", "STOP", "-n", nodeId], universal_newlines=True)
    except subprocess.CalledProcessError:
        print("Error: Failed to stop node with id:" + str(nodeId) + " from instance " + safir_instance)
        killNodeProcesses()
        sys.exit(1)

    del os.environ["SAFIR_INSTANCE"]


#checks if the node given by the safir_instance has a connection to the given nodes
def checkConnectionToNodes(safir_instance, nodes):
    os.environ["SAFIR_INSTANCE"] = safir_instance

    #try for one minute
    for i in range(60):
        connectedNodes = readConnectedNodes(safir_instance)
        connectedNodes.sort()
        nodes.sort()

        if connectedNodes == nodes:
            print("Server_" + safir_instance + " has connection to (only) " + str(nodes))
            return
        time.sleep(1.0)

    print("Error: Server_" + safir_instance + " does not have contact with (only) " + str(nodes) +
          " we have connection to: " + str(connectedNodes))
    killNodeProcesses()
    sys.exit(1)


def waitForProcessExit(node):
    #try for one minute
    for _ in range(120):
        if not envs[node].SafirControlRunning():
            envs[node].killprocs()
            return
        time.sleep(0.5)

    print("Error: Node " + str(node) + " isn't stopping as expected")
    killNodeProcesses()
    sys.exit(1)


parser = argparse.ArgumentParser("test script")
parser.add_argument("--safir-control", required=True)
parser.add_argument("--dose_main", required=True)
parser.add_argument("--dope_main", required=True)
parser.add_argument("--system_picture_listener", required=True)
parser.add_argument("--safir_control_cli", required=True)
parser.add_argument("--safir-show-config", required=True)

arguments = parser.parse_args()

envs = dict()

#start all nodes
print("Starting nodes 0,1,2")

try:
    envs[0] = startNode("0")
    envs[1] = startNode("1")
    envs[2] = startNode("2")
except:
    print("Error: Failed to start nodes 0,1,2")
    killNodeProcesses()
    sys.exit(1)

#make Server_0 has connection to the other two
print("Checking that Server_0 has connection to ['Server_0', 'Server_1', 'Server_2']")
checkConnectionToNodes("0", ['Server_0', 'Server_1', 'Server_2'])

#stop node two
print("Stopping node 2 from System_0")
stopNode("0", readconnectedNodeId('0', "Server_2"))

#make sure Server_0 has connection to Server_1 only
print("Checking that Server_0 has connection to ['Server_0', 'Server_1']")
checkConnectionToNodes("0", ['Server_0', 'Server_1'])

waitForProcessExit(2)

#restart node 2
try:
    print("Starting node 2")
    envs[2] = startNode("2")
except:
    print("Error: Failed to start node 2")
    killNodeProcesses()
    sys.exit(1)

#make sure Server_0 has connection to the other two
print("Checking that Server_0 has connection to ['Server_0', 'Server_1', 'Server_2']")
checkConnectionToNodes("0", ['Server_0', 'Server_1', 'Server_2'])

#stop the whole system via safir_control_cli from node 0, note that System_1 is started with the "ignore-control-cmd" flag
#which will make it not stop. The other nodes will add the current incarnation id to the blacklist and stop
#when we start the other two again they shall not join System_1
print("Stopping the system from Server_0, Server_0 should not go down due to being started with --ignore-control-cmd")
stopSystem('0')

print("Checking that Server_0 has only to itself")
checkConnectionToNodes("0", ['Server_0'])

waitForProcessExit(1)
waitForProcessExit(2)

#start Server_1 and Server_2 again
try:
    print("Starting Server_1 and Server_2")
    envs[1] = startNode("1")
    envs[2] = startNode("2")
except:
    print("Error: Failed to start Server_1 and Server_2")
    killNodeProcesses()
    sys.exit(1)

print("Checking that Server_0 has only to itself")
checkConnectionToNodes("0", ['Server_0'])

print("Checking that Server_1 has connection to ['Server_1', 'Server_2']")
checkConnectionToNodes("1", ['Server_1', 'Server_2'])

#hard kill Server_0 and restart it, see that it joins Server_1 and Server_2 again
print("Hardkill Server_0")
envs[0].killprocs()

waitForProcessExit(0)

try:
    print("Starting Server_0")
    envs[0] = startNode("0")
except Exception as e:
    print("Error: Failed to start Server_0. Exception " + str(e))
    killNodeProcesses()
    sys.exit(1)

#make sure Server_0 has connection to the other two
checkConnectionToNodes("0", ['Server_0', 'Server_1', 'Server_2'])

print("Stopping node 1 from System_1")
stopNode("1", readconnectedNodeId('0', "Server_1"))

#make sure Server_0 has connection to Server_2 only
print("Checking that Server_0 has connection to ['Server_0', 'Server_2']")
checkConnectionToNodes("0", ['Server_0', 'Server_2'])

#end the test
print("Ending test, stopping all nodes")

killNodeProcesses()

for env in envs:
    if not envs[env].ReturnCodesOk():
        print("Error: Some process exited with an unexpected value")
        sys.exit(1)

sys.exit(0)
