#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2015 (http://safir.sourceforge.net)
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
from __future__ import print_function
import subprocess, os, time, sys, signal, re, argparse
from testenv import TestEnv, TestEnvStopper

#starts a node in the given instance and adds the env to the given list
def startNode(safir_instance):

  os.environ["SAFIR_INSTANCE"] = safir_instance

  env = TestEnv(safir_control = arguments.safir_control,
               dose_main = arguments.dose_main,
               dope_main = arguments.dope_main,
               safir_show_config = arguments.safir_show_config,
               start_syslog_server = True if safir_instance == "1" else False)

  del os.environ["SAFIR_INSTANCE"]

  return env;


#kills all nodes by terminating their processes
def killNodeProcesses():
  for env in envs:
    envs[env].killprocs()


#returns a list of nodenames to which the given node (safir_instance) is connected
#by calling system_picture_listener
def readConnectedNodes(safir_instance):

  os.environ["SAFIR_INSTANCE"] = safir_instance

  #launch the system_picture_listener and read the output
  output = subprocess.check_output([arguments.system_picture_listener, "-o"],
                                    universal_newlines = True)

  del os.environ["SAFIR_INSTANCE"]

  return re.findall('(\S+)@', output)


#returns a list of nodeIds to which the given node (safir_instance) is connected
#by calling system_picture_listener
def readconnectedNodeIds(safir_instance):

  os.environ["SAFIR_INSTANCE"] = safir_instance

  #launch the system_picture_listener and read the output
  try:
    output = subprocess.check_output([arguments.system_picture_listener, "-o"],
                                    universal_newlines = True)
  except CalledProcessError:
    print("Failed to read connected nodeIds " + safir_instance)
    killNodeProcesses()
    sys.exit(1)

  del os.environ["SAFIR_INSTANCE"]

  return re.findall('id = (\S+),', output)

def readconnectedNodeId(safir_instance, nodeName):

  os.environ["SAFIR_INSTANCE"] = safir_instance

  #launch the system_picture_listener and read the output
  try:
    output = subprocess.check_output([arguments.system_picture_listener, "-o"],
                                    universal_newlines = True)
  except CalledProcessError:
    print("*-* Failed read nodeId for " + nodeName + " from instance" + safir_instance)
    killNodeProcesses()
    sys.exit(1)

  del os.environ["SAFIR_INSTANCE"]

  return re.findall(nodeName + ".+id = (\S+)", output)


#stops the node given nodeId via the safir_control_cli ran at node given by safir_instance
def stopNode(safir_instance, nodeId):

  os.environ["SAFIR_INSTANCE"] = safir_instance

  #launch the safir_control_cli and wait until it exists
  try:
    subprocess.check_call([arguments.safir_control_cli, "-a", "STOP", "-n", nodeId],
                                    universal_newlines = True)
  except CalledProcessError:
    print("*-* Failed to stop node with id:" + str(nodeId) + " from instance " + safir_instance)
    killNodeProcesses()
    sys.exit(1)

  del os.environ["SAFIR_INSTANCE"]

#checks if the node given by the safir_instance has a connection to the given nodes
def checkConnectionToNodes(safir_instance, nodes):

  os.environ["SAFIR_INSTANCE"] = safir_instance

  if readConnectedNodes(safir_instance).sort() == nodes.sort():
    print("Server_" + safir_instance + " has connection to " + str(nodes))
  else:
    print("Server_" + safir_instance + " does not have contact with the " + str(nodes))
    killNodeProcesses()
    sys.exit(1)

parser = argparse.ArgumentParser("test script")
parser.add_argument("--safir-control", required=True)
parser.add_argument("--dose_main", required=True)
parser.add_argument("--dope_main", required=True)
parser.add_argument("--system_picture_listener", required=True)
parser.add_argument("--safir_control_cli", required=True)
parser.add_argument("--safir-show-config", required=True)
parser.add_argument("--safir-generated-paths", required=True)

arguments = parser.parse_args()

#add all the environment variables. passed on format A=10;B=20
for pair in arguments.safir_generated_paths.split(";"):
    (name,value) = pair.split("=")
    print("*-* Setting environment variable", name, "to", value)
    os.environ[name] = value

envs = dict()

#start all nodes
print("*-* Starting nodes 0,1,2")
envs['0'] = startNode("0");
envs['1'] = startNode("1")
envs['2'] = startNode("2")

#make Server_0 has connection to the other two
checkConnectionToNodes("0", ['Server_0', 'Server_1', 'Server_2'])

#stop node two
print("*-* Stopping node 2")
stopNode("2", readconnectedNodeIds("0")[2])

time.sleep(5)

#make sure Server_0 has connection to Server_1 only
checkConnectionToNodes("0", ['Server_0', 'Server_1'])

#restart node 2
print("*-* Starting node 2")
envs['2'] = startNode("2")

#make Server_0 has connection to the other two
checkConnectionToNodes("0", ['Server_0', 'Server_1', 'Server_2'])

print("*-* Stopping all nodes")
killNodeProcesses()

for env in envs:
    if not envs[env].ReturnCodesOk():
        print("Some process exited with an unexpected value")
        sys.exit(1)

sys.exit(0)

