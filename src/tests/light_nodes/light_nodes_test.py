#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2022 (http://safirsdkcore.com)
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
import os, time, sys, argparse, re, socket
import asyncio, json, websockets
from testenv import TestEnv, TestEnvStopper, log

return_code = 0
def set_return_code(code):
    global return_code
    return_code = code

# ===================================================================
# Helpers
# ===================================================================
# Launch a Safir node. For simlicity we use safir_instance as nodeId,
# thats why we use start at 1 and not 0, since Communication does not accept nodeId=0
def launch_node(args, safir_instance):    
    os.environ["SAFIR_COM_NETWORK_SIMULATION"] = "True" # Enable network simulation
    os.environ["SAFIR_INSTANCE"] = str(safir_instance)
    print("Launching node", str(safir_instance))
    env = TestEnv(args.safir_control,
                  args.dose_main,
                  args.dope_main if safir_instance == 1 else None,
                  args.safir_show_config,
                  start_syslog_server=True if safir_instance == 1 else False,
                  ignore_control_cmd=True if safir_instance == 1 else False,
                  wait_for_persistence=True,
                  force_node_id=safir_instance)
    
    env.launchProcess("safir_websocket", args.safir_websocket)
    return env

# Simulate network up/down
def set_network_state(state, safir_instance):
    cmd = ("up " if state == True else "down ") + str(safir_instance)
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.sendto(bytes(cmd, "utf-8"), ("239.6.6.6", 16666))

def parse_arguments():
    parser = argparse.ArgumentParser("test script")
    parser.add_argument("--safir-control", required=True)
    parser.add_argument("--dose_main", required=True)
    parser.add_argument("--dope_main", required=True)
    parser.add_argument("--safir-show-config", required=True)
    parser.add_argument("--safir_websocket", required=True)
    arguments = parser.parse_args()
    return arguments

# Read json method
def method(msg):
        return msg["method"] if "method" in msg else None

# Read json douType
def dou_type(msg):
    p = msg["params"]
    if "typeId" in p:
        return p["typeId"]
    if "entity" in p:
        return p["entity"]["_DouType"];
    if "message" in p:
        return p["message"]["_DouType"];
    if "request" in p:
        return p["request"]["_DouType"];

def instance_id(msg):
    return msg["params"]["instanceId"]

def handler_id(msg):
    return msg["params"]["handlerId"]

def entity(msg):
    return msg["params"]["entity"]

# Type is local
def is_local(msg):
    return dou_type(msg) in ["Safir.Dob.ProcessInfo"]

def dict_to_sorted_list(d):
    l = list()
    for k, v in d.items():
        l.append(k + " - " + v)
    l.sort()
    return l

def pools_equal(safir_app1, safir_app2):
    ent = dict_to_sorted_list(safir_app1.entities) == dict_to_sorted_list(safir_app2.entities)
    reg = dict_to_sorted_list(safir_app1.registrations) == dict_to_sorted_list(safir_app2.registrations)
    return ent and reg

# ==============================================================================
# Simple Safir application.
# Instance decides which Dob instance to connect to. Normal 1,2 Light 3,4
# ==============================================================================
class SafirApp:
    def __init__(self, instance):
        self.instance = instance
        self.sendQueue = asyncio.Queue()
        self.entities = dict()
        self.registrations = dict()

    async def run(self):
        uri = "ws://localhost:1000" + str(self.instance)
        connectTry = 0
        while connectTry <  5: # make 5 efforts to connect, total 15 sec
            try:
                async with websockets.connect(uri) as ws:
                    connectTry = 10 
                    self.ws = ws
                    await asyncio.gather(self._reader(), self._sender())
            except ConnectionRefusedError:
                connectTry = connectTry +1
                await asyncio.sleep(3)
            
        if connectTry == 5:
            print("Failed to connect to websocket")

    async def stop(self):
        await self.sendQueue.put(None)
        await self.sendQueue.join()

    async def send(self, msg):
        await self.sendQueue.put(msg)

    async def _reader(self):
        async for message in self.ws:
            msg = json.loads(message)
            callback = method(msg)
            if callback in ["onNewEntity", "onUpdatedEntity"] and not is_local(msg):
                entity_id = dou_type(msg) + ":" + str(instance_id(msg))
                self.entities[entity_id] = json.dumps(entity(msg))
            elif callback == "onDeletedEntity":
                entity_id = dou_type(msg) + ":" + str(instance_id(msg))
                self.entities.pop(entity_id, None)
            elif callback == "onRegistered" and not is_local(msg):
                handler = dou_type(msg) + ":" + str(handler_id(msg))
                self.registrations[handler] = "Registered"
            elif callback == "onUnregistered":
                handler = dou_type(msg) + ":" + str(handler_id(msg))
                self.registrations.pop(handler, None)

    async def _sender(self):
        while True:
            msg = await self.sendQueue.get()
            self.sendQueue.task_done()
            if msg is not None:
                await self.ws.send(msg)
            else:
                await self.ws.close()
                return

    def dump_pool(self):
        log("=== Node " + str(self.instance) + " ===")
        log("Registrations:")
        for val in dict_to_sorted_list(self.registrations):
            log(val)            
        print("Entities:")
        for val in dict_to_sorted_list(self.entities):
            log(val)
        log("\n")

# ===================================================================
# Test cases
# ===================================================================
async def one_normal_one_light_detach_attach(args):    
    # Setup: One Normal and one Light and one app running on each node
    node1 = launch_node(args, 1)
    node3 = launch_node(args, 3) 
    app1 = SafirApp(1)
    app3 = SafirApp(3)
    
    # Test steps
    async def test_sequence():
        # app1 - connect to Dob and subscribe for all Entities and Registrations
        await app1.send('{"method": "open", "params": {"connectionName": "app1"}, "id": 1}')
        await app1.send('{"method": "subscribeEntity", "params": {"typeId": "Safir.Dob.Entity"}, "id": 2}')
        await app1.send('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Entity"}, "id": 3}')
        await app1.send('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Service"}, "id": 4}')

        # app3 - connect to Dob and subscribe for all Entities and Registrations
        await app3.send('{"method": "open", "params": {"connectionName": "app3"}, "id": 1}')
        await app3.send('{"method": "subscribeEntity", "params": {"typeId": "Safir.Dob.Entity"}, "id": 2}')
        await app3.send('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Entity"}, "id": 3}')
        await app3.send('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Service"}, "id": 4}')
        
        # let the system run for a while
        await asyncio.sleep(5)

        # Disable network on lightnode and wait for it to be Detached
        set_network_state(False, safir_instance=3) 
        while "Detached" not in app3.entities.get("Safir.Dob.NodeInfo:3"):
            await asyncio.sleep(2)

        log("--- node3 is now detached")
        # node3 is detached and the pools should not be the same
        if pools_equal(app1, app3):
            log("ERROR: Not expected to have the same pools after detach")
            app1.dump_pool()
            app3.dump_pool()
            set_return_code(1)

        # Enable network on lightnode and wait for it to be Attached
        set_network_state(True, safir_instance=3) 
        while "Attached" not in app3.entities.get("Safir.Dob.NodeInfo:3"):
            await asyncio.sleep(2)

        log("--- node3 is now attached again")

        # Now node3 has been detached and then re-joined the system. Let it run for a while and then check that the pools are in sync
        await asyncio.sleep(5)

        if pools_equal(app1, app3) == False:
            log("ERROR: Nodes are expected to have the same pools again after lightnode re-joined system")
            app1.dump_pool()
            app3.dump_pool()
            set_return_code(1)

        await app1.stop()
        await app3.stop()

    # Run the test_sequence
    await asyncio.gather(app1.run(), app3.run(), test_sequence());

    # Close nodes
    node1.killprocs()
    node3.killprocs()
    return return_code

# ===========================================
# main
# ===========================================
if __name__ == "__main__":
    args = parse_arguments()
    asyncio.run(one_normal_one_light_detach_attach(args))
    sys.exit(return_code)
