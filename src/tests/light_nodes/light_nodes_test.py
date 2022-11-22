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
import os, sys, argparse, socket, glob
import asyncio, json, websockets
from testenv import TestEnv, TestEnvStopper, log

failed_tests = set()
def set_failed(test):
    global failed_tests
    failed_tests.add(test)

# ===================================================================
# Helpers
# ===================================================================
# Launch a Safir node.
def launch_node(args, safir_instance, node_id):    
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
                  force_node_id=node_id)
    
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

# Some types we just ignore since they are hard to predict in the test cases
def ignore_type(msg):
    return dou_type(msg) in ["Safir.Dob.ProcessInfo", "Safir.Dob.PersistentDataStatus"]

def dict_to_sorted_list(d):
    l = list()
    for k, v in d.items():
        l.append(k + " - " + v)
    l.sort()
    return l

# check if all dict values are equal for specified keys
def equal_values(keys, *dictionaries):
    for key in keys:
        items = set()
        for d in dictionaries:
            val = d.get(key)
            if val == None:
                return False
            items.add(val)
        if len(items) > 1:
            return False

    return True

# Checklif dictionary contains exact specied keys
def contains_exact(keys, dictionary):
    return sorted(list(dictionary.keys())) == sorted(keys)

def pools_equal(safir_app1, safir_app2):
    ent = dict_to_sorted_list(safir_app1.entities) == dict_to_sorted_list(safir_app2.entities)
    reg = dict_to_sorted_list(safir_app1.registrations) == dict_to_sorted_list(safir_app2.registrations)
    return ent and reg

async def correct_pool_detached_node(app):
    ok = False
    limited_type = "DoseTest.LimitedEntity" if app.safir_instance == 3 else "DoseTest.LimitedEntity2"
    expected_reg = [limited_type + ":DEFAULT_HANDLER", "DoseTest.LimitedService:app" + str(app.node_id), "Safir.Dob.NodeInfo:" + str(app.node_id)]
    expected_ent = [limited_type + ":1", "Safir.Dob.NodeInfo:" + str(app.node_id)]
    expected_reg.sort()
    expected_ent.sort()
    
    max_tries = 5
    for check in range(max_tries):
        if contains_exact(expected_reg, app.registrations) and contains_exact(expected_ent, app.entities):
            ok = True
            break
        log("--- Extended wait for PD to finish")
        await asyncio.sleep(5)

    if not ok:
        log("*** ERROR: Incorrect pool on light node " + str(app.node_id)) + ", safir_instance: " + str(app.safir_instance)
        print("  Expected registrations:")
        for i in expected_reg: print("    " + i)
        print("  Expected entities:")
        for i in expected_ent: print("    " + i)
        app.dump_pool()

    return ok

async def correct_pools_connected_nodes(*apps):
    ok = True
    expected_global_reg = list()
    expected_global_ent = list()
    expected_limited_reg = list()
    expected_limited_ent = list()
    for app in apps:
        if app.safir_instance < 3: # normal node
            global_type = "DoseTest.GlobalEntity" if app.safir_instance == 1 else "DoseTest.GlobalEntity2"
            expected_global_reg.append(global_type + ":DEFAULT_HANDLER")
            expected_global_ent.append(global_type + ":1")
            expected_global_reg.append("Safir.Dob.NodeInfo:" + str(app.node_id)) #NodeInfo is actually a limited type, but when registered on a normal node, limited types are visible to all nodes
            expected_global_ent.append("Safir.Dob.NodeInfo:" + str(app.node_id))
            expected_global_reg.append("DoseTest.GlobalService:app" + str(app.node_id))
        else: # light node
            limited_type =  "DoseTest.LimitedEntity" if app.safir_instance == 3 else "DoseTest.LimitedEntity2"
            expected_limited_reg.append(limited_type + ":DEFAULT_HANDLER")
            expected_limited_ent.append(limited_type + ":1")
            expected_limited_reg.append("Safir.Dob.NodeInfo:" + str(app.node_id))
            expected_limited_ent.append("Safir.Dob.NodeInfo:" + str(app.node_id))
            expected_limited_reg.append("DoseTest.LimitedService:app" + str(app.node_id))

    max_tries = 5
    for check in range(max_tries):
        ok = True
        for app in apps:
            node_ok = True
            expected_reg = [*expected_global_reg]
            expected_ent = [*expected_global_ent]
            if app.safir_instance < 3: # normal node
                expected_reg.extend(expected_limited_reg)
                expected_ent.extend(expected_limited_ent)
            else: # light node
                limited_type =  "DoseTest.LimitedEntity" if app.safir_instance == 3 else "DoseTest.LimitedEntity2"
                expected_reg.extend([limited_type + ":DEFAULT_HANDLER", "Safir.Dob.NodeInfo:" + str(app.node_id)])
                expected_ent.extend([limited_type + ":1", "Safir.Dob.NodeInfo:" + str(app.node_id)])
                expected_reg.append("DoseTest.LimitedService:app" + str(app.node_id))

            if not (contains_exact(expected_reg, app.registrations) and contains_exact(expected_ent, app.entities)):
                ok = False
                if check == max_tries - 1:
                    log("*** ERROR: Incorrect pool on node " + str(app.node_id)) + ", safir_instance: " + str(app.safir_instance)
                    print("  Expected registrations:")
                    for i in expected_reg: print("    " + i)
                    print("  Expected entities:")
                    for i in expected_ent: print("    " + i)
                    app.dump_pool()

        if ok:
            break

        log("--- Extended wait for PD to finish")
        await asyncio.sleep(5)

    return ok

# ==============================================================================
# Simple Safir application.
# Safir_instance:  1,2 = normal node,  3,4 = light node
# ==============================================================================
class SafirApp:
    def __init__(self, safir_instance, node_id):
        self.safir_instance = safir_instance
        self.node_id = node_id
        self.sendQueue = asyncio.Queue()
        self.entities = dict()
        self.registrations = dict()

    async def run(self):
        uri = "ws://localhost:1000" + str(self.safir_instance)
        connectTry = 0
        while connectTry <  5: # make 5 efforts to connect, total 15 sec
            try:
                async with websockets.connect(uri) as ws:
                    connectTry = 10 
                    self.ws = ws
                    await self._setup_dob()
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
            try:
                msg = json.loads(message)
                callback = method(msg)
                if callback in ["onNewEntity", "onUpdatedEntity"] and not ignore_type(msg):
                    entity_id = dou_type(msg) + ":" + str(instance_id(msg))
                    self.entities[entity_id] = json.dumps(entity(msg))
                elif callback == "onDeletedEntity":
                    entity_id = dou_type(msg) + ":" + str(instance_id(msg))
                    self.entities.pop(entity_id, None)
                elif callback == "onRegistered" and not ignore_type(msg):
                    handler = dou_type(msg) + ":" + str(handler_id(msg))
                    self.registrations[handler] = "Registered"
                elif callback == "onUnregistered":
                    handler = dou_type(msg) + ":" + str(handler_id(msg))
                    self.registrations.pop(handler, None)
                elif "error" in msg:
                    log("*** Received error response from safir_websocket:", message)
                    continue
            except json.JSONDecodeError:
                log("JSONDecodeError", message) # indicates error in safir_websocket
                raise

    async def _sender(self):
        while True:
            msg = await self.sendQueue.get()
            self.sendQueue.task_done()
            if msg is not None:
                await self.ws.send(msg)
            else:
                await self.ws.close()
                return

    async def _setup_dob(self):
        app_name = "app" + str(self.node_id)
        entity_type = ""
        service_type = ""
        if (self.safir_instance < 3): # normal node
            entity_type = "DoseTest.GlobalEntity" if self.safir_instance == 1 else "DoseTest.GlobalEntity2"
            service_type = "DoseTest.GlobalService"
        else: # light node
            entity_type =  "DoseTest.LimitedEntity" if self.safir_instance == 3 else "DoseTest.LimitedEntity2"
            service_type = "DoseTest.LimitedService"

        await self.send('{"method": "open", "params": {"connectionName": "' + app_name + '"}, "id": 1}')
        await self.send('{"method": "registerEntityHandler", "params": {"typeId": "' + entity_type + '"}, "id": 2}')
        await self.send('{"method": "subscribeEntity", "params": {"typeId": "Safir.Dob.Entity"}, "id": 3}')
        await self.send('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Entity"}, "id": 4}')
        await self.send('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Service"}, "id": 5}')
        await self.send('{"method": "setEntity", "params": {"entity": {"_DouType": "' + entity_type + '", "Info": "Hello"}, "instanceId": 1}, "id": 6}')
        await self.send('{"method": "registerServiceHandler", "params": {"typeId": "' + service_type + '", "handlerId": "' + app_name + '"}, "id": 7}')

    def dump_pool(self):
        print("=== Node: " + str(self.node_id) + ", safir_instance: " + str(self.safir_instance) + " ===")
        print("  Registrations:")
        for val in dict_to_sorted_list(self.registrations):
            print("    " + val)            
        print("  Entities:")
        for val in dict_to_sorted_list(self.entities):
            print("    " + val)
        log("--------------")

# ===================================================================
# Test cases
# ===================================================================
async def one_normal_one_light_detach_reattach_light(args):
    test_name =  "one_normal_one_light_detach_reattach_light"
    log("--- Run", test_name)
    node1 = launch_node(args, safir_instance=1, node_id=1)
    node3 = launch_node(args, safir_instance=3, node_id=3) 
    app1 = SafirApp(safir_instance=1, node_id=1)
    app3 = SafirApp(safir_instance=3, node_id=3)
    
    # Test steps
    async def test_sequence():
        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        if not await correct_pools_connected_nodes(app1, app3):
            set_failed(test_name)

        # Disable network on lightnode and wait for it to be Detached
        set_network_state(False, safir_instance=3) 
        while "Detached" not in app3.entities.get("Safir.Dob.NodeInfo:3"):
            await asyncio.sleep(2)
        log("--- node3 is now detached")

        if not (await correct_pools_connected_nodes(app1) and await correct_pool_detached_node(app3)):
            set_failed(test_name)

        # Enable network on lightnode and wait for it to be Attached
        set_network_state(True, safir_instance=3) 
        while "Attached" not in app3.entities.get("Safir.Dob.NodeInfo:3"):
            await asyncio.sleep(2)
        log("--- node3 is now attached again")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        if not await correct_pools_connected_nodes(app1, app3):
            set_failed(test_name)

        await app1.stop()
        await app3.stop()

    # Run the test_sequence
    await asyncio.gather(app1.run(), app3.run(), test_sequence());

    # Close nodes
    node1.killprocs()
    node3.killprocs()

async def one_normal_two_light_detach_reattach_one_light(args):
    test_name =  "one_normal_two_light_detach_reattach_one_light"
    log("--- Run", test_name)
    node1 = launch_node(args, safir_instance=1, node_id=1)
    node3 = launch_node(args, safir_instance=3, node_id=3)
    node4 = launch_node(args, safir_instance=4, node_id=4)
    app1 = SafirApp(safir_instance=1, node_id=1)
    app3 = SafirApp(safir_instance=3, node_id=3)
    app4 = SafirApp(safir_instance=4, node_id=4)
    
    # Test steps
    async def test_sequence():
        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        if not await correct_pools_connected_nodes(app1, app3, app4):
            set_failed(test_name)

        # Disable network on lightnode and wait for it to be Detached
        set_network_state(False, safir_instance=3) 
        while "Detached" not in app3.entities.get("Safir.Dob.NodeInfo:3"):
            await asyncio.sleep(2)
        log("--- node3 is now detached")

        if not await correct_pools_connected_nodes(app1, app4):
            set_failed(test_name)
        if not await correct_pool_detached_node(app3):
            set_failed(test_name)

        # Enable network on lightnode and wait for it to be Attached
        set_network_state(True, safir_instance=3) 
        while "Attached" not in app3.entities.get("Safir.Dob.NodeInfo:3"):
            await asyncio.sleep(2)
        log("--- node3 is now attached again")
        
        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        if not await correct_pools_connected_nodes(app1, app3, app4):
            set_failed(test_name)

        await app1.stop()
        await app3.stop()
        await app4.stop()

    # Run the test_sequence
    await asyncio.gather(app1.run(), app3.run(), app4.run(), test_sequence());

    # Close nodes
    node1.killprocs()
    node3.killprocs()
    node4.killprocs()

async def one_normal_two_light_restart_normal(args):
    test_name =  "one_normal_two_light_restart_normal"
    log("--- Run", test_name)
    node1 = launch_node(args, safir_instance=1, node_id=1)
    node3 = launch_node(args, safir_instance=3, node_id=3)
    node4 = launch_node(args, safir_instance=4, node_id=4)
    app1 = SafirApp(safir_instance=1, node_id=1)
    app3 = SafirApp(safir_instance=3, node_id=3)
    app4 = SafirApp(safir_instance=4, node_id=4)
    
    # Test steps
    async def test_sequence():
        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        if not await correct_pools_connected_nodes(app1, app3, app4):
            set_failed(test_name)
        
        # restart node 1
        log("--- Stop node 1")
        await app1.stop()
        node1.killprocs()        
        node11 = launch_node(args, safir_instance=1, node_id=11) # use safir_instace 1 again to start dope
        app11 = SafirApp(safir_instance=1, node_id=11)
        app11_task = asyncio.create_task(app11.run())
        log("--- Node 1 has been restarted, now with new node_id 11")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        while "Attached" not in app3.entities.get("Safir.Dob.NodeInfo:3") and "Attached" not in app4.entities.get("Safir.Dob.NodeInfo:4"):
            await asyncio.sleep(2)

        if not await correct_pools_connected_nodes(app11, app3, app4):
            set_failed(test_name)

        await app11.stop()
        await app3.stop()
        await app4.stop()

        # kill the restarted node
        await app11_task
        node11.killprocs()

    # Run the test_sequence
    await asyncio.gather(app1.run(), app3.run(), app4.run(), test_sequence());

    # Close nodes
    node3.killprocs()
    node4.killprocs()


async def two_normal_two_light_detach_reattach_both_light(args):
    test_name =  "two_normal_two_light_detach_reattach_both_light"
    log("--- Run", test_name)
    node1 = launch_node(args, safir_instance=1, node_id=1)
    node2 = launch_node(args, safir_instance=2, node_id=2)
    node3 = launch_node(args, safir_instance=3, node_id=3)
    node4 = launch_node(args, safir_instance=4, node_id=4)
    app1 = SafirApp(safir_instance=1, node_id=1)
    app2 = SafirApp(safir_instance=2, node_id=2)
    app3 = SafirApp(safir_instance=3, node_id=3)
    app4 = SafirApp(safir_instance=4, node_id=4)

    async def test_sequence():
        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        if not await correct_pools_connected_nodes(app1, app2, app3, app4):
            set_failed(test_name)

        # Disable network on lightnodes and wait for them to be Detached
        set_network_state(False, safir_instance=3) 
        set_network_state(False, safir_instance=4) 
        while "Detached" not in app3.entities.get("Safir.Dob.NodeInfo:3") and "Detached" not in app4.entities.get("Safir.Dob.NodeInfo:4"):
            await asyncio.sleep(2)
        log("--- node 3 and node 4 are now detached")

        if not (await correct_pools_connected_nodes(app1, app2) and
            await correct_pool_detached_node(app3) and
            await correct_pool_detached_node(app4)):
            set_failed(test_name)

        # Enable network again and wait for nodes to become attached
        set_network_state(True, safir_instance=3) 
        set_network_state(True, safir_instance=4) 
        while "Attached" not in app3.entities.get("Safir.Dob.NodeInfo:3") and "Attached" not in app4.entities.get("Safir.Dob.NodeInfo:4"):
            await asyncio.sleep(2)
        log("--- node 3 and node 4 are now attached again")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        if not await correct_pools_connected_nodes(app1, app2, app3, app4):
            set_failed(test_name)

        await app1.stop()
        await app2.stop()
        await app3.stop()
        await app4.stop()

    # Run the test_sequence
    await asyncio.gather(app1.run(), app2.run(), app3.run(), app4.run(), test_sequence());

    # Close nodes
    node1.killprocs()
    node2.killprocs()
    node3.killprocs()
    node4.killprocs()

async def two_normal_two_light_restart_one_normal(args):
    test_name =  "two_normal_two_light_restart_one_normal"
    log("--- Run", test_name)
    node1 = launch_node(args, safir_instance=1, node_id=1)
    node2 = launch_node(args, safir_instance=2, node_id=2)
    node3 = launch_node(args, safir_instance=3, node_id=3)
    node4 = launch_node(args, safir_instance=4, node_id=4)
    app1 = SafirApp(safir_instance=1, node_id=1)
    app2 = SafirApp(safir_instance=2, node_id=2)
    app3 = SafirApp(safir_instance=3, node_id=3)
    app4 = SafirApp(safir_instance=4, node_id=4)

    async def test_sequence():
        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        if not await correct_pools_connected_nodes(app1, app2, app3, app4):
            set_failed(test_name)

        # restart node 2
        log("--- Stop node 2")
        await app2.stop()
        node2.killprocs()        
        node22 = launch_node(args, safir_instance=2, node_id=22) # use safir_instace 1 again to start dope
        app22 = SafirApp(safir_instance=2, node_id=22)
        app22_task = asyncio.create_task(app22.run())
        log("--- Node 2 has been restarted, now with new node_id 22")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        if not await correct_pools_connected_nodes(app1, app22, app3, app4):
            set_failed(test_name)
        
        await app1.stop()
        await app22.stop()
        await app3.stop()
        await app4.stop()
        

        # kill the restarted node
        # kill the restarted node
        await app22_task
        node22.killprocs()

    # Run the test_sequence
    await asyncio.gather(app1.run(), app2.run(), app3.run(), app4.run(), test_sequence());

    # Close nodes
    node1.killprocs()
    node3.killprocs()
    node4.killprocs()

async def two_normal_two_light_restart_both_normal(args):
    test_name =  "two_normal_two_light_restart_both_normal"
    log("--- Run", test_name)
    node1 = launch_node(args, safir_instance=1, node_id=1)
    node2 = launch_node(args, safir_instance=2, node_id=2)
    node3 = launch_node(args, safir_instance=3, node_id=3)
    node4 = launch_node(args, safir_instance=4, node_id=4)
    app1 = SafirApp(safir_instance=1, node_id=1)
    app2 = SafirApp(safir_instance=2, node_id=2)
    app3 = SafirApp(safir_instance=3, node_id=3)
    app4 = SafirApp(safir_instance=4, node_id=4)

    async def test_sequence():
        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        if not await correct_pools_connected_nodes(app1, app2, app3, app4):
            set_failed(test_name)

        # restart node 1 and 2
        log("--- Stop node 1")
        await app1.stop()
        await app2.stop()
        node1.killprocs()
        node2.killprocs()
        node11 = launch_node(args, safir_instance=1, node_id=11)
        node22 = launch_node(args, safir_instance=2, node_id=22)
        app11 = SafirApp(safir_instance=1, node_id=11)
        app22 = SafirApp(safir_instance=2, node_id=22)
        app11_task = asyncio.create_task(app11.run())
        app22_task = asyncio.create_task(app22.run())
        log("--- Node 1 and 2 has been restarted, now with new node_id 11 and 22")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        if not await correct_pools_connected_nodes(app11, app22, app3, app4):
            set_failed(test_name)
        
        await app11.stop()
        await app22.stop()
        await app3.stop()
        await app4.stop()
        

        # kill the restarted node
        # kill the restarted node
        await app11_task
        await app22_task
        node11.killprocs()
        node22.killprocs()

    # Run the test_sequence
    await asyncio.gather(app1.run(), app2.run(), app3.run(), app4.run(), test_sequence());

    # Close nodes
    node3.killprocs()
    node4.killprocs()

async def two_normal_two_light_toggle_network_many_times_on_both_light(args):
    test_name =  "two_normal_two_light_toggle_network_many_times_on_both_light"
    log("--- Run", test_name)
    node1 = launch_node(args, safir_instance=1, node_id=1)
    node2 = launch_node(args, safir_instance=2, node_id=2)
    node3 = launch_node(args, safir_instance=3, node_id=3)
    node4 = launch_node(args, safir_instance=4, node_id=4)
    app1 = SafirApp(safir_instance=1, node_id=1)
    app2 = SafirApp(safir_instance=2, node_id=2)
    app3 = SafirApp(safir_instance=3, node_id=3)
    app4 = SafirApp(safir_instance=4, node_id=4)

    async def test_sequence():
        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        if not await correct_pools_connected_nodes(app1, app2, app3, app4):
            set_failed(test_name)

        # Toggle network 10 times and end with disabled network
        for toggle in range(10):
            network_state = toggle % 2 == 0 # will end as disabled network
            set_network_state(network_state, safir_instance=3)
            set_network_state(network_state, safir_instance=4)
            await asyncio.sleep(2)

        while "Detached" not in app3.entities.get("Safir.Dob.NodeInfo:3") and "Detached" not in app4.entities.get("Safir.Dob.NodeInfo:4"):
            await asyncio.sleep(2)
        log("--- node 3 and node 4 are now detached")

        # Give some time for PD
        if not (await correct_pools_connected_nodes(app1, app2) and
            await correct_pool_detached_node(app3) and
            await correct_pool_detached_node(app4)):
            set_failed(test_name)

        # Toggle network 10 times and end with enabled network
        for toggle in range(10):
            network_state = toggle % 2 == 1 # will end as enabled network
            set_network_state(network_state, safir_instance=3)
            set_network_state(network_state, safir_instance=4)
            await asyncio.sleep(2)

        while "Attached" not in app3.entities.get("Safir.Dob.NodeInfo:3") and "Detached" not in app4.entities.get("Safir.Dob.NodeInfo:4"):
            await asyncio.sleep(2)
        log("--- node 3 and node 4 are now attached")
        
        # Give some time for PD
        await asyncio.sleep(5)
        if not await correct_pools_connected_nodes(app1, app2, app3, app4):            
            set_failed(test_name)
        
        if not await correct_pools_connected_nodes(app1, app2, app3, app4):
            set_failed(test_name)

        await app1.stop()
        await app2.stop()
        await app3.stop()
        await app4.stop()

    # Run the test_sequence
    await asyncio.gather(app1.run(), app2.run(), app3.run(), app4.run(), test_sequence());

    # Close nodes
    node1.killprocs()
    node2.killprocs()
    node3.killprocs()
    node4.killprocs()

# ===========================================
# main
# ===========================================
async def main(args):
    await one_normal_one_light_detach_reattach_light(args)
    await one_normal_two_light_detach_reattach_one_light(args)
    await one_normal_two_light_restart_normal(args)
    await two_normal_two_light_detach_reattach_both_light(args)
    await two_normal_two_light_restart_one_normal(args)
    # await two_normal_two_light_restart_both_normal(args)
    await two_normal_two_light_toggle_network_many_times_on_both_light(args)

    #---- Repeat one test and stop on failure. Clear log after each run
    # for i in range(10):
    #     files = glob.glob("/home/joel/dev/log/*")
    #     for f in files:
    #         os.remove(f)

    #     await two_normal_two_light_restart_both_normal(args)
    #     if len(failed_tests) > 0:
    #         return


if __name__ == "__main__":
    asyncio.run(main(parse_arguments()))
    for failed in failed_tests:
        log("*** Failed: ", failed)
    sys.exit(1 if len(failed_tests) > 0 else 0)
