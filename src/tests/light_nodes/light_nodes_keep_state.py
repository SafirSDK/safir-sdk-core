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
import os, sys, argparse, socket, glob, logging, uuid
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
def launch_node(args, safir_instance, node_id):
    try:
        session_id = str(uuid.uuid4())
        os.environ["SAFIR_COM_NETWORK_SIMULATION"] = session_id # Enable network simulation
        os.environ["SAFIR_INSTANCE"] = str(safir_instance)
        print("--- Launching node", str(node_id))
        env = TestEnv(args.safir_control,
                    args.dose_main,
                    args.dope_main if safir_instance == 1 else None,
                    args.safir_show_config,
                    start_syslog_server = True if safir_instance == 1 else False,
                    ignore_control_cmd = True if safir_instance == 1 else False,
                    wait_for_persistence = safir_instance < 3,
                    force_node_id = node_id)
        env.session_id = session_id
        env.launchProcess("safir_websocket", args.safir_websocket)
        yield env
    finally:
        log("--- kill node" + str(node_id))
        env.killprocs()

# Simulate network up/down
def set_network_state(state, session_id):
    cmd = ("up " if state == True else "down ") + session_id
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
        return p["entity"]["_DouType"]
    if "message" in p:
        return p["message"]["_DouType"]
    if "request" in p:
        return p["request"]["_DouType"]

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

async def check_pool(app, expected_registrations, expected_entities):
    def _check():
        if len(expected_registrations) != len(app.registrations): return False
        for id, content in expected_registrations:
            if app.registrations.get(id) is None:
                return False

        if len(expected_entities) != len(app.entities): return False
        for id, content in expected_entities:
            ent = app.entities.get(id)
            if ent is None: return False
            if content not in ent: return False

        return True
    
    max_tries = 5
    for check in range(max_tries):
        if _check():
            return True
        else:
            if check > 2: log("--- Extended wait for PD to finish, wait time " + str((check + 1)*10) + " sec")
            await asyncio.sleep(10)

    log("*** ERROR: Incorrect pool on node " + str(app.node_id) + ", safir_instance: " + str(app.safir_instance))
    print("  Expected registrations:")
    for t, v in expected_registrations: print("    " + t + " - " + str(v))
    print("  Expected entities:")
    for t, v in expected_entities: print("    " + t + " - " + str(v))
    app.dump_pool()
    raise AssertionError("Incorrect pool")    

# ==============================================================================================
# Simple Safir application. The constructor will connect to the DOB and subscribe for all 
# entites and all registrations.
# It will start a running task that handles send and receive to the Dob. Stop will end the task
# Safir_instance:  1,2 = normal node,  3,4 = lightnode clear state,  5,6 = lightnodes keep state
# ==============================================================================================
class SafirApp:
    def __init__(self, safir_instance, node_id, num_entities=3):
        self.safir_instance = safir_instance
        self.node_id = node_id
        self.number_of_entities = num_entities
        self.sendQueue = asyncio.Queue()
        self.entities = dict()
        self.registrations = dict()
        self.stopped = False
        self._setup_dob()
        self.task = asyncio.create_task(self._run())

    async def stop(self):
        if not self.stopped:
            await self.sendQueue.put(None)
            await self.sendQueue.join()
            await self.task
            self.stopped = True

    async def send(self, msg):
        await self.sendQueue.put(msg)

    async def wait_for_node_state(self, state, timeout=60):
        nodeInfoEntityId = "Safir.Dob.NodeInfo:" + str(self.node_id)
        t = 0
        while t < timeout:
            t = t + 3
            await asyncio.sleep(3)
            nodeInfo = self.entities.get(nodeInfoEntityId)
            if nodeInfo is not None and state in nodeInfo:
                return

        log("*** wait_for_node_state timed out, dump pool")
        self.dump_pool()
        raise AssertionError("app" + str(self.node_id) +" did not get NodeInfo.State='" + state +"' within " + str(timeout) + " seconds.")

    def dump_pool(self):
        print("=== Node: " + str(self.node_id) + ", safir_instance: " + str(self.safir_instance) + " ===")
        print("  Registrations:")
        for val in dict_to_sorted_list(self.registrations):
            print("    " + val)            
        print("  Entities:")
        for val in dict_to_sorted_list(self.entities):
            print("    " + val)
        log("--------------")

    async def _run(self):
        uri = "ws://localhost:1667" + str(self.safir_instance)
        for connectTry in range(10):
            try:
                async with websockets.connect(uri) as ws:
                    self.ws = ws
                    log("--- Node " + str(self.node_id) + " is connected to the DOB")
                    await asyncio.gather(self._reader(), self._sender())
                    break;
            except ConnectionRefusedError as e:
                await asyncio.sleep(5)
                if connectTry == 9:
                    log("*** Failed to connect, ConnectionRefusedError! uri= " + uri)
                    log(e)
                    raise
            except Exception as e:
                await asyncio.sleep(5)
                if connectTry == 9:
                    log("*** Failed to connect, Exception! uri= " + uri)
                    log(e)
                    raise

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
                elif "result" in msg and msg["result"] != "OK":
                    result_id = msg["id"] if "id" in msg else "None"
                    log("--- node_" + str(self.node_id) + " received result " + msg["result"] + " with id=" + str(result_id))
                elif "error" in msg:
                    log("*** node_ " + str(self.node_id) + " received error response from safir_websocket:", message)
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

    def _setup_dob(self):
        app_name = "app" + str(self.node_id)
        self.sendQueue.put_nowait('{"method": "open", "params": {"connectionName": "' + app_name + '"}, "id": 1}')
        self.sendQueue.put_nowait('{"method": "subscribeEntity", "params": {"typeId": "Safir.Dob.Entity"}, "id": 2}')
        self.sendQueue.put_nowait('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Entity"}, "id": 3}')
        self.sendQueue.put_nowait('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Service"}, "id": 4}')

# ===================================================================
# Test cases
# ===================================================================
async def one_normal_one_light_detach_reattach_light_changed_entities(args):
    with test_case("one_normal_one_light_detach_reattach_light_changed_entities"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=5, node_id=5) as node5:
    
        app1 = SafirApp(safir_instance=1, node_id=1)
        app5 = SafirApp(safir_instance=5, node_id=5)

        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity"}, "id": 5}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 1}, "id": 6}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 2}, "id": 7}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 3}, "id": 8}')
        initialRegGlobal = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered")]
        initialEntGlobal = [("Safir.Dob.NodeInfo:1", "_"), ("DoseTest.GlobalEntity:1", "version0"), ("DoseTest.GlobalEntity:2", "version0"), ("DoseTest.GlobalEntity:3", "version0")]

        await app5.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.LimitedEntity"}, "id": 5}')
        await app5.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.LimitedEntity", "Info": "version0"}, "instanceId": 1}, "id": 6}')
        await app5.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.LimitedEntity", "Info": "version0"}, "instanceId": 2}, "id": 7}')
        await app5.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.LimitedEntity", "Info": "version0"}, "instanceId": 3}, "id": 8}')
        initialRegLimited = [("Safir.Dob.NodeInfo:5", "Registered"), ("DoseTest.LimitedEntity:DEFAULT_HANDLER", "Registered")]
        initialEntLimited = [("Safir.Dob.NodeInfo:5", "_"), ("DoseTest.LimitedEntity:1", "version0"), ("DoseTest.LimitedEntity:2", "version0"), ("DoseTest.LimitedEntity:3", "version0")]

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached")        
        
        await check_pool(app1, initialRegGlobal + initialRegLimited, initialEntGlobal + initialEntLimited)
        await check_pool(app5, initialRegGlobal + initialRegLimited, initialEntGlobal + initialEntLimited)

        # Disable network on lightnode and wait for it to be Detached
        set_network_state(False, node5.session_id) 
        await app5.wait_for_node_state("Detached")
        log("--- node5 is now detached")

        # Do some changes on bothe nodes while they are detached
        await app1.send('{"method": "deleteEntity", "params": {"typeId": "DoseTest.GlobalEntity", "instanceId": 2}, "id": 9}') # delete
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version1"}, "instanceId": 3}, "id": 10}') # update
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 4}, "id": 11}') # new
        changedEntGlobal = [("Safir.Dob.NodeInfo:1", "_"), ("DoseTest.GlobalEntity:1", "version0"), ("DoseTest.GlobalEntity:3", "version1"), ("DoseTest.GlobalEntity:4", "version0")]

        await app5.send('{"method": "deleteEntity", "params": {"typeId": "DoseTest.LimitedEntity", "instanceId": 2}, "id": 9}') # delete
        await app5.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.LimitedEntity", "Info": "version1"}, "instanceId": 3}, "id": 10}') # update
        await app5.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.LimitedEntity", "Info": "version0"}, "instanceId": 4}, "id": 11}') # new
        changedEntLimited = [("Safir.Dob.NodeInfo:5", "_"), ("DoseTest.LimitedEntity:1", "version0"), ("DoseTest.LimitedEntity:3", "version1"), ("DoseTest.LimitedEntity:4", "version0")]

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pool(app1, initialRegGlobal, changedEntGlobal)
        await check_pool(app5, initialRegGlobal + initialRegLimited, initialEntGlobal + changedEntLimited)

        # Enable network on lightnode and wait for it to be Attached
        set_network_state(True, node5.session_id) 
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached again")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pool(app1, initialRegGlobal + initialRegLimited, changedEntGlobal + changedEntLimited)
        await check_pool(app5, initialRegGlobal + initialRegLimited, changedEntGlobal + changedEntLimited)

        await asyncio.gather(app1.stop(), app5.stop())

async def one_normal_one_light_detach_reattach_light_changed_registrations(args):
    with test_case("one_normal_one_light_detach_reattach_light_changed_registrations"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=5, node_id=5) as node5:
    
        app1 = SafirApp(safir_instance=1, node_id=1)
        app5 = SafirApp(safir_instance=5, node_id=5)

        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity"}, "id": 5}')
        await app1.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.GlobalService", "handlerId": "app1"}, "id": 6}')
        initialRegGlobal = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalService:app1", "Registered")]
        initialEntGlobal = [("Safir.Dob.NodeInfo:1", "_")]

        await app5.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.LimitedEntity"}, "id": 5}')
        await app5.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.LimitedService", "handlerId": "app5"}, "id": 6}')
        initialRegLimited = [("Safir.Dob.NodeInfo:5", "Registered"), ("DoseTest.LimitedEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.LimitedService:app5", "Registered")]
        initialEntLimited = [("Safir.Dob.NodeInfo:5", "_")]

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached")        
        await check_pool(app1, initialRegGlobal + initialRegLimited, initialEntGlobal + initialEntLimited)
        await check_pool(app5, initialRegGlobal + initialRegLimited, initialEntGlobal + initialEntLimited)

        # Disable network on lightnode and wait for it to be Detached
        set_network_state(False, node5.session_id) 
        await app5.wait_for_node_state("Detached")
        log("--- node5 is now detached")

        # Do some changes on bothe nodes while they are detached
        await app1.send('{"method": "unregisterHandler", "params": {"typeId": "DoseTest.GlobalService"}, "id": 7}')
        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity2"}, "id": 8}')
        await app1.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.GlobalService", "handlerId": "updatedApp1"}, "id": 9}')
        changedRegGlobal = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalEntity2:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalService:updatedApp1", "Registered")]

        await app5.send('{"method": "unregisterHandler", "params": {"typeId": "DoseTest.LimitedService"}, "id": 7}')
        await app5.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.LimitedEntity2"}, "id": 8}')
        await app5.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.LimitedService", "handlerId": "updatedApp5"}, "id": 9}')
        changedRegLimited = [("Safir.Dob.NodeInfo:5", "Registered"), ("DoseTest.LimitedEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.LimitedEntity2:DEFAULT_HANDLER", "Registered"), ("DoseTest.LimitedService:updatedApp5", "Registered")]

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pool(app1, changedRegGlobal, initialEntGlobal)
        await check_pool(app5, initialRegGlobal + changedRegLimited, initialEntGlobal + initialEntLimited)

        # Enable network on lightnode and wait for it to be Attached
        set_network_state(True, node5.session_id)
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached again")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pool(app1, changedRegGlobal + changedRegLimited, initialEntGlobal + initialEntLimited)
        await check_pool(app5, changedRegGlobal + changedRegLimited, initialEntGlobal + initialEntLimited)

        await asyncio.gather(app1.stop(), app5.stop())

async def one_normal_one_light_detach_reattach_light_big_pool(args):
    with test_case("one_normal_one_light_detach_reattach_light_big_pool"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=5, node_id=5) as node5:
    
        app1 = SafirApp(safir_instance=1, node_id=1)
        app5 = SafirApp(safir_instance=5, node_id=5)

        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity"}, "id": 5}')
        await app1.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.GlobalService", "handlerId": "app1"}, "id": 6}')
        initialRegGlobal = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalService:app1", "Registered")]
        initialEntGlobal = [("Safir.Dob.NodeInfo:1", "_")]

        await app5.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.LimitedEntity"}, "id": 5}')
        await app5.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.LimitedService", "handlerId": "app5"}, "id": 6}')
        initialRegLimited = [("Safir.Dob.NodeInfo:5", "Registered"), ("DoseTest.LimitedEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.LimitedService:app5", "Registered")]
        initialEntLimited = [("Safir.Dob.NodeInfo:5", "_")]

        # Create 8000 global entities
        for i in range(8000):
            await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": ' + str(i + 1) + '}, "id": ' + str(i + 6) +'}')
            initialEntGlobal.append(("DoseTest.GlobalEntity:" + str(i + 1), "version0"))

        # Create 2000 limited entities
        for i in range(2000):
            await app5.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.LimitedEntity", "Info": "version0"}, "instanceId": ' + str(i + 1) + '}, "id": ' + str(i + 6) +'}')
            initialEntLimited.append(("DoseTest.LimitedEntity:" + str(i + 1), "version0"))

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached")        
        await check_pool(app1, initialRegGlobal + initialRegLimited, initialEntGlobal + initialEntLimited)
        await check_pool(app5, initialRegGlobal + initialRegLimited, initialEntGlobal + initialEntLimited)

        # Disable network on lightnode and wait for it to be Detached
        set_network_state(False, node5.session_id)
        await app5.wait_for_node_state("Detached")
        log("--- node5 is now detached")

        # Do some changes on bothe nodes while they are detached
        await app1.send('{"method": "unregisterHandler", "params": {"typeId": "DoseTest.GlobalService"}, "id": 7}')
        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity2"}, "id": 8}')
        await app1.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.GlobalService", "handlerId": "updatedApp1"}, "id": 9}')
        changedRegGlobal = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalEntity2:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalService:updatedApp1", "Registered")]
        changedEntGlobal = [("Safir.Dob.NodeInfo:1", "_")]
        for i in range(1, 4001): # Unchanged entities
            changedEntGlobal.append(("DoseTest.GlobalEntity:" + str(i), "version0"))
        for i in range(4001, 6001): # Updated entities
            await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version1"}, "instanceId": ' + str(i) + '}, "id": ' + str(i + 9000) +'}')
            changedEntGlobal.append(("DoseTest.GlobalEntity:" + str(i), "version1"))
        for i in range(6001, 8001): # Deleted entities
            await app1.send('{"method": "deleteEntity", "params": {"typeId": "DoseTest.GlobalEntity", "instanceId": ' + str(i) + '}, "id": ' + str(i + 11000) +'}')
        for i in range(1, 2001): # New entities
            await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity2", "Info": "version0"}, "instanceId": ' + str(i) + '}, "id": ' + str(i + 13000) +'}')
            changedEntGlobal.append(("DoseTest.GlobalEntity2:" + str(i), "version0"))

        await app5.send('{"method": "unregisterHandler", "params": {"typeId": "DoseTest.LimitedService"}, "id": 7}')
        await app5.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.LimitedEntity2"}, "id": 8}')
        await app5.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.LimitedService", "handlerId": "updatedApp5"}, "id": 9}')
        changedRegLimited = [("Safir.Dob.NodeInfo:5", "Registered"), ("DoseTest.LimitedEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.LimitedEntity2:DEFAULT_HANDLER", "Registered"), ("DoseTest.LimitedService:updatedApp5", "Registered")]
        changedEntLimited = [("Safir.Dob.NodeInfo:5", "_")]
        for i in range(1, 1001): # Unchanged entities
            changedEntLimited.append(("DoseTest.LimitedEntity:" + str(i), "version0"))
        for i in range(1001, 1501): # Updated entities
            await app5.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.LimitedEntity", "Info": "version1"}, "instanceId": ' + str(i) + '}, "id": ' + str(i + 9000) +'}')
            changedEntLimited.append(("DoseTest.LimitedEntity:" + str(i), "version1"))
        for i in range(1501, 2001): # Deleted entities
            await app5.send('{"method": "deleteEntity", "params": {"typeId": "DoseTest.LimitedEntity", "instanceId": ' + str(i) + '}, "id": ' + str(i + 11000) +'}')
        for i in range(1, 501): # New entities
            await app5.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.LimitedEntity2", "Info": "version0"}, "instanceId": ' + str(i) + '}, "id": ' + str(i + 13000) +'}')
            changedEntLimited.append(("DoseTest.LimitedEntity2:" + str(i), "version0"))

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pool(app1, changedRegGlobal, changedEntGlobal)
        await check_pool(app5, initialRegGlobal + changedRegLimited, initialEntGlobal + changedEntLimited)

        # Enable network on lightnode and wait for it to be Attached
        set_network_state(True, node5.session_id)
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached again")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pool(app1, changedRegGlobal + changedRegLimited, changedEntGlobal + changedEntLimited)
        await check_pool(app5, changedRegGlobal + changedRegLimited, changedEntGlobal + changedEntLimited)

        await asyncio.gather(app1.stop(), app5.stop())

# async def two_normal_two_light_detach_reattach_light(args):
#     with test_case("two_normal_two_light_detach_reattach_light"),\
#         launch_node(args, safir_instance=1, node_id=1) as node1,\
#         launch_node(args, safir_instance=2, node_id=2) as node2,\
#         launch_node(args, safir_instance=5, node_id=5) as node5,\
#         launch_node(args, safir_instance=6, node_id=6) as node6:

#         app1 = SafirApp(safir_instance=1, node_id=1)
#         app2 = SafirApp(safir_instance=2, node_id=2)
#         app5 = SafirApp(safir_instance=5, node_id=5)
#         app6 = SafirApp(safir_instance=6, node_id=6)

#         await asyncio.sleep(5)
#         await asyncio.gather(app5.wait_for_node_state("Attached"), app6.wait_for_node_state("Attached"))
#         log("--- node5 and node6 are now attached")

#         set_network_state(False, safir_instance=5)
#         set_network_state(False, safir_instance=6)
#         await asyncio.sleep(5)
#         await asyncio.gather(app5.wait_for_node_state("Detached"), app6.wait_for_node_state("Detached"))
#         log("--- node5 and node6 are now detached")

#         # Do some changes while both light nodes are detached

#         # connect all nodes again
#         set_network_state(True, safir_instance=5)
#         set_network_state(True, safir_instance=6)
#         await asyncio.sleep(5)
#         await asyncio.gather(app5.wait_for_node_state("Attached"), app6.wait_for_node_state("Attached"))
#         log("--- node5 and node6 are now attached again")

#         # check that pools are in sync

#         await asyncio.gather(app1.stop(), app2.stop(), app5.stop(), app6.stop())

# ===========================================
# main
# ===========================================
async def main(args):
    for f in glob.glob("/home/joel/dev/log/*"): os.remove(f)
    await one_normal_one_light_detach_reattach_light_changed_entities(args)
    await one_normal_one_light_detach_reattach_light_changed_registrations(args)
    await one_normal_one_light_detach_reattach_light_big_pool(args)
    # await two_normal_two_light_detach_reattach_light(args)

if __name__ == "__main__":
    asyncio.run(main(parse_arguments()))
    for failed in failed_tests:
        log("*** Failed: ", failed)
    sys.exit(1 if len(failed_tests) > 0 else 0)
