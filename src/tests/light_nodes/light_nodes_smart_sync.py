#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2023 (http://safirsdkcore.com)
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
    log_dir = os.path.normpath(os.path.join(os.getcwd(), "test_output", "smart_sync", name))
    for f in glob.glob(os.path.join(log_dir, "*")): os.remove(f)
    os.environ["LLL_LOGDIR"] = log_dir    

    try:
        log("=== Start: " + name + " ===")
        log("logdir: " + log_dir)
        yield
    except Exception as e:
        failed_tests.add(name)
        logging.exception('Got exception')
        log(e)
        log("*** Test failed: " + name)
    finally:
        log("--- Finished: " + name + " ---")

@contextmanager
def launch_node(args, safir_instance, node_id, start_dope=False):
    try:
        session_id = str(uuid.uuid4())
        os.environ["SAFIR_COM_NETWORK_SIMULATION"] = session_id # Enable network simulation
        os.environ["SAFIR_INSTANCE"] = str(safir_instance)
        print("--- Launching node", str(node_id))
        env = TestEnv(args.safir_control,
                    args.dose_main,
                    args.dope_main if start_dope or safir_instance == 1 else None,
                    args.safir_show_config,
                    start_syslog_server = True if safir_instance == 1 else False,
                    ignore_control_cmd = True if safir_instance == 1 else False,
                    wait_for_persistence = safir_instance < 3,
                    force_node_id = node_id)
        env.session_id = session_id
        env.launchProcess("safir_websocket", args.safir_websocket)

        #if node_id == 5:
            #env.launchProcess("dobexplorer", args.dobexplorer)
        yield env
    finally:
        log("--- kill node" + str(node_id))
        env.killprocs()

# Simulate network up/down
async def set_network_state(state, session_id):
    cmd = ("up " if state == True else "down ") + session_id
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.sendto(bytes(cmd, "utf-8"), ("239.6.6.6", 16666))
    await asyncio.sleep(0.1)
    sock.sendto(bytes(cmd, "utf-8"), ("239.6.6.6", 16666))
    await asyncio.sleep(0.1)
    sock.sendto(bytes(cmd, "utf-8"), ("239.6.6.6", 16666))

def parse_arguments():
    parser = argparse.ArgumentParser("test script")
    parser.add_argument("--safir-control", required=True)
    parser.add_argument("--dose_main", required=True)
    parser.add_argument("--dope_main", required=True)
    parser.add_argument("--safir-show-config", required=True)
    parser.add_argument("--dobexplorer", required=False)
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
    
    pool_timestamp = app.last_pool_update
    num_tries = 0
    while pool_timestamp < app.last_pool_update or num_tries < 60:
        num_tries = num_tries +1
        pool_timestamp = app.last_pool_update
        if _check():
            log("--- Node " + str(app.node_id) + " is synced OK!")
            return True
        else:
            if num_tries > 2: log("--- Extended wait for PD to finish, wait time " + str((num_tries + 1)*10) + " sec, node_" + str(app.node_id))
            await asyncio.sleep(10)

    log("*** ERROR: Incorrect pool on node " + str(app.node_id) + ", safir_instance: " + str(app.safir_instance))
    app.dump_pool()
    print("  Expected registrations:")
    for t, v in expected_registrations: print("    " + t + " - " + str(v))
    print("  Expected entities:")
    if len(expected_entities) < 500:
        for t, v in expected_entities: print("    " + t + " - " + str(v))
    else:
        print("Number of expected entities: " + str(len(expected_entities)))
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
        self.notifications = None
        self.entities = dict()
        self.registrations = dict()
        self.last_pool_update = time.time()
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

    async def wait_for_node_state(self, state):
        timeout = 600 #seconds
        nodeInfoEntityId = "Safir.Dob.NodeInfo:" + str(self.node_id)
        t = 0
        while t < timeout:
            t = t + 3
            await asyncio.sleep(3)
            nodeInfo = self.entities.get(nodeInfoEntityId)
            if nodeInfo is not None and state in nodeInfo:
                print(f"Node state received after {t} seconds")
                return

        log("*** wait_for_node_state timed out, dump pool")
        self.dump_pool()
        raise AssertionError("app" + str(self.node_id) +" did not get NodeInfo.State='" + state +"' within " + str(timeout) + " seconds. Current NodeInfo: " + str(self.entities.get(nodeInfoEntityId)))

    def dump_pool(self):
        print("=== Node: " + str(self.node_id) + ", safir_instance: " + str(self.safir_instance) + " ===")
        print("  Registrations:")
        for val in dict_to_sorted_list(self.registrations):
            print("    " + val)            
        print("  Entities:")
        for val in dict_to_sorted_list(self.entities):
            print("    " + val)

        if self.notifications:
            print("  Nofifications:")
            for val in self.notifications:
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
                    break
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
                if self.notifications is not None:
                    self.notifications.append(message)

                msg = json.loads(message)
                callback = method(msg)
                if callback == "onNewEntity" and not ignore_type(msg):
                    self.last_pool_update = time.time()                    
                    entity_id = dou_type(msg) + ":" + str(instance_id(msg))                    
                    self.entities[entity_id] = json.dumps(entity(msg))
                    # log("--- node_" + str(self.node_id) + " onNewEntity: " + entity_id)
                elif callback == "onUpdatedEntity" and not ignore_type(msg):
                    self.last_pool_update = time.time()                    
                    entity_id = dou_type(msg) + ":" + str(instance_id(msg))                    
                    self.entities[entity_id] = json.dumps(entity(msg))
                    # log("--- node_" + str(self.node_id) + " onUpdatedEntity: " + entity_id)
                elif callback == "onDeletedEntity" and not ignore_type(msg):
                    self.last_pool_update = time.time()
                    entity_id = dou_type(msg) + ":" + str(instance_id(msg))
                    self.entities.pop(entity_id, None)
                    # log("--- node_" + str(self.node_id) + " onDeletedEntity: " + entity_id)
                elif callback == "onRegistered" and not ignore_type(msg):
                    self.last_pool_update = time.time()
                    handler = dou_type(msg) + ":" + str(handler_id(msg))
                    self.registrations[handler] = "Registered"
                    # log("--- node_" + str(self.node_id) + " onRegistered: " + handler)
                elif callback == "onUnregistered" and not ignore_type(msg):
                    self.last_pool_update = time.time()
                    handler = dou_type(msg) + ":" + str(handler_id(msg))
                    self.registrations.pop(handler, None)
                    # log("--- node_" + str(self.node_id) + " onUnregistered: " + handler)
                elif callback == "onUpdateRequest":
                    log("--- node_" + str(self.node_id) + " got a request: " + message)
                    await self.send('{"result": {"_DouType": "Safir.Dob.SuccessResponse"}, "id": ' + str(msg["id"]) + '}')
                    await self.send('{"method": "setEntity", "params": {"entity": ' + json.dumps(entity(msg)) + ', "instanceId": ' + str(instance_id(msg)) + '}, "id": 123}')
                elif "result" in msg and msg["result"] != "OK":
                    result_id = msg["id"] if "id" in msg else "None"
                    # log("--- node_" + str(self.node_id) + " received result " + msg["result"] + " with id=" + str(result_id))
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
        self.sendQueue.put_nowait('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Entity"}, "id": 2}')
        self.sendQueue.put_nowait('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Service"}, "id": 3}')
        self.sendQueue.put_nowait('{"method": "subscribeEntity", "params": {"typeId": "Safir.Dob.Entity"}, "id": 4}')

# ===================================================================
# Test cases
# ===================================================================
async def two_normal_one_light_detach_reattach_light(args):
    with test_case("two_normal_one_light_detach_reattach_light"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=2, node_id=2) as node2,\
        launch_node(args, safir_instance=5, node_id=5) as node5:
    
        app1 = SafirApp(safir_instance=1, node_id=1)
        app2 = SafirApp(safir_instance=2, node_id=2)
        app5 = SafirApp(safir_instance=5, node_id=5)

        # App 1
        await app1.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.GlobalService", "handlerId": "APP1"}, "id": 101}')
        await app1.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseStressTest.Service", "handlerId": "APP1"}, "id": 102}')
        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity"}, "id": 103}')
        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseStressTest.RootEntity", "handlerId": "APP1"}, "id": 104}')
        
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 1}, "id": 105}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 2}, "id": 106}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 3}, "id": 107}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseStressTest.RootEntity", "SequenceNumber": 111}, "instanceId": 1, "handlerId": "APP1"}, "id": 108}')
        
        initialRegApp1 = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered"), ("DoseStressTest.RootEntity:APP1", "Registered"), ("DoseTest.GlobalService:APP1", "Registered"), ("DoseStressTest.Service:APP1", "Registered")]
        initialEntApp1 = [("Safir.Dob.NodeInfo:1", "_"), ("DoseTest.GlobalEntity:1", "version0"), ("DoseTest.GlobalEntity:2", "version0"), ("DoseTest.GlobalEntity:3", "version0"), ("DoseStressTest.RootEntity:1", "111")]

        # App 2
        await app2.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.GlobalService", "handlerId": "APP2"}, "id": 201}')
        await app2.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseStressTest.Service", "handlerId": "APP2"}, "id": 202}')
        await app2.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity2"}, "id": 203}')
        await app2.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseStressTest.RootEntity", "handlerId": "APP2"}, "id": 204}')
        
        await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity2", "Info": "version0"}, "instanceId": 1}, "id": 205}')
        await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity2", "Info": "version0"}, "instanceId": 2}, "id": 206}')
        await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity2", "Info": "version0"}, "instanceId": 3}, "id": 207}')
        await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseStressTest.RootEntity", "SequenceNumber": 222}, "instanceId": 2, "handlerId": "APP2"}, "id": 208}')
        
        initialRegApp2 = [("Safir.Dob.NodeInfo:2", "Registered"), ("DoseTest.GlobalEntity2:DEFAULT_HANDLER", "Registered"), ("DoseStressTest.RootEntity:APP2", "Registered"), ("DoseTest.GlobalService:APP2", "Registered"), ("DoseStressTest.Service:APP2", "Registered")]
        initialEntApp2 = [("Safir.Dob.NodeInfo:2", "_"), ("DoseTest.GlobalEntity2:1", "version0"), ("DoseTest.GlobalEntity2:2", "version0"), ("DoseTest.GlobalEntity2:3", "version0"), ("DoseStressTest.RootEntity:2", "222")]

        # App 5 - Light node
        initialRegLight = [("Safir.Dob.NodeInfo:5", "Registered")]
        initialEntLight = [("Safir.Dob.NodeInfo:5", "_")]

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached")
        
        await check_pool(app1, initialRegApp1 + initialRegApp2 + initialRegLight, initialEntApp1 + initialEntApp2 + initialEntLight)
        await check_pool(app2, initialRegApp1 + initialRegApp2 + initialRegLight, initialEntApp1 + initialEntApp2 + initialEntLight)
        await check_pool(app5, initialRegApp1 + initialRegApp2 + initialRegLight, initialEntApp1 + initialEntApp2 + initialEntLight)
        
        # Disable network on lightnode and wait for it to be Detached
        log("---------- set_network_state=down -----------")
        await set_network_state(False, node5.session_id) 
        await app5.wait_for_node_state("Detached")
        log("--- node5 is now detached")

        # Do some changes while light node is detached

        # App 1
        await app1.send('{"method": "unregisterHandler", "params": {"typeId": "DoseStressTest.Service", "handlerId": "APP1"}, "id": 9}') # unreg service
        await app1.send('{"method": "unregisterHandler", "params": {"typeId": "DoseStressTest.RootEntity", "handlerId": "APP1"}, "id": 9}') # unreg entity
        await app1.send('{"method": "deleteEntity", "params": {"typeId": "DoseTest.GlobalEntity", "instanceId": 1}, "id": 9}') # delete
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version1"}, "instanceId": 2}, "id": 10}') # update
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 4}, "id": 11}') # new
        changedRegApp1 = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalService:APP1", "Registered")]
        changedEntApp1 = [("Safir.Dob.NodeInfo:1", "_"), ("DoseTest.GlobalEntity:2", "version1"), ("DoseTest.GlobalEntity:3", "version0"), ("DoseTest.GlobalEntity:4", "version0")]

        # App 2
        await app2.send('{"method": "unregisterHandler", "params": {"typeId": "DoseStressTest.Service", "handlerId": "APP2"}, "id": 9}') # unreg service
        await app2.send('{"method": "unregisterHandler", "params": {"typeId": "DoseStressTest.RootEntity", "handlerId": "APP2"}, "id": 9}') # unreg entity
        await app2.send('{"method": "deleteEntity", "params": {"typeId": "DoseTest.GlobalEntity2", "instanceId": 1}, "id": 9}') # delete
        await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity2", "Info": "version1"}, "instanceId": 2}, "id": 10}') # update
        await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity2", "Info": "version0"}, "instanceId": 4}, "id": 11}') # new
        changedRegApp2 = [("Safir.Dob.NodeInfo:2", "Registered"), ("DoseTest.GlobalEntity2:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalService:APP2", "Registered")]
        changedEntApp2 = [("Safir.Dob.NodeInfo:2", "_"), ("DoseTest.GlobalEntity2:2", "version1"), ("DoseTest.GlobalEntity2:3", "version0"), ("DoseTest.GlobalEntity2:4", "version0")]

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pool(app1, changedRegApp1 + changedRegApp2, changedEntApp1 + changedEntApp2)
        await check_pool(app2, changedRegApp1 + changedRegApp2, changedEntApp1 + changedEntApp2)
        await check_pool(app5, initialRegApp1 + initialRegApp2 + initialRegLight, initialEntApp1 + initialEntApp2 + initialEntLight)

        # Enable network on lightnode and wait for it to be Attached
        log("---------- set_network_state=up -----------")
        await set_network_state(True, node5.session_id) 
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached again")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pool(app1, changedRegApp1 + changedRegApp2 + initialRegLight, changedEntApp1 + changedEntApp2 + initialEntLight)
        await check_pool(app2, changedRegApp1 + changedRegApp2 + initialRegLight, changedEntApp1 + changedEntApp2 + initialEntLight)
        await check_pool(app5, changedRegApp1 + changedRegApp2 + initialRegLight, changedEntApp1 + changedEntApp2 + initialEntLight)

        await asyncio.gather(app1.stop(), app5.stop())

async def two_normal_one_light_kill_one_normal_while_light_is_detached(args):
    with test_case("two_normal_one_light_kill_one_normal_while_light_is_detached"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=2, node_id=2) as node2,\
        launch_node(args, safir_instance=5, node_id=5) as node5:
    
        app1 = SafirApp(safir_instance=1, node_id=1)
        app2 = SafirApp(safir_instance=2, node_id=2)
        app5 = SafirApp(safir_instance=5, node_id=5)

        # App 1
        await app1.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.GlobalService", "handlerId": "APP1"}, "id": 101}')
        await app1.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseStressTest.Service", "handlerId": "APP1"}, "id": 102}')
        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity"}, "id": 103}')
        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseStressTest.RootEntity", "handlerId": "APP1"}, "id": 104}')
        
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 1}, "id": 105}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 2}, "id": 106}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 3}, "id": 107}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseStressTest.RootEntity", "SequenceNumber": 111}, "instanceId": 1, "handlerId": "APP1"}, "id": 108}')
        
        initialRegApp1 = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered"), ("DoseStressTest.RootEntity:APP1", "Registered"), ("DoseTest.GlobalService:APP1", "Registered"), ("DoseStressTest.Service:APP1", "Registered")]
        initialEntApp1 = [("Safir.Dob.NodeInfo:1", "_"), ("DoseTest.GlobalEntity:1", "version0"), ("DoseTest.GlobalEntity:2", "version0"), ("DoseTest.GlobalEntity:3", "version0"), ("DoseStressTest.RootEntity:1", "111")]

        # App 2
        await app2.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.GlobalService", "handlerId": "APP2"}, "id": 201}')
        await app2.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseStressTest.Service", "handlerId": "APP2"}, "id": 202}')
        await app2.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity2"}, "id": 203}')
        await app2.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseStressTest.RootEntity", "handlerId": "APP2"}, "id": 204}')
        
        await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity2", "Info": "version0"}, "instanceId": 1}, "id": 205}')
        await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity2", "Info": "version0"}, "instanceId": 2}, "id": 206}')
        await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity2", "Info": "version0"}, "instanceId": 3}, "id": 207}')
        await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseStressTest.RootEntity", "SequenceNumber": 222}, "instanceId": 2, "handlerId": "APP2"}, "id": 208}')
        
        initialRegApp2 = [("Safir.Dob.NodeInfo:2", "Registered"), ("DoseTest.GlobalEntity2:DEFAULT_HANDLER", "Registered"), ("DoseStressTest.RootEntity:APP2", "Registered"), ("DoseTest.GlobalService:APP2", "Registered"), ("DoseStressTest.Service:APP2", "Registered")]
        initialEntApp2 = [("Safir.Dob.NodeInfo:2", "_"), ("DoseTest.GlobalEntity2:1", "version0"), ("DoseTest.GlobalEntity2:2", "version0"), ("DoseTest.GlobalEntity2:3", "version0"), ("DoseStressTest.RootEntity:2", "222")]

        # App 5 - Light node
        initialRegLight = [("Safir.Dob.NodeInfo:5", "Registered")]
        initialEntLight = [("Safir.Dob.NodeInfo:5", "_")]

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached")
        
        await check_pool(app1, initialRegApp1 + initialRegApp2 + initialRegLight, initialEntApp1 + initialEntApp2 + initialEntLight)
        await check_pool(app2, initialRegApp1 + initialRegApp2 + initialRegLight, initialEntApp1 + initialEntApp2 + initialEntLight)
        await check_pool(app5, initialRegApp1 + initialRegApp2 + initialRegLight, initialEntApp1 + initialEntApp2 + initialEntLight)

        # Disable network on lightnode and wait for it to be Detached
        log("---------- set_network_state=down -----------")
        await set_network_state(False, node5.session_id) 
        await app5.wait_for_node_state("Detached")
        log("--- node5 is now detached")

        # Do some changes while light node is detached

        # App 1
        await app1.send('{"method": "unregisterHandler", "params": {"typeId": "DoseStressTest.Service", "handlerId": "APP1"}, "id": 9}') # unreg service
        await app1.send('{"method": "unregisterHandler", "params": {"typeId": "DoseStressTest.RootEntity", "handlerId": "APP1"}, "id": 9}') # unreg entity
        await app1.send('{"method": "deleteEntity", "params": {"typeId": "DoseTest.GlobalEntity", "instanceId": 1}, "id": 9}') # delete
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version1"}, "instanceId": 2}, "id": 10}') # update
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 4}, "id": 11}') # new
        changedRegApp1 = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalService:APP1", "Registered")]
        changedEntApp1 = [("Safir.Dob.NodeInfo:1", "_"), ("DoseTest.GlobalEntity:2", "version1"), ("DoseTest.GlobalEntity:3", "version0"), ("DoseTest.GlobalEntity:4", "version0")]

        # App 2
        await app2.send('{"method": "unregisterHandler", "params": {"typeId": "DoseStressTest.Service", "handlerId": "APP2"}, "id": 9}') # unreg service
        await app2.send('{"method": "unregisterHandler", "params": {"typeId": "DoseStressTest.RootEntity", "handlerId": "APP2"}, "id": 9}') # unreg entity
        await app2.send('{"method": "deleteEntity", "params": {"typeId": "DoseTest.GlobalEntity2", "instanceId": 1}, "id": 9}') # delete
        await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity2", "Info": "version1"}, "instanceId": 2}, "id": 10}') # update
        await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity2", "Info": "version0"}, "instanceId": 4}, "id": 11}') # new
        changedRegApp2 = [("Safir.Dob.NodeInfo:2", "Registered"), ("DoseTest.GlobalEntity2:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalService:APP2", "Registered")]
        changedEntApp2 = [("Safir.Dob.NodeInfo:2", "_"), ("DoseTest.GlobalEntity2:2", "version1"), ("DoseTest.GlobalEntity2:3", "version0"), ("DoseTest.GlobalEntity2:4", "version0")]

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pool(app1, changedRegApp1 + changedRegApp2, changedEntApp1 + changedEntApp2)
        await check_pool(app2, changedRegApp1 + changedRegApp2, changedEntApp1 + changedEntApp2)
        await check_pool(app5, initialRegApp1 + initialRegApp2 + initialRegLight, initialEntApp1 + initialEntApp2 + initialEntLight)

        # kill node 2 while detached
        log("---------- kill node 2 -----------")
        node2.killprocs()
        await asyncio.sleep(5)
        await check_pool(app1, changedRegApp1, changedEntApp1)
        await check_pool(app5, initialRegApp1 + initialRegApp2 + initialRegLight, initialEntApp1 + initialEntApp2 + initialEntLight)

        # Enable network on lightnode and wait for it to be Attached
        log("---------- set_network_state=up -----------")
        await set_network_state(True, node5.session_id) 
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached again")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pool(app1, changedRegApp1 + initialRegLight, changedEntApp1 + initialEntLight)
        await check_pool(app5, changedRegApp1 + initialRegLight, changedEntApp1 + initialEntLight)

        await asyncio.gather(app1.stop(), app5.stop())

async def one_normal_two_light_detach_reattach_one_light_big_pool(args):
    with test_case("one_normal_two_light_detach_reattach_one_light_big_pool"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=5, node_id=5) as node5,\
        launch_node(args, safir_instance=6, node_id=6) as node6:
    
        app1 = SafirApp(safir_instance=1, node_id=1)
        app5 = SafirApp(safir_instance=5, node_id=5)
        app6 = SafirApp(safir_instance=6, node_id=6)

        # App 1
        await app1.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.GlobalService"}, "id": 101}')
        await app1.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseStressTest.Service"}, "id": 102}')
        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity"}, "id": 103}')
        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseStressTest.RootEntity"}, "id": 104}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseStressTest.RootEntity", "SequenceNumber": 111}, "instanceId": 1}, "id": 105}')

        initialRegApp1 = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered"), ("DoseStressTest.RootEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalService:DEFAULT_HANDLER", "Registered"), ("DoseStressTest.Service:DEFAULT_HANDLER", "Registered")]
        initialEntApp1 = [("Safir.Dob.NodeInfo:1", "_"), ("DoseStressTest.RootEntity:1", "111")]

        # Create 8000 global entities
        for i in range(8000):
            await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": ' + str(i + 1) + '}, "id": ' + str(i + 106) +'}')
            initialEntApp1.append(("DoseTest.GlobalEntity:" + str(i + 1), "version0"))

        # App 5 - Light node
        initialRegApp5 = [("Safir.Dob.NodeInfo:5", "Registered")]
        initialEntApp5 = [("Safir.Dob.NodeInfo:5", "_")]

        # App 6 - Light node
        initialRegApp6 = [("Safir.Dob.NodeInfo:6", "Registered")]
        initialEntApp6 = [("Safir.Dob.NodeInfo:6", "_")]

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await asyncio.gather(app5.wait_for_node_state("Attached"), app6.wait_for_node_state("Attached"))
        log("--- node5 and node6 are now attached")
        await check_pool(app1, initialRegApp1 + initialRegApp5 + initialRegApp6, initialEntApp1 + initialEntApp5 + initialEntApp6)
        await check_pool(app5, initialRegApp1 + initialRegApp5, initialEntApp1 + initialEntApp5)
        await check_pool(app6, initialRegApp1 + initialRegApp6, initialEntApp1 + initialEntApp6)

        # Disable network on lightnode and wait for it to be Detached
        log("---------- set_network_state=down for node5 -----------")
        await set_network_state(False, node5.session_id)
        await asyncio.sleep(5)
        await app5.wait_for_node_state("Detached")
        log("--- node5 is now detached")
        await check_pool(app1, initialRegApp1 + initialRegApp6, initialEntApp1 + initialEntApp6)
        await check_pool(app5, initialRegApp1 + initialRegApp5, initialEntApp1 + initialEntApp5)
        await check_pool(app6, initialRegApp1 + initialRegApp6, initialEntApp1 + initialEntApp6)

        # Do some changes while light node is detached
        await app1.send('{"method": "unregisterHandler", "params": {"typeId": "DoseStressTest.Service"}, "id": 9}') # unreg service
        await app1.send('{"method": "unregisterHandler", "params": {"typeId": "DoseStressTest.RootEntity"}, "id": 9}') # unreg entity
        await app1.send('{"method": "deleteEntity", "params": {"typeId": "DoseTest.GlobalEntity", "instanceId": 1}, "id": 9}') # delete
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version1"}, "instanceId": 2}, "id": 10}') # update
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 8001}, "id": 11}') # new
        changedRegApp1 = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalService:DEFAULT_HANDLER", "Registered")]
        changedEntApp1 = [("Safir.Dob.NodeInfo:1", "_"), ("DoseTest.GlobalEntity:2", "version1")]
        for i in range(2, 8001):
            changedEntApp1.append(("DoseTest.GlobalEntity:" + str(i + 1), "version0"))

        await check_pool(app1, changedRegApp1 + initialRegApp6, changedEntApp1 + initialEntApp6)
        await check_pool(app5, initialRegApp1 + initialRegApp5, initialEntApp1 + initialEntApp5)
        await check_pool(app6, changedRegApp1 + initialRegApp6, changedEntApp1 + initialEntApp6)

        # Enable network again for node5
        log("---------- set_network_state=up for node5 -----------")
        await set_network_state(True, node5.session_id) 
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached again")
        await check_pool(app1, changedRegApp1 + initialRegApp5 + initialRegApp6, changedEntApp1 + initialEntApp5 + initialEntApp6)
        await check_pool(app5, changedRegApp1 + initialRegApp5, changedEntApp1 + initialEntApp5)
        await check_pool(app6, changedRegApp1 + initialRegApp6, changedEntApp1 + initialEntApp6)

        await asyncio.gather(app1.stop(), app5.stop(), app6.stop())

async def one_normal_one_light_ensure_only_valid_requests_and_no_unnecessary_notifications(args):
    with test_case("one_normal_one_light_ensure_only_valid_requests_and_no_unnecessary_notifications"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=5, node_id=5) as node5:
    
        app1 = SafirApp(safir_instance=1, node_id=1)
        app5 = SafirApp(safir_instance=5, node_id=5)

        # App 1
        await app1.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.GlobalService"}, "id": 101}')
        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity"}, "id": 103}')        
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 1}, "id": 104}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 2}, "id": 105}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 3}, "id": 106}')
        initialRegApp1 = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalService:DEFAULT_HANDLER", "Registered")]
        initialEntApp1 = [("Safir.Dob.NodeInfo:1", "_"), ("DoseTest.GlobalEntity:1", "version0"), ("DoseTest.GlobalEntity:2", "version0"), ("DoseTest.GlobalEntity:3", "version0")]

        # App 5 - Light node
        initialRegApp5 = [("Safir.Dob.NodeInfo:5", "Registered")]
        initialEntApp5 = [("Safir.Dob.NodeInfo:5", "_")]

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached")
        await check_pool(app1, initialRegApp1 + initialRegApp5, initialEntApp1 + initialEntApp5)
        await check_pool(app5, initialRegApp1 + initialRegApp5, initialEntApp1 + initialEntApp5)

        # Start saving notifications
        app5.notifications = list() 

        # Disable network on lightnode and wait for it to be Detached
        log("---------- set_network_state=down for node5 -----------")
        await set_network_state(False, node5.session_id)
        await asyncio.sleep(5)
        await app5.wait_for_node_state("Detached")
        log("--- node5 is now detached")
        await check_pool(app1, initialRegApp1, initialEntApp1)
        await check_pool(app5, initialRegApp1 + initialRegApp5, initialEntApp1 + initialEntApp5)

        # Try to send an updateRequest while detached. Expected to fail
        await app5.send('{"method": "updateRequest", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version1"}, "instanceId": 3}, "id": 501}')
        
        # Verify we got a detached state error response
        gotDetachedStateRespnse = False
        for x in range(60):
            found = False
            for msg in app5.notifications:
                if "501" in msg and "SafirDetachedState" in msg:
                    gotDetachedStateRespnse = True
                    break
            if gotDetachedStateRespnse: break
            await asyncio.sleep(5)

        if not gotDetachedStateRespnse:
            log("***** ERROR: Did not get a DetachedState error code when sending requests in detached state! *****")
            app1.dump_pool()
            app5.dump_pool()
            log("********************************************")
            raise AssertionError("Did not get a DetachedState error code when sending requests in detached state!")    

        # Enable network again for node5
        log("---------- set_network_state=up for node5 -----------")
        await set_network_state(True, node5.session_id) 
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached again")
        await app5.send('{"method": "updateRequest", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version1"}, "instanceId": 3}, "id": 502}')
        initialEntApp1[3] = ("DoseTest.GlobalEntity:3", "version1")
        await asyncio.sleep(5)
        await check_pool(app5, initialRegApp1 + initialRegApp5, initialEntApp1 + initialEntApp5)
        await check_pool(app1, initialRegApp1 + initialRegApp5, initialEntApp1 + initialEntApp5)

        # finally verify that we did not get any updates on unchanged entities
        for msg in app5.notifications:
            if "onUpdatedEntity" in msg and "DoseTest.GlobalEntity" in msg and "version0" in msg:
                log("***** ERROR: Received onUpdatedEntity on an instance that have not changed when we were detached! *****")
                app1.dump_pool()
                app5.dump_pool()
                log("********************************************")
                raise AssertionError("Received onUpdatedEntity on an instance that have not changed when we were detached!")    

        await asyncio.gather(app1.stop(), app5.stop())

async def lightnode_attaches_to_new_system_after_detached(args):
    with test_case("lightnode_attaches_to_new_system_after_detached"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=5, node_id=5) as node5:
    
        app1 = SafirApp(safir_instance=1, node_id=1)
        app5 = SafirApp(safir_instance=5, node_id=5)

        # App 1
        await app1.send('{"method": "registerServiceHandler", "params": {"typeId": "DoseTest.GlobalService"}, "id": 101}')
        await app1.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity"}, "id": 103}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 1}, "id": 104}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 2}, "id": 105}')
        await app1.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version0"}, "instanceId": 3}, "id": 106}')
        initialRegApp1 = [("Safir.Dob.NodeInfo:1", "Registered"), ("DoseTest.GlobalEntity:DEFAULT_HANDLER", "Registered"), ("DoseTest.GlobalService:DEFAULT_HANDLER", "Registered")]
        initialEntApp1 = [("Safir.Dob.NodeInfo:1", "_"), ("DoseTest.GlobalEntity:1", "version0"), ("DoseTest.GlobalEntity:2", "version0"), ("DoseTest.GlobalEntity:3", "version0")]

        # App 5 - Light node
        initialRegApp5 = [("Safir.Dob.NodeInfo:5", "Registered")]
        initialEntApp5 = [("Safir.Dob.NodeInfo:5", "_")]

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await app5.wait_for_node_state("Attached")
        log("--- node5 is now attached")
        await check_pool(app1, initialRegApp1 + initialRegApp5, initialEntApp1 + initialEntApp5)
        await check_pool(app5, initialRegApp1 + initialRegApp5, initialEntApp1 + initialEntApp5)

        # Disable network on lightnode and wait for it to be Detached
        log("---------- set_network_state=down for node5 -----------")
        await set_network_state(False, node5.session_id)
        await asyncio.sleep(5)
        await app5.wait_for_node_state("Detached")
        log("--- node5 is now detached")
        await check_pool(app1, initialRegApp1, initialEntApp1)
        await check_pool(app5, initialRegApp1 + initialRegApp5, initialEntApp1 + initialEntApp5)

        log("--- stop node1 while node5 is detached")
        node1.killprocs()

        # Enable network again for node5
        log("---------- set_network_state=up for node5 -----------")
        await set_network_state(True, node5.session_id) 
        await asyncio.sleep(10) # wait for a while and make sure the node stays in detached mode
        await app5.wait_for_node_state("Detached")

        # Start a new normal node
        log("--- start node2")
        with launch_node(args, safir_instance=2, node_id=2, start_dope=True) as node2:
            app2 = SafirApp(safir_instance=2, node_id=2)
            await app2.send('{"method": "registerEntityHandler", "params": {"typeId": "DoseTest.GlobalEntity", "handlerId": "APP2"}, "id": 107}')
            await app2.send('{"method": "setEntity", "params": {"entity": {"_DouType": "DoseTest.GlobalEntity", "Info": "version2"}, "instanceId": 22, "handlerId": "APP2"}, "id": 104}')
            initialRegApp2 = [("Safir.Dob.NodeInfo:2", "Registered"), ("DoseTest.GlobalEntity:APP2", "Registered")]
            initialEntApp2 = [("Safir.Dob.NodeInfo:2", "_"), ("DoseTest.GlobalEntity:22", "version2")]

            # let the system run for a while to complete PD
            await asyncio.sleep(5)
            await app5.wait_for_node_state("Attached")
            log("--- node5 is now attached to a new system")
            await check_pool(app2, initialRegApp2 + initialRegApp5, initialEntApp2 + initialEntApp5)
            await check_pool(app5, initialRegApp2 + initialRegApp5, initialEntApp2 + initialEntApp5)
            # Turn off network, since we cannot control the order TestEnv will kill the processes. If safir_websocket is killed before dose_main node5 might get an unregistration before it is detached
            await set_network_state(False, node5.session_id) 

        # node5 should now be detached again
        await set_network_state(True, node5.session_id) # turn on networkl again. Actually I'm not sure if this will fix all timing issues. The correct way would be to force TestEnv to kill dose_main and safir_control before safir_websocket.
        log("--- Killed node2, wait for node5 to be detached again")
        await app5.wait_for_node_state("Detached")
        await check_pool(app5, initialRegApp2 + initialRegApp5, initialEntApp2 + initialEntApp5)

        await app5.stop()


# ===========================================
# main
# ===========================================
async def main(args):
    await two_normal_one_light_detach_reattach_light(args)
    await two_normal_one_light_kill_one_normal_while_light_is_detached(args)
    await one_normal_two_light_detach_reattach_one_light_big_pool(args)
    await one_normal_one_light_ensure_only_valid_requests_and_no_unnecessary_notifications(args)
    await lightnode_attaches_to_new_system_after_detached(args)
    
if __name__ == "__main__":
    asyncio.run(main(parse_arguments()))
    for failed in failed_tests:
        log("*** Failed: ", failed)
    sys.exit(1 if len(failed_tests) > 0 else 0)
