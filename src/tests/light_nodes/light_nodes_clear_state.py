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
    os.environ["LLL_LOGDIR"] = os.path.normpath(os.path.join(os.getcwd(), "test_output", "clear", name))

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

# Check if dictionary contains exact specied keys
def contains_exact(keys, dictionary):
    return sorted(list(dictionary.keys())) == sorted(keys)

async def check_pool_detached_node(app):
    ok = False
    limited_type = "DoseTest.LimitedEntity:" if app.safir_instance == 3 else "DoseTest.LimitedEntity2:"
    expected_reg = [limited_type + "DEFAULT_HANDLER", "DoseTest.LimitedService:app" + str(app.node_id), "Safir.Dob.NodeInfo:" + str(app.node_id)]
    expected_reg.sort()

    expected_ent = ["Safir.Dob.NodeInfo:" + str(app.node_id)]
    for instance in range(1, app.number_of_entities + 1):
        expected_ent.append(limited_type + str(instance))
    expected_ent.sort()
    
    pool_timestamp = app.last_pool_update
    num_tries = 0
    while pool_timestamp < app.last_pool_update or num_tries < 5:
        num_tries = num_tries +1
        pool_timestamp = app.last_pool_update
        if contains_exact(expected_reg, app.registrations) and contains_exact(expected_ent, app.entities):
            ok = True
            break
        else:
            if num_tries > 2: log("--- Extended wait for PD to finish, wait time " + str((num_tries + 1)*10) + " sec")
            await asyncio.sleep(10)

    if not ok:
        log("*** ERROR: Incorrect pool on light node " + str(app.node_id) + ", safir_instance: " + str(app.safir_instance))
        app.dump_pool()
        print("  Expected registrations:")
        for i in expected_reg: print("    " + i)
        print("  Expected entities:")
        for i in expected_ent: print("    " + i)

    if not ok:
        raise AssertionError("Incorrect pool")    

async def check_pools_connected_nodes(*apps):
    ok = True
    expected_global_reg = list()
    expected_global_ent = list()
    expected_limited_reg = list()
    expected_limited_ent = list()
    for app in apps:
        if app.safir_instance < 3: # normal node
            global_type = "DoseTest.GlobalEntity:" if app.safir_instance == 1 else "DoseTest.GlobalEntity2:"
            expected_global_reg.append(global_type + "DEFAULT_HANDLER")
            expected_global_reg.append("Safir.Dob.NodeInfo:" + str(app.node_id)) #NodeInfo is actually a limited type, but when registered on a normal node, limited types are visible to all nodes
            expected_global_reg.append("DoseTest.GlobalService:app" + str(app.node_id))
            expected_global_ent.append("Safir.Dob.NodeInfo:" + str(app.node_id))
            for instance in range(1, app.number_of_entities + 1):
                expected_global_ent.append(global_type + str(instance))
        else: # light node
            limited_type =  "DoseTest.LimitedEntity:" if app.safir_instance == 3 else "DoseTest.LimitedEntity2:"
            expected_limited_reg.append(limited_type + "DEFAULT_HANDLER")
            expected_limited_reg.append("Safir.Dob.NodeInfo:" + str(app.node_id))
            expected_limited_reg.append("DoseTest.LimitedService:app" + str(app.node_id))
            expected_limited_ent.append("Safir.Dob.NodeInfo:" + str(app.node_id))
            for instance in range(1, app.number_of_entities + 1):
                expected_limited_ent.append(limited_type + str(instance))

    def check_apps(print_on_failure):
        apps_ok = True
        for app in apps:
            expected_reg = [*expected_global_reg]
            expected_ent = [*expected_global_ent]
            if app.safir_instance < 3: # normal node
                expected_reg.extend(expected_limited_reg)
                expected_ent.extend(expected_limited_ent)
            else: # light node
                limited_type =  "DoseTest.LimitedEntity:" if app.safir_instance == 3 else "DoseTest.LimitedEntity2:"
                expected_reg.extend([limited_type + "DEFAULT_HANDLER", "Safir.Dob.NodeInfo:" + str(app.node_id)])
                expected_reg.append("DoseTest.LimitedService:app" + str(app.node_id))
                expected_ent.append("Safir.Dob.NodeInfo:" + str(app.node_id))
                for instance in range(1, app.number_of_entities + 1):
                    expected_ent.append(limited_type + str(instance))

            expected_reg.sort()
            expected_ent.sort()
            if not (contains_exact(expected_reg, app.registrations) and contains_exact(expected_ent, app.entities)):
                apps_ok = False
                if print_on_failure:
                    log("*** ERROR: Incorrect pool on node " + str(app.node_id) + ", safir_instance: " + str(app.safir_instance))
                    app.dump_pool()
                    print("  Expected registrations:")
                    for i in expected_reg: print("    " + i)
                    print("  Expected entities:")
                    for i in expected_ent: print("    " + i)

        return apps_ok

    pool_timestamp = app.last_pool_update
    num_tries = 0
    while pool_timestamp < app.last_pool_update or num_tries < 5:
        num_tries = num_tries +1
        pool_timestamp = app.last_pool_update
        ok = check_apps(False)
        if ok:
            break
        else:
            if num_tries > 2: log("--- Extended wait for PD to finish, wait time " + str((num_tries + 1)*10) + " sec")
            await asyncio.sleep(10)

    if not ok:
        ok = check_apps(True) # One last try with print_on_failure

    if not ok:
        raise AssertionError("Incorrect pool")    

# ==============================================================================================
# Simple Safir application. The constructor will connect to the DOB and subscribe for all 
# entites and all registrations and will also register two handlers and create some entities.
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
        self.last_pool_update = time.time()
        self.stopped = False
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
                    log("Node " + str(self.node_id) + " is connected to the DOB")
                    await asyncio.gather(self._reader(), self._sender(), self._setup_dob())
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
                    self.last_pool_update = time.time()
                    entity_id = dou_type(msg) + ":" + str(instance_id(msg))
                    self.entities[entity_id] = json.dumps(entity(msg))
                elif callback == "onDeletedEntity":
                    self.last_pool_update = time.time()
                    entity_id = dou_type(msg) + ":" + str(instance_id(msg))
                    self.entities.pop(entity_id, None)
                elif callback == "onRegistered" and not ignore_type(msg):
                    self.last_pool_update = time.time()
                    handler = dou_type(msg) + ":" + str(handler_id(msg))
                    self.registrations[handler] = "Registered"
                elif callback == "onUnregistered":
                    self.last_pool_update = time.time()
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
        await self.send('{"method": "registerServiceHandler", "params": {"typeId": "' + service_type + '", "handlerId": "' + app_name + '"}, "id": 3}')
        await self.send('{"method": "subscribeEntity", "params": {"typeId": "Safir.Dob.Entity"}, "id": 4}')
        await self.send('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Entity"}, "id": 5}')
        await self.send('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Service"}, "id": 6}')
        for instance in range(1, self.number_of_entities + 1):
            await self.send('{"method": "setEntity", "params": {"entity": {"_DouType": "' + entity_type + '", "Info": "Hello ' + str(instance) + '"}, "instanceId": ' + str(instance) + '}, "id": ' + str(instance + 6) + '}')
        

# ===================================================================
# Test cases
# ===================================================================
async def one_normal_one_light_detach_reattach_light(args):
    with test_case("one_normal_one_light_detach_reattach_light"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=3, node_id=3) as node3:
    
        app1 = SafirApp(safir_instance=1, node_id=1)
        app3 = SafirApp(safir_instance=3, node_id=3)

        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        await check_pools_connected_nodes(app1, app3)

        # Disable network on lightnode and wait for it to be Detached
        await set_network_state(False, node3.session_id) 
        await app3.wait_for_node_state("Detached")
        log("--- node3 is now detached")

        await check_pools_connected_nodes(app1)
        await check_pool_detached_node(app3)

        # Enable network on lightnode and wait for it to be Attached
        await set_network_state(True, node3.session_id) 
        await app3.wait_for_node_state("Attached")
        log("--- node3 is now attached again")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pools_connected_nodes(app1, app3)

        await asyncio.gather(app1.stop(), app3.stop())

async def one_normal_one_light_restart_light(args):
    with test_case("one_normal_one_light_restart_light"),\
        launch_node(args, safir_instance=1, node_id=1) as node1:
        app1 = SafirApp(safir_instance=1, node_id=1)

        # Normal node 1 is up and running, now launch lightnode 3        
        with launch_node(args, safir_instance=3, node_id=3) as node3:
            app3 = SafirApp(safir_instance=3, node_id=3)

            # let the system run for a while to complete PD
            await asyncio.sleep(5)

            # Check that lightnode becomes Attached, and that the pools are correct
            await app3.wait_for_node_state("Attached")
            log("--- node3 is now attached")
            await check_pools_connected_nodes(app1, app3)
            await app3.stop()

        # node3 has been switched off, wait for node1 to notice that it is gone
        await asyncio.sleep(5)
        await check_pools_connected_nodes(app1)

        # Now start another lightnode, same safir_instance but a new nodeId
        with launch_node(args, safir_instance=3, node_id=33) as node33:
            app33 = SafirApp(safir_instance=3, node_id=33)

            # let the system run for a while to complete PD
            await asyncio.sleep(5)

            # Check that lightnode becomes Attached, and that the pools are correct
            await app33.wait_for_node_state("Attached")
            log("--- node33 is now attached")
            await check_pools_connected_nodes(app1, app33)
            await app33.stop()

        await app1.stop()

async def one_normal_two_light_detach_reattach_one_light(args):
    with test_case("one_normal_two_light_detach_reattach_one_light"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=3, node_id=3) as node3,\
        launch_node(args, safir_instance=4, node_id=4) as node4:
    
        app1 = SafirApp(safir_instance=1, node_id=1)
        app3 = SafirApp(safir_instance=3, node_id=3)
        app4 = SafirApp(safir_instance=4, node_id=4)
        
        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        await check_pools_connected_nodes(app1, app3, app4)

        # Disable network on lightnode and wait for it to be Detached
        await set_network_state(False, node3.session_id)
        await app3.wait_for_node_state("Detached")
        log("--- node3 is now detached")

        await check_pools_connected_nodes(app1, app4)
        await check_pool_detached_node(app3)

        # Enable network on lightnode and wait for it to be Attached
        await set_network_state(True, node3.session_id)
        await app3.wait_for_node_state("Attached")
        log("--- node3 is now attached again")
        
        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pools_connected_nodes(app1, app3, app4)

        await asyncio.gather(app1.stop(), app3.stop(), app4.stop())
    
async def one_normal_two_light_restart_normal(args):
    with test_case("one_normal_two_light_restart_normal"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=3, node_id=3) as node3,\
        launch_node(args, safir_instance=4, node_id=4) as node4:
    
        app1 = SafirApp(safir_instance=1, node_id=1)
        app3 = SafirApp(safir_instance=3, node_id=3)
        app4 = SafirApp(safir_instance=4, node_id=4)
        
        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pools_connected_nodes(app1, app3, app4)
        
        # restart node 1
        log("--- Stop node 1")
        await app1.stop()
        node1.killprocs()

        with launch_node(args, safir_instance=1, node_id=11) as node11:
            app11 = SafirApp(safir_instance=1, node_id=11)
            log("--- Node 1 has been restarted, now with new node_id 11")

            # let the system run for a while to complete PD
            await asyncio.sleep(5)
            await asyncio.gather(app3.wait_for_node_state("Attached"), app4.wait_for_node_state("Attached"))
            
            await check_pools_connected_nodes(app11, app3, app4)

            await asyncio.gather(app11.stop(), app3.stop(), app4.stop())
   

async def two_normal_two_light_detach_reattach_both_light(args):
    with test_case("two_normal_two_light_detach_reattach_both_light"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=2, node_id=2) as node2,\
        launch_node(args, safir_instance=3, node_id=3) as node3,\
        launch_node(args, safir_instance=4, node_id=4) as node4:
    
        app1 = SafirApp(safir_instance=1, node_id=1)
        app2 = SafirApp(safir_instance=2, node_id=2)
        app3 = SafirApp(safir_instance=3, node_id=3)
        app4 = SafirApp(safir_instance=4, node_id=4)

        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        await check_pools_connected_nodes(app1, app2, app3, app4)

        # Disable network on lightnodes and wait for them to be Detached
        await set_network_state(False, node3.session_id) 
        await set_network_state(False, node4.session_id) 
        await asyncio.gather(app3.wait_for_node_state("Detached"), app4.wait_for_node_state("Detached"))
        log("--- node 3 and node 4 are now detached")

        await check_pools_connected_nodes(app1, app2)
        await check_pool_detached_node(app3)
        await check_pool_detached_node(app4)

        # Enable network again and wait for nodes to become attached
        await set_network_state(True, node3.session_id)
        await set_network_state(True, node4.session_id)
        await asyncio.gather(app3.wait_for_node_state("Attached"), app4.wait_for_node_state("Attached"))
        log("--- node 3 and node 4 are now attached again")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pools_connected_nodes(app1, app2, app3, app4)
        
        await asyncio.gather(app1.stop(), app2.stop(), app3.stop(), app4.stop())

async def two_normal_two_light_detach_reattach_both_light_big_pool(args):
    with test_case("two_normal_two_light_detach_reattach_both_light_big_pool"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=2, node_id=2) as node2,\
        launch_node(args, safir_instance=3, node_id=3) as node3,\
        launch_node(args, safir_instance=4, node_id=4) as node4:
    
        app1 = SafirApp(safir_instance=1, node_id=1, num_entities=7000)
        app2 = SafirApp(safir_instance=2, node_id=2, num_entities=7000)
        app3 = SafirApp(safir_instance=3, node_id=3, num_entities=1000)
        app4 = SafirApp(safir_instance=4, node_id=4, num_entities=1000)

        # let the system run for a while to complete PD
        await asyncio.sleep(5)

        await check_pools_connected_nodes(app1, app2, app3, app4)

        # Disable network on lightnodes and wait for them to be Detached
        await set_network_state(False, node3.session_id)
        await set_network_state(False, node4.session_id)
        await asyncio.gather(app3.wait_for_node_state("Detached"), app4.wait_for_node_state("Detached"))
        log("--- node 3 and node 4 are now detached")

        await check_pools_connected_nodes(app1, app2)
        await check_pool_detached_node(app3)
        await check_pool_detached_node(app4)

        # Enable network again and wait for nodes to become attached
        await set_network_state(True, node3.session_id)
        await set_network_state(True, node4.session_id)
        await asyncio.gather(app3.wait_for_node_state("Attached"), app4.wait_for_node_state("Attached"))
        log("--- node 3 and node 4 are now attached again")

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pools_connected_nodes(app1, app2, app3, app4)
        
        await asyncio.gather(app1.stop(), app2.stop(), app3.stop(), app4.stop())

async def two_normal_two_light_restart_one_normal(args):
    with test_case("two_normal_two_light_restart_one_normal"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=2, node_id=2) as node2,\
        launch_node(args, safir_instance=3, node_id=3) as node3,\
        launch_node(args, safir_instance=4, node_id=4) as node4:

        app1 = SafirApp(safir_instance=1, node_id=1)
        app2 = SafirApp(safir_instance=2, node_id=2)
        app3 = SafirApp(safir_instance=3, node_id=3)
        app4 = SafirApp(safir_instance=4, node_id=4)

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pools_connected_nodes(app1, app2, app3, app4)

        # restart node 2
        log("--- Stop node 2")
        await app2.stop()
        node2.killprocs()    
        with launch_node(args, safir_instance=2, node_id=22) as node22:
            app22 = SafirApp(safir_instance=2, node_id=22)
            log("--- Node 2 has been restarted, now with new node_id 22")
            # let the system run for a while to complete PD
            await asyncio.sleep(5)
            await check_pools_connected_nodes(app1, app22, app3, app4)
            
            await asyncio.gather(app1.stop(), app22.stop(), app3.stop(), app4.stop())

async def two_normal_two_light_restart_both_normal(args):
    with test_case("two_normal_two_light_restart_both_normal"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=2, node_id=2) as node2,\
        launch_node(args, safir_instance=3, node_id=3) as node3,\
        launch_node(args, safir_instance=4, node_id=4) as node4:
        app1 = SafirApp(safir_instance=1, node_id=1)
        app2 = SafirApp(safir_instance=2, node_id=2)
        app3 = SafirApp(safir_instance=3, node_id=3)
        app4 = SafirApp(safir_instance=4, node_id=4)

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pools_connected_nodes(app1, app2, app3, app4)

        # restart node 1 and 2
        log("--- Restart node 1 and 2")
        await app1.stop()
        await app2.stop()
        node1.killprocs()
        node2.killprocs()

        with launch_node(args, safir_instance=1, node_id=11) as node1,\
            launch_node(args, safir_instance=2, node_id=22) as node2:
            app11 = SafirApp(safir_instance=1, node_id=11)
            app22 = SafirApp(safir_instance=2, node_id=22)
            log("--- Node 1 and 2 has been restarted, now with new node_id 11 and 22")
            # let the system run for a while to complete PD
            await asyncio.sleep(5)
            await check_pools_connected_nodes(app11, app22, app3, app4)
            
            await asyncio.gather(app11.stop(), app22.stop(), app3.stop(), app4.stop())

async def two_normal_two_light_toggle_network_many_times_on_both_light(args):
    with test_case("two_normal_two_light_toggle_network_many_times_on_both_light"),\
        launch_node(args, safir_instance=1, node_id=1) as node1,\
        launch_node(args, safir_instance=2, node_id=2) as node2,\
        launch_node(args, safir_instance=3, node_id=3) as node3,\
        launch_node(args, safir_instance=4, node_id=4) as node4:
        app1 = SafirApp(safir_instance=1, node_id=1)
        app2 = SafirApp(safir_instance=2, node_id=2)
        app3 = SafirApp(safir_instance=3, node_id=3)
        app4 = SafirApp(safir_instance=4, node_id=4)

        # let the system run for a while to complete PD
        await asyncio.sleep(5)
        await check_pools_connected_nodes(app1, app2, app3, app4)

        # Toggle network 10 times and end with disabled network
        for toggle in range(10):
            network_state = toggle % 2 == 0 # will end as disabled network
            await set_network_state(network_state, node3.session_id)
            await set_network_state(network_state, node4.session_id)
            await asyncio.sleep(2)

        # Give extra time to reach correct state
        await asyncio.gather(app3.wait_for_node_state("Detached", timeout=120),
                            app4.wait_for_node_state("Detached", timeout=120))
        log("--- node 3 and node 4 are now detached")

        # Give some time for PD
        await asyncio.sleep(5)
        await check_pools_connected_nodes(app1, app2)
        await check_pool_detached_node(app3)
        await check_pool_detached_node(app4)

        # Toggle network 10 times and end with enabled network
        for toggle in range(10):
            network_state = toggle % 2 == 1 # will end as enabled network
            await set_network_state(network_state, node3.session_id)
            await set_network_state(network_state, node4.session_id)
            await asyncio.sleep(2)

        # Give extra time to reach correct state
        await asyncio.gather(app3.wait_for_node_state("Attached", timeout=120),
                            app4.wait_for_node_state("Attached", timeout=120))
        log("--- node 3 and node 4 are now attached")
        
        # Give some time for PD
        await asyncio.sleep(5)
        await check_pools_connected_nodes(app1, app2, app3, app4)
        
        # Run the test_sequence
        await asyncio.gather(app1.stop(), app2.stop(), app3.stop(), app4.stop())

# ===========================================
# main
# ===========================================
async def main(args):
    await one_normal_one_light_detach_reattach_light(args)                      # ok
    await one_normal_two_light_detach_reattach_one_light(args)                  # ok
    await one_normal_two_light_restart_normal(args)                             # ok
    await two_normal_two_light_restart_one_normal(args)                         # ok
    await one_normal_one_light_restart_light(args)                              # ok
    
    # await two_normal_two_light_detach_reattach_both_light(args)               # fails sometimes
    # await two_normal_two_light_detach_reattach_both_light_big_pool(args)      # fails sometimes
    # await two_normal_two_light_restart_both_normal(args)                      # fails sometimes
    # await two_normal_two_light_toggle_network_many_times_on_both_light(args)  # fails sometimes

    #---- Some code for repeating a test and clearing local log folder after each run
    # for i in range(25):
    #     for f in glob.glob("/home/joel/dev/log/*"): os.remove(f)
    #     await two_normal_two_light_restart_both_normal(args)
    #     if len(failed_tests) > 0: return

if __name__ == "__main__":
    asyncio.run(main(parse_arguments()))
    for failed in failed_tests:
        log("*** Failed: ", failed)
    sys.exit(1 if len(failed_tests) > 0 else 0)
