#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2026 (http://safirsdkcore.com)
#
# Created by: Joel Ottosson
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
import sys
import argparse
import asyncio
import json
from urllib import request, error
import websockets
from testenv import TestEnv, TestEnvStopper, log


def parse_arguments():
    parser = argparse.ArgumentParser("rest/ws combined test scaffold")
    parser.add_argument("--safir-control", required=True)
    parser.add_argument("--dose_main", required=True)
    parser.add_argument("--dope_main", required=True)
    parser.add_argument("--safir-show-config", required=True)
    parser.add_argument("--safir_web", required=True)
    return parser.parse_args()


def rest_post(url, payload, allow_http_error=False):
    data = json.dumps(payload).encode("utf-8")
    req = request.Request(url=url,
                          data=data,
                          headers={"Content-Type": "application/json"},
                          method="POST")
    try:
        with request.urlopen(req, timeout=10) as response:
            body = response.read().decode("utf-8")
            return response.getcode(), json.loads(body)
    except error.HTTPError as http_error:
        if not allow_http_error:
            raise
        body = http_error.read().decode("utf-8") if http_error.fp is not None else "{}"
        try:
            parsed = json.loads(body) if body else {}
        except json.JSONDecodeError:
            parsed = {"error": body}
        return http_error.code, parsed


def rest_get(url, allow_http_error=False):
    req = request.Request(url=url, method="GET")
    try:
        with request.urlopen(req, timeout=10) as response:
            body = response.read().decode("utf-8")
            return response.getcode(), json.loads(body)
    except error.HTTPError as http_error:
        if not allow_http_error:
            raise
        body = http_error.read().decode("utf-8") if http_error.fp is not None else "{}"
        try:
            parsed = json.loads(body) if body else {}
        except json.JSONDecodeError:
            parsed = {"error": body}
        return http_error.code, parsed


def rest_put(url, payload=None, allow_http_error=False):
    data = json.dumps(payload).encode("utf-8") if payload is not None else b""
    req = request.Request(url=url,
                          data=data,
                          headers={"Content-Type": "application/json"},
                          method="PUT")
    try:
        with request.urlopen(req, timeout=10) as response:
            body = response.read().decode("utf-8")
            return response.getcode(), json.loads(body)
    except error.HTTPError as http_error:
        if not allow_http_error:
            raise
        body = http_error.read().decode("utf-8") if http_error.fp is not None else "{}"
        try:
            parsed = json.loads(body) if body else {}
        except json.JSONDecodeError:
            parsed = {"error": body}
        return http_error.code, parsed


def rest_patch(url, payload, allow_http_error=False):
    data = json.dumps(payload).encode("utf-8")
    req = request.Request(url=url,
                          data=data,
                          headers={"Content-Type": "application/json"},
                          method="PATCH")
    try:
        with request.urlopen(req, timeout=10) as response:
            body = response.read().decode("utf-8")
            return response.getcode(), json.loads(body)
    except error.HTTPError as http_error:
        if not allow_http_error:
            raise
        body = http_error.read().decode("utf-8") if http_error.fp is not None else "{}"
        try:
            parsed = json.loads(body) if body else {}
        except json.JSONDecodeError:
            parsed = {"error": body}
        return http_error.code, parsed


def rest_delete(url, allow_http_error=False):
    req = request.Request(url=url, method="DELETE")
    try:
        with request.urlopen(req, timeout=10) as response:
            body = response.read().decode("utf-8")
            return response.getcode(), json.loads(body)
    except error.HTTPError as http_error:
        if not allow_http_error:
            raise
        body = http_error.read().decode("utf-8") if http_error.fp is not None else "{}"
        try:
            parsed = json.loads(body) if body else {}
        except json.JSONDecodeError:
            parsed = {"error": body}
        return http_error.code, parsed


class CombinedClient:
    def __init__(self, websocket_uri):
        self.websocket_uri = websocket_uri
        self.send_queue = asyncio.Queue()
        self.receive_queue = asyncio.Queue()
        self.ws = None
        self.task = asyncio.create_task(self._run())

    async def stop(self):
        await self.send_queue.put(None)
        await self.send_queue.join()
        await self.task

    async def send(self, payload):
        await self.send_queue.put(payload)

    async def wait_for_response(self, expected_id, timeout_seconds=20):
        async with asyncio.timeout(timeout_seconds):
            while True:
                msg = await self.receive_queue.get()
                if msg.get("id") == expected_id:
                    return msg

    async def wait_for_notification(self, expected_method, timeout_seconds=20, predicate=None):
        async with asyncio.timeout(timeout_seconds):
            while True:
                msg = await self.receive_queue.get()
                if msg.get("method") != expected_method:
                    continue
                if predicate is None or predicate(msg):
                    return msg

    async def _run(self):
        for connect_try in range(10):
            try:
                async with websockets.connect(self.websocket_uri) as websocket:
                    self.ws = websocket
                    log("Connected websocket client:", self.websocket_uri)
                    await asyncio.gather(self._reader(), self._sender())
                    return
            except ConnectionRefusedError:
                await asyncio.sleep(1)
                if connect_try == 9:
                    raise
            except Exception:
                await asyncio.sleep(1)
                if connect_try == 9:
                    raise

    async def _reader(self):
        async for message in self.ws:
            decoded = json.loads(message)
            await self.receive_queue.put(decoded)

    async def _sender(self):
        while True:
            msg = await self.send_queue.get()
            self.send_queue.task_done()
            if msg is None:
                await self.ws.close()
                return
            await self.ws.send(msg)


async def run_combined_flow():
    rest_base = "http://127.0.0.1:10000"
    connection_name = None
    client = None

    try:
        ws_uri = "ws://127.0.0.1:10000"
        client = CombinedClient(ws_uri)
        connection_name = "rest_ws_combined"
        conn_url = lambda path: f"{rest_base}/connections/{connection_name}/{path}"

        # ======= STEP 1: Open connection via websocket =======
        log("Step 1: Opening websocket connection")
        open_request_id = "combined-open-1"
        await client.send('{"jsonrpc":"2.0", "method":"open", "params":{"connectionName":"' + connection_name + '", "context":0}, "id":"' + open_request_id + '"}')
        open_response = await client.wait_for_response(open_request_id)
        if open_response.get("result") != "OK":
            raise AssertionError("Websocket open failed: " + str(open_response))
        log("Step 1 succeeded")

        # ======= STEP 2: isOpen (REST) =======
        log("Step 2: REST isOpen")
        status, result = rest_get(conn_url("isOpen"))
        if status != 200:
            raise AssertionError("REST isOpen http status was not 200: " + str(status))
        if result.get("isOpen") is not True:
            raise AssertionError("REST isOpen failed: " + str(result))
        log("Step 2 succeeded")

        # ======= STEP 3: ping (REST) =======
        log("Step 3: REST ping")
        status, result = rest_get(conn_url("ping"))
        if status != 200:
            raise AssertionError("REST ping http status was not 200: " + str(status))
        if result.get("result") != "pong":
            raise AssertionError("REST ping expected 'pong', got: " + str(result))
        log("Step 3 succeeded")

        # ======= STEP 4: getTypeHierarchy (REST) =======
        log("Step 4: REST getTypeHierarchy")
        status, result = rest_get(rest_base + "/typeHierarchy")
        if status != 200:
            raise AssertionError("REST getTypeHierarchy http status was not 200: " + str(status))
        if "result" not in result:
            raise AssertionError("REST getTypeHierarchy missing 'result': " + str(result))
        log("Step 4 succeeded")

        # ======= STEP 5: readEntity - expected error (REST) =======
        log("Step 5: REST readEntity (expected error, entity not created)")
        status, result = rest_get(conn_url("entities/Safir.Control.Status/1"), allow_http_error=True)
        if status != 400:
            raise AssertionError("REST readEntity unexpected http status: " + str(status))
        if "error" not in result:
            raise AssertionError("REST readEntity expected error response: " + str(result))
        log("Step 5 succeeded")

        # ======= STEP 6: isOpen (WS) =======
        log("Step 6: WS isOpen")
        request_id = "combined-isOpen-1"
        await client.send('{"jsonrpc":"2.0", "method":"isOpen", "id":"' + request_id + '"}')
        response = await client.wait_for_response(request_id)
        if response.get("result") is not True:
            raise AssertionError("Expected websocket isOpen=true, got: " + str(response))
        log("Step 6 succeeded")

        # ======= STEP 7: subscribeEntity (REST) =======
        log("Step 7: REST subscribeEntity (Safir.Control.Status)")
        status, result = rest_put(conn_url("subscriptions/entities/Safir.Control.Status"))
        if status != 200:
            raise AssertionError("REST subscribeEntity http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST subscribeEntity failed: " + str(result))
        log("Step 7 succeeded")

        # ======= STEP 8: subscribeRegistration (REST) =======
        log("Step 8: REST subscribeRegistration (Safir.Control.Status)")
        status, result = rest_put(conn_url("subscriptions/registrations/Safir.Control.Status"))
        if status != 200:
            raise AssertionError("REST subscribeRegistration http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST subscribeRegistration failed: " + str(result))
        log("Step 8 succeeded")

        # ======= STEP 9: subscribeMessage (REST) =======
        log("Step 9: REST subscribeMessage (Safir.Application.BackdoorCommand)")
        status, result = rest_put(conn_url("subscriptions/messages/Safir.Application.BackdoorCommand"))
        if status != 200:
            raise AssertionError("REST subscribeMessage http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST subscribeMessage failed: " + str(result))
        log("Step 9 succeeded")

        # ======= STEP 10: sendMessage (REST) + onMessage (WS) =======
        log("Step 10: REST sendMessage + WS onMessage")
        status, result = rest_post(
            conn_url("messages/Safir.Application.BackdoorCommand"),
            {"_DouType": "Safir.Application.BackdoorCommand", "NodeName": "Hello", "Command": "World"})
        if status != 200:
            raise AssertionError("REST sendMessage http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST sendMessage failed: " + str(result))
        msg_notif = await client.wait_for_notification(
            "onMessage",
            predicate=lambda msg: msg.get("params", {}).get("message", {}).get("NodeName") == "Hello"
            and msg.get("params", {}).get("message", {}).get("Command") == "World")
        log("Received WS onMessage:", msg_notif)
        log("Step 10 succeeded")

        # ======= STEP 11: registerEntityHandler (REST) + onRegistered (WS) =======
        log("Step 11: REST registerEntityHandler (Safir.Control.Status handler=1) + WS onRegistered")
        status, result = rest_put(conn_url("handlers/entities/Safir.Control.Status?handler=1"))
        if status != 200:
            raise AssertionError("REST registerEntityHandler http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST registerEntityHandler failed: " + str(result))
        reg_notif = await client.wait_for_notification(
            "onRegistered",
            predicate=lambda msg: msg.get("params", {}).get("typeId") == "Safir.Control.Status"
            and msg.get("params", {}).get("handlerId") == "1")
        log("Received WS onRegistered:", reg_notif)
        log("Step 11 succeeded")

        # ======= STEP 12: getInstanceIdPolicy (REST) =======
        log("Step 12: REST getInstanceIdPolicy (Safir.Control.Status handler=1)")
        status, result = rest_get(conn_url("entities/Safir.Control.Status/instanceIdPolicy?handler=1"))
        if status != 200:
            raise AssertionError("REST getInstanceIdPolicy http status was not 200: " + str(status))
        if result.get("result") != "RequestorDecidesInstanceId":
            raise AssertionError("REST getInstanceIdPolicy unexpected result: " + str(result))
        log("Step 12 succeeded")

        # ======= STEP 13: setEntity instance 101 (REST) + onNewEntity (WS) =======
        log("Step 13: REST setEntity (Safir.Control.Status instance 101) + WS onNewEntity")
        status, result = rest_put(
            conn_url("entities/Safir.Control.Status/101?handler=1"),
            {"_DouType": "Safir.Control.Status", "NodeId": 101})
        if status != 200:
            raise AssertionError("REST setEntity http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST setEntity failed: " + str(result))
        on_new = await client.wait_for_notification(
            "onNewEntity",
            predicate=lambda msg: msg.get("params", {}).get("instanceId") == "101"
            and msg.get("params", {}).get("entity", {}).get("NodeId") == "101")
        log("Received WS onNewEntity:", on_new)
        log("Step 13 succeeded")

        # ======= STEP 14: getAllInstanceIds (REST) =======
        log("Step 14: REST getAllInstanceIds (Safir.Control.Status)")
        status, result = rest_get(conn_url("entities/Safir.Control.Status/instances"))
        if status != 200:
            raise AssertionError("REST getAllInstanceIds http status was not 200: " + str(status))
        if "result" not in result:
            raise AssertionError("REST getAllInstanceIds missing 'result': " + str(result))
        if "101" not in result["result"]:
            raise AssertionError("REST getAllInstanceIds: expected 101 in result, got: " + str(result["result"]))
        log("REST getAllInstanceIds result:", result["result"])
        log("Step 14 succeeded")

        # ======= STEP 15: getNumberOfInstances (REST) =======
        log("Step 15: REST getNumberOfInstances (Safir.Control.Status handler=1)")
        status, result = rest_get(conn_url("entities/Safir.Control.Status/count?handler=1"))
        if status != 200:
            raise AssertionError("REST getNumberOfInstances http status was not 200: " + str(status))
        if result.get("result") != 1:
            raise AssertionError("REST getNumberOfInstances expected 1, got: " + str(result))
        log("Step 15 succeeded")

        # ======= STEP 16: getNumberOfInstances for non-entity type - expected error (REST) =======
        log("Step 16: REST getNumberOfInstances (Safir.Dob.Item - not an entity type, expected 0)")
        status, result = rest_get(conn_url("entities/Safir.Dob.Item/count"))
        if status != 200:
            raise AssertionError("REST getNumberOfInstances http status was not 200: " + str(status))
        if result.get("result") != 0:
            raise AssertionError("REST getNumberOfInstances expected 1, got: " + str(result))
        log("Step 16 succeeded")

        # ======= STEP 17: isCreated (REST) =======
        log("Step 17: REST isCreated (true for 101, false for 999)")
        status, result = rest_get(conn_url("entities/Safir.Control.Status/101/isCreated"))
        if status != 200:
            raise AssertionError("REST isCreated(101) http status was not 200: " + str(status))
        if result.get("result") is not True:
            raise AssertionError("REST isCreated(101) expected true, got: " + str(result))
        status, result = rest_get(conn_url("entities/Safir.Control.Status/999/isCreated"))
        if status != 200:
            raise AssertionError("REST isCreated(999) http status was not 200: " + str(status))
        if result.get("result") is not False:
            raise AssertionError("REST isCreated(999) expected false, got: " + str(result))
        log("Step 17 succeeded")

        # ======= STEP 18: setEntityChanges PATCH (REST) + onUpdatedEntity (WS) =======
        log("Step 18: REST setEntityChanges PATCH (Safir.Control.Status instance 101) + WS onUpdatedEntity")
        status, result = rest_patch(
            conn_url("entities/Safir.Control.Status/101?handler=1"),
            {"_DouType": "Safir.Control.Status", "SystemIncarnation": 99})
        if status != 200:
            raise AssertionError("REST setEntityChanges http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST setEntityChanges failed: " + str(result))
        on_updated = await client.wait_for_notification(
            "onUpdatedEntity",
            predicate=lambda msg: msg.get("params", {}).get("instanceId") == "101")
        log("Received WS onUpdatedEntity:", on_updated)
        log("Step 18 succeeded")

        # ======= STEP 19: readEntity success (REST) =======
        log("Step 19: REST readEntity (Safir.Control.Status instance 101) - success")
        status, result = rest_get(conn_url("entities/Safir.Control.Status/101"))
        if status != 200:
            raise AssertionError("REST readEntity http status was not 200: " + str(status))
        if "entity" not in result:
            raise AssertionError("REST readEntity missing 'entity' field: " + str(result))
        if result["entity"].get("_DouType") != "Safir.Control.Status":
            raise AssertionError("REST readEntity unexpected entity type: " + str(result))
        log("REST readEntity result:", result)
        log("Step 19 succeeded")

        # ======= STEP 20: setEntity instance 102 (REST) + onNewEntity (WS) =======
        log("Step 20: REST setEntity (Safir.Control.Status instance 102) + WS onNewEntity")
        status, result = rest_put(
            conn_url("entities/Safir.Control.Status/102?handler=1"),
            {"_DouType": "Safir.Control.Status", "NodeId": 102})
        if status != 200:
            raise AssertionError("REST setEntity instance 102 http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST setEntity instance 102 failed: " + str(result))
        await client.wait_for_notification(
            "onNewEntity",
            predicate=lambda msg: msg.get("params", {}).get("instanceId") == "102")
        log("Step 20 succeeded")

        # ======= STEP 21: deleteEntity instance 101 (REST) + onDeletedEntity (WS) =======
        log("Step 21: REST deleteEntity (Safir.Control.Status instance 101) + WS onDeletedEntity")
        status, result = rest_delete(conn_url("entities/Safir.Control.Status/101?handler=1"))
        if status != 200:
            raise AssertionError("REST deleteEntity http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST deleteEntity failed: " + str(result))
        on_deleted = await client.wait_for_notification(
            "onDeletedEntity",
            predicate=lambda msg: msg.get("params", {}).get("instanceId") == "101")
        log("Received WS onDeletedEntity for instance 101:", on_deleted)
        log("Step 21 succeeded")

        # ======= STEP 22: deleteAllInstances (REST) + onDeletedEntity for 102 (WS) =======
        log("Step 22: REST deleteAllInstances (Safir.Control.Status handler=1) + WS onDeletedEntity")
        status, result = rest_delete(conn_url("entities/Safir.Control.Status?handler=1"))
        if status != 200:
            raise AssertionError("REST deleteAllInstances http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST deleteAllInstances failed: " + str(result))
        on_deleted_all = await client.wait_for_notification(
            "onDeletedEntity",
            predicate=lambda msg: msg.get("params", {}).get("instanceId") == "102")
        log("Received WS onDeletedEntity for instance 102:", on_deleted_all)
        log("Step 22 succeeded")

        # ======= STEP 23: subscribeRegistration for Safir.Control.Command (REST) =======
        log("Step 23: REST subscribeRegistration (Safir.Control.Command)")
        status, result = rest_put(conn_url("subscriptions/registrations/Safir.Control.Command"))
        if status != 200:
            raise AssertionError("REST subscribeRegistration (Command) http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST subscribeRegistration (Command) failed: " + str(result))
        log("Step 23 succeeded")

        # ======= STEP 24: registerServiceHandler (REST) + onRegistered (WS) =======
        log("Step 24: REST registerServiceHandler (Safir.Control.Command handler=99) + WS onRegistered")
        status, result = rest_put(conn_url("handlers/services/Safir.Control.Command?handler=99"))
        if status != 200:
            raise AssertionError("REST registerServiceHandler http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST registerServiceHandler failed: " + str(result))
        srv_reg_notif = await client.wait_for_notification(
            "onRegistered",
            predicate=lambda msg: msg.get("params", {}).get("typeId") == "Safir.Control.Command"
            and msg.get("params", {}).get("handlerId") == "99")
        log("Received WS onRegistered for service handler:", srv_reg_notif)
        log("Step 24 succeeded")

        # ======= STEP 25: createRequest (REST) + onCreateRequest (WS) + respond =======
        log("Step 25: REST createRequest (Safir.Control.Status instance 200) + WS onCreateRequest + respond")
        status, result = rest_post(
            conn_url("requests/entities/Safir.Control.Status?handler=1&instanceId=200"),
            {"_DouType": "Safir.Control.Status", "NodeId": 200})
        if status != 200:
            raise AssertionError("REST createRequest http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST createRequest failed: " + str(result))
        on_create_req = await client.wait_for_notification(
            "onCreateRequest",
            predicate=lambda msg: msg.get("params", {}).get("instanceId") == "200")
        response_sender_id = on_create_req.get("id")
        log("Received WS onCreateRequest, responseSenderId:", response_sender_id)
        await client.send(json.dumps({"jsonrpc": "2.0",
                                      "result": {"_DouType": "Safir.Dob.SuccessResponse"},
                                      "id": response_sender_id}))
        log("Step 25 succeeded")

        # ======= STEP 26: setEntity instance 200 (REST) + onNewEntity (WS) =======
        log("Step 26: REST setEntity (Safir.Control.Status instance 200) + WS onNewEntity")
        status, result = rest_put(
            conn_url("entities/Safir.Control.Status/200?handler=1"),
            {"_DouType": "Safir.Control.Status", "NodeId": 200})
        if status != 200:
            raise AssertionError("REST setEntity instance 200 http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST setEntity instance 200 failed: " + str(result))
        await client.wait_for_notification(
            "onNewEntity",
            predicate=lambda msg: msg.get("params", {}).get("instanceId") == "200")
        log("Step 26 succeeded")

        # ======= STEP 27: updateRequest (REST) + onUpdateRequest (WS) + respond =======
        log("Step 27: REST updateRequest (Safir.Control.Status instance 200) + WS onUpdateRequest + respond")
        status, result = rest_post(
            conn_url("requests/entities/Safir.Control.Status/200/update"),
            {"_DouType": "Safir.Control.Status", "SystemIncarnation": 42})
        if status != 200:
            raise AssertionError("REST updateRequest http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST updateRequest failed: " + str(result))
        on_update_req = await client.wait_for_notification(
            "onUpdateRequest",
            predicate=lambda msg: msg.get("params", {}).get("instanceId") == "200")
        response_sender_id = on_update_req.get("id")
        log("Received WS onUpdateRequest, responseSenderId:", response_sender_id)
        await client.send(json.dumps({"jsonrpc": "2.0",
                                      "result": {"_DouType": "Safir.Dob.SuccessResponse"},
                                      "id": response_sender_id}))
        log("Step 27 succeeded")

        # ======= STEP 28: deleteRequest (REST) + onDeleteRequest (WS) + respond =======
        log("Step 28: REST deleteRequest (Safir.Control.Status instance 200) + WS onDeleteRequest + respond")
        status, result = rest_post(
            conn_url("requests/entities/Safir.Control.Status/200/delete"),
            {})
        if status != 200:
            raise AssertionError("REST deleteRequest http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST deleteRequest failed: " + str(result))
        on_delete_req = await client.wait_for_notification(
            "onDeleteRequest",
            predicate=lambda msg: msg.get("params", {}).get("instanceId") == "200")
        response_sender_id = on_delete_req.get("id")
        log("Received WS onDeleteRequest, responseSenderId:", response_sender_id)
        await client.send(json.dumps({"jsonrpc": "2.0",
                                      "result": {"_DouType": "Safir.Dob.SuccessResponse"},
                                      "id": response_sender_id}))
        log("Step 28 succeeded")

        # ======= STEP 29: serviceRequest (REST) + onServiceRequest (WS) + respond =======
        log("Step 29: REST serviceRequest (Safir.Control.Command handler=99) + WS onServiceRequest + respond")
        status, result = rest_post(
            conn_url("requests/services/Safir.Control.Command?handler=99"),
            {"_DouType": "Safir.Control.Command", "Operation": "Shutdown", "NodeId": 0})
        if status != 200:
            raise AssertionError("REST serviceRequest http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST serviceRequest failed: " + str(result))
        on_svc_req = await client.wait_for_notification(
            "onServiceRequest",
            predicate=lambda msg: msg.get("params", {}).get("request", {}).get("_DouType") == "Safir.Control.Command")
        response_sender_id = on_svc_req.get("id")
        log("Received WS onServiceRequest, responseSenderId:", response_sender_id)
        await client.send(json.dumps({"jsonrpc": "2.0",
                                      "result": {"_DouType": "Safir.Dob.SuccessResponse"},
                                      "id": response_sender_id}))
        log("Step 29 succeeded")

        # ======= STEP 30: unsubscribeEntity (REST) =======
        log("Step 30: REST unsubscribeEntity (Safir.Control.Status)")
        status, result = rest_delete(conn_url("subscriptions/entities/Safir.Control.Status"))
        if status != 200:
            raise AssertionError("REST unsubscribeEntity http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST unsubscribeEntity failed: " + str(result))
        log("Step 30 succeeded")

        # ======= STEP 31: unsubscribeMessage (REST) =======
        log("Step 31: REST unsubscribeMessage (Safir.Application.BackdoorCommand)")
        status, result = rest_delete(conn_url("subscriptions/messages/Safir.Application.BackdoorCommand"))
        if status != 200:
            raise AssertionError("REST unsubscribeMessage http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST unsubscribeMessage failed: " + str(result))
        log("Step 31 succeeded")

        # ======= STEP 32: unsubscribeRegistration (Safir.Control.Status) (REST) =======
        log("Step 32: REST unsubscribeRegistration (Safir.Control.Status)")
        status, result = rest_delete(conn_url("subscriptions/registrations/Safir.Control.Status"))
        if status != 200:
            raise AssertionError("REST unsubscribeRegistration (Status) http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST unsubscribeRegistration (Status) failed: " + str(result))
        log("Step 32 succeeded")

        # ======= STEP 33: unsubscribeRegistration (Safir.Control.Command) (REST) =======
        log("Step 33: REST unsubscribeRegistration (Safir.Control.Command)")
        status, result = rest_delete(conn_url("subscriptions/registrations/Safir.Control.Command"))
        if status != 200:
            raise AssertionError("REST unsubscribeRegistration (Command) http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST unsubscribeRegistration (Command) failed: " + str(result))
        log("Step 33 succeeded")

        # ======= STEP 34: unregisterHandler for Safir.Control.Status (REST) =======
        log("Step 34: REST unregisterHandler (Safir.Control.Status handler=1)")
        status, result = rest_delete(conn_url("handlers/Safir.Control.Status?handler=1"))
        if status != 200:
            raise AssertionError("REST unregisterHandler (Status) http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST unregisterHandler (Status) failed: " + str(result))
        log("Step 34 succeeded")

        # ======= STEP 35: unregisterHandler for Safir.Control.Command (REST) =======
        log("Step 35: REST unregisterHandler (Safir.Control.Command handler=99)")
        status, result = rest_delete(conn_url("handlers/Safir.Control.Command?handler=99"))
        if status != 200:
            raise AssertionError("REST unregisterHandler (Command) http status was not 200: " + str(status))
        if result.get("status") != "OK":
            raise AssertionError("REST unregisterHandler (Command) failed: " + str(result))
        log("Step 35 succeeded")

        # ======= STEP 36: Close connection via websocket =======
        log("Step 36: Close websocket connection")
        close_request_id = "combined-close-1"
        await client.send('{"jsonrpc":"2.0", "method":"close", "id":"' + close_request_id + '"}')
        close_response = await client.wait_for_response(close_request_id)
        if close_response.get("result") != "OK":
            raise AssertionError("Websocket close failed: " + str(close_response))
        log("Step 36 succeeded")

        log("All steps succeeded! Full REST API coverage verified.")
    finally:
        if client is not None:
            await client.stop()
            log("Websocket client stopped")

        if connection_name is not None and client is not None:
            post_ws_status, post_ws_result = rest_get(
                rest_base + "/connections/" + connection_name + "/isOpen",
                allow_http_error=True)
            if post_ws_status == 200:
                if post_ws_result.get("isOpen") is not False:
                    raise AssertionError("Expected connection to be closed after websocket disconnect, got: " + str(post_ws_result))
                log("Verified connection is closed after websocket disconnect")
            elif post_ws_status == 404:
                log("Verified connection has been removed after websocket disconnect")
            else:
                raise AssertionError("REST isOpen after websocket stop returned unexpected HTTP status: " + str(post_ws_status))



def main():
    arguments = parse_arguments()

    env = TestEnv(safir_control=arguments.safir_control,
                  dose_main=arguments.dose_main,
                  dope_main=arguments.dope_main,
                  safir_show_config=arguments.safir_show_config)

    with TestEnvStopper(env):
        env.launchProcess("safir_web", arguments.safir_web)
        log("Waiting for safir_web to start")
        env.WaitForOutput("safir_web", "Running API server on")
        asyncio.run(run_combined_flow())

    if len(env.Syslog()) != 0:
        log("Unexpected syslog output:\n" + env.Syslog())
        return 1

    if not env.ReturnCodesOk():
        log("Some process exited with an unexpected value")
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
