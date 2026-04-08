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
    parser = argparse.ArgumentParser("multiple connections test")
    parser.add_argument("--safir-control", required=True)
    parser.add_argument("--dose_main", required=True)
    parser.add_argument("--dope_main", required=True)
    parser.add_argument("--safir-show-config", required=True)
    parser.add_argument("--safir_web", required=True)
    return parser.parse_args()


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


class WsClient:
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


async def open_connection(client, connection_name):
    """Open a Dob connection via WebSocket and verify OK."""
    req_id = f"open-{connection_name}"
    await client.send(json.dumps({
        "jsonrpc": "2.0",
        "method": "open",
        "params": {"connectionName": connection_name, "context": 0},
        "id": req_id
    }))
    response = await client.wait_for_response(req_id)
    if response.get("result") != "OK":
        raise AssertionError(f"open({connection_name}) failed: {response}")


def register_entity_handler(rest_base, connection_name, handler_id):
    """Register Safir.Control.Status via REST for the given connection and handler id."""
    url = f"{rest_base}/connections/{connection_name}/handlers/entities/Safir.Control.Status?handler={handler_id}"
    status, result = rest_put(url)
    if status != 200:
        raise AssertionError(f"registerEntityHandler({connection_name}, handler={handler_id}) HTTP status was not 200: {status}")
    if result.get("status") != "OK":
        raise AssertionError(f"registerEntityHandler({connection_name}, handler={handler_id}) failed: {result}")
    log(f"Registered Safir.Control.Status handler={handler_id} on connection '{connection_name}'")


def verify_connections(rest_base, expected_names):
    """Call GET /connections and assert the result matches expected_names (order-independent)."""
    status, result = rest_get(f"{rest_base}/connections")
    if status != 200:
        raise AssertionError(f"GET /connections HTTP status was not 200: {status}")
    if "result" not in result:
        raise AssertionError(f"GET /connections missing 'result': {result}")
    actual = sorted(result["result"])
    expected = sorted(expected_names)
    if actual != expected:
        raise AssertionError(f"GET /connections: expected {expected}, got {actual}")
    log(f"GET /connections returned {actual} as expected")


async def run_multiple_connections_flow():
    rest_base = "http://127.0.0.1:10000"
    ws_uri = "ws://127.0.0.1:10000"

    clients = []
    connection_names = ["conn_alpha", "conn_beta", "conn_gamma"]

    try:
        # ======= STEP 1: No connections open — list should be empty =======
        log("Step 1: GET /connections with no open connections")
        verify_connections(rest_base, [])
        log("Step 1 succeeded")

        # ======= STEP 2: Open first connection, register handler, verify list =======
        log("Step 2: Open first connection (conn_alpha)")
        client1 = WsClient(ws_uri)
        clients.append(client1)
        await open_connection(client1, "conn_alpha")
        register_entity_handler(rest_base, "conn_alpha", 1)
        log("Step 2a: Verify GET /connections contains only conn_alpha")
        verify_connections(rest_base, ["conn_alpha"])
        log("Step 2 succeeded")

        # ======= STEP 3: Open second connection, register handler, verify list =======
        log("Step 3: Open second connection (conn_beta)")
        client2 = WsClient(ws_uri)
        clients.append(client2)
        await open_connection(client2, "conn_beta")
        register_entity_handler(rest_base, "conn_beta", 2)
        log("Step 3a: Verify GET /connections contains conn_alpha and conn_beta")
        verify_connections(rest_base, ["conn_alpha", "conn_beta"])
        log("Step 3 succeeded")

        # ======= STEP 4: Open third connection, register handler, verify list =======
        log("Step 4: Open third connection (conn_gamma)")
        client3 = WsClient(ws_uri)
        clients.append(client3)
        await open_connection(client3, "conn_gamma")
        register_entity_handler(rest_base, "conn_gamma", 3)
        log("Step 4a: Verify GET /connections contains all three connections")
        verify_connections(rest_base, ["conn_alpha", "conn_beta", "conn_gamma"])
        log("Step 4 succeeded")

        # ======= STEP 5: Set entity with wrong and correct handler for each connection =======
        log("Step 5: Set entity instances using wrong and correct handler IDs")
        for conn_name, correct_handler, wrong_handler, instance_id in [
            ("conn_alpha", 1, 2, 10),
            ("conn_beta",  2, 3, 20),
            ("conn_gamma", 3, 1, 30),
        ]:
            entity = {"_DouType": "Safir.Control.Status", "NodeId": instance_id}
            url_wrong   = f"{rest_base}/connections/{conn_name}/entities/Safir.Control.Status/{instance_id}?handler={wrong_handler}"
            url_correct = f"{rest_base}/connections/{conn_name}/entities/Safir.Control.Status/{instance_id}?handler={correct_handler}"

            log(f"Step 5 ({conn_name}): setEntity with wrong handler={wrong_handler} — expect error")
            status, result = rest_put(url_wrong, entity, allow_http_error=True)
            if status != 400:
                raise AssertionError(f"setEntity wrong handler ({conn_name}) expected HTTP 400, got {status}: {result}")
            if "error" not in result:
                raise AssertionError(f"setEntity wrong handler ({conn_name}) expected error body, got: {result}")
            log(f"Step 5 ({conn_name}): got expected error: {result['error']}")

            log(f"Step 5 ({conn_name}): setEntity with correct handler={correct_handler} — expect OK")
            status, result = rest_put(url_correct, entity)
            if status != 200:
                raise AssertionError(f"setEntity correct handler ({conn_name}) HTTP status was not 200: {status}")
            if result.get("status") != "OK":
                raise AssertionError(f"setEntity correct handler ({conn_name}) failed: {result}")
            log(f"Step 5 ({conn_name}): setEntity succeeded")
        log("Step 5 succeeded")

        # ======= STEP 6: Close first connection, verify list shrinks =======
        log("Step 6: Close conn_alpha, verify GET /connections no longer contains it")
        close_id = "close-conn_alpha"
        await client1.send(json.dumps({"jsonrpc": "2.0", "method": "close", "id": close_id}))
        await client1.wait_for_response(close_id)
        await client1.stop()
        clients.remove(client1)
        verify_connections(rest_base, ["conn_beta", "conn_gamma"])
        log("Step 6 succeeded")

        log("All steps succeeded!")
    finally:
        for client in clients:
            await client.stop()
            log("Websocket client stopped")


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
        asyncio.run(run_multiple_connections_flow())

    if len(env.Syslog()) != 0:
        log("Unexpected syslog output:\n" + env.Syslog())
        return 1

    if not env.ReturnCodesOk():
        log("Some process exited with an unexpected value")
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
