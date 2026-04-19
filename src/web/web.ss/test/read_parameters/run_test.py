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
import math
from urllib import request, error
import websockets
from testenv import TestEnv, TestEnvStopper, log

PREFIX = "WebTest.Parameters"


def parse_arguments():
    parser = argparse.ArgumentParser("websocket/rest parameter reading test")
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


async def ws_get_parameter(client, param_name):
    """Send a getParameter request via WebSocket and return the result."""
    full_name = f"{PREFIX}.{param_name}"
    rid = f"get-ws-{param_name}"
    await client.send(json.dumps({
        "jsonrpc": "2.0",
        "method": "getParameter",
        "params": {"parameter": full_name},
        "id": rid
    }))
    response = await client.wait_for_response(rid)
    if "error" in response:
        raise AssertionError(f"WS getParameter {full_name} returned error: {response['error']}")
    return response["result"]


def rest_get_parameter(rest_base, param_name):
    """Fetch a parameter via REST GET /parameter?name=... and return the result."""
    full_name = f"{PREFIX}.{param_name}"
    status, result = rest_get(f"{rest_base}/parameter?name={full_name}")
    if status != 200:
        raise AssertionError(f"REST getParameter {full_name} returned HTTP {status}: {result}")
    if "error" in result:
        raise AssertionError(f"REST getParameter {full_name} returned error: {result['error']}")
    # The REST response wraps the value in {"result": <value>}
    return result["result"]


def assert_equal(desc, actual, expected):
    if actual != expected:
        raise AssertionError(f"{desc}: expected {expected!r}, got {actual!r}")


def assert_close(desc, actual, expected, rel_tol=1e-5):
    if not math.isclose(float(actual), float(expected), rel_tol=rel_tol):
        raise AssertionError(f"{desc}: expected ~{expected}, got {actual}")


async def run_parameter_tests():
    rest_base = "http://127.0.0.1:10000"
    client = None

    try:
        client = WsClient("ws://127.0.0.1:10000")

        # ===== Single value parameters =====

        # --- MyBooleanPar ---
        log("Testing MyBooleanPar")
        ws_r = await ws_get_parameter(client, "MyBooleanPar")
        assert_equal("WS MyBooleanPar", ws_r, True)
        rest_r = rest_get_parameter(rest_base, "MyBooleanPar")
        assert_equal("REST MyBooleanPar", rest_r, True)
        log("MyBooleanPar OK")

        # --- MyInt32Par ---
        log("Testing MyInt32Par")
        ws_r = await ws_get_parameter(client, "MyInt32Par")
        assert_equal("WS MyInt32Par", ws_r, 42)
        rest_r = rest_get_parameter(rest_base, "MyInt32Par")
        assert_equal("REST MyInt32Par", rest_r, 42)
        log("MyInt32Par OK")

        # --- MyInt64Par ---
        log("Testing MyInt64Par")
        ws_r = await ws_get_parameter(client, "MyInt64Par")
        assert_equal("WS MyInt64Par", ws_r, 1234567890123)
        rest_r = rest_get_parameter(rest_base, "MyInt64Par")
        assert_equal("REST MyInt64Par", rest_r, 1234567890123)
        log("MyInt64Par OK")

        # --- MyFloat32Par ---
        log("Testing MyFloat32Par")
        ws_r = await ws_get_parameter(client, "MyFloat32Par")
        assert_close("WS MyFloat32Par", ws_r, 3.14)
        rest_r = rest_get_parameter(rest_base, "MyFloat32Par")
        assert_close("REST MyFloat32Par", rest_r, 3.14)
        log("MyFloat32Par OK")

        # --- MyFloat64Par ---
        log("Testing MyFloat64Par")
        ws_r = await ws_get_parameter(client, "MyFloat64Par")
        assert_close("WS MyFloat64Par", ws_r, 2.718281828459)
        rest_r = rest_get_parameter(rest_base, "MyFloat64Par")
        assert_close("REST MyFloat64Par", rest_r, 2.718281828459)
        log("MyFloat64Par OK")

        # --- MyStringPar ---
        log("Testing MyStringPar")
        ws_r = await ws_get_parameter(client, "MyStringPar")
        assert_equal("WS MyStringPar", ws_r, "Hello World")
        rest_r = rest_get_parameter(rest_base, "MyStringPar")
        assert_equal("REST MyStringPar", rest_r, "Hello World")
        log("MyStringPar OK")

        # --- MyBinaryPar (base64 of "Hello World") ---
        log("Testing MyBinaryPar")
        ws_r = await ws_get_parameter(client, "MyBinaryPar")
        assert_equal("WS MyBinaryPar", ws_r, "SGVsbG8gV29ybGQ=")
        rest_r = rest_get_parameter(rest_base, "MyBinaryPar")
        assert_equal("REST MyBinaryPar", rest_r, "SGVsbG8gV29ybGQ=")
        log("MyBinaryPar OK")

        # --- MyTypeIdPar ---
        log("Testing MyTypeIdPar")
        ws_r = await ws_get_parameter(client, "MyTypeIdPar")
        assert_equal("WS MyTypeIdPar", ws_r, "Safir.Dob.Entity")
        rest_r = rest_get_parameter(rest_base, "MyTypeIdPar")
        assert_equal("REST MyTypeIdPar", rest_r, "Safir.Dob.Entity")
        log("MyTypeIdPar OK")

        # --- MyInstanceIdPar ---
        log("Testing MyInstanceIdPar")
        ws_r = await ws_get_parameter(client, "MyInstanceIdPar")
        assert_equal("WS MyInstanceIdPar", ws_r, "myNamedInstance")
        rest_r = rest_get_parameter(rest_base, "MyInstanceIdPar")
        assert_equal("REST MyInstanceIdPar", rest_r, "myNamedInstance")
        log("MyInstanceIdPar OK")

        # --- MyHandlerIdPar ---
        log("Testing MyHandlerIdPar")
        ws_r = await ws_get_parameter(client, "MyHandlerIdPar")
        assert_equal("WS MyHandlerIdPar", ws_r, "myHandler")
        rest_r = rest_get_parameter(rest_base, "MyHandlerIdPar")
        assert_equal("REST MyHandlerIdPar", rest_r, "myHandler")
        log("MyHandlerIdPar OK")

        # --- MyChannelIdPar ---
        log("Testing MyChannelIdPar")
        ws_r = await ws_get_parameter(client, "MyChannelIdPar")
        assert_equal("WS MyChannelIdPar", ws_r, "myChannel")
        rest_r = rest_get_parameter(rest_base, "MyChannelIdPar")
        assert_equal("REST MyChannelIdPar", rest_r, "myChannel")
        log("MyChannelIdPar OK")

        # --- MyEntityIdPar ---
        log("Testing MyEntityIdPar")
        ws_r = await ws_get_parameter(client, "MyEntityIdPar")
        assert_equal("WS MyEntityIdPar typeId",     ws_r["typeId"],     "Safir.Dob.Entity")
        assert_equal("WS MyEntityIdPar instanceId", ws_r["instanceId"], "theOnlyInstance")
        rest_r = rest_get_parameter(rest_base, "MyEntityIdPar")
        assert_equal("REST MyEntityIdPar typeId",     rest_r["typeId"],     "Safir.Dob.Entity")
        assert_equal("REST MyEntityIdPar instanceId", rest_r["instanceId"], "theOnlyInstance")
        log("MyEntityIdPar OK")

        # --- MyEnumPar ---
        log("Testing MyEnumPar")
        ws_r = await ws_get_parameter(client, "MyEnumPar")
        assert_equal("WS MyEnumPar", ws_r, "Normal")
        rest_r = rest_get_parameter(rest_base, "MyEnumPar")
        assert_equal("REST MyEnumPar", rest_r, "Normal")
        log("MyEnumPar OK")

        # --- MyObjectPar (Safir.Dob.ResponseErrorInfo) ---
        log("Testing MyObjectPar")
        ws_r = await ws_get_parameter(client, "MyObjectPar")
        assert_equal("WS MyObjectPar _DouType", ws_r.get("_DouType"), "Safir.Dob.ResponseErrorInfo")
        assert_equal("WS MyObjectPar Member",   ws_r.get("Member"),   1)
        assert_equal("WS MyObjectPar Index",    ws_r.get("Index"),    0)
        assert_equal("WS MyObjectPar Code",     ws_r.get("Code"),     "SafirReqErr")
        rest_r = rest_get_parameter(rest_base, "MyObjectPar")
        assert_equal("REST MyObjectPar _DouType", rest_r.get("_DouType"), "Safir.Dob.ResponseErrorInfo")
        assert_equal("REST MyObjectPar Member",   rest_r.get("Member"),   1)
        assert_equal("REST MyObjectPar Index",    rest_r.get("Index"),    0)
        assert_equal("REST MyObjectPar Code",     rest_r.get("Code"),     "SafirReqErr")
        log("MyObjectPar OK")

        # ===== Array parameters =====

        # --- MyInt32ArrayPar ---
        log("Testing MyInt32ArrayPar")
        ws_r = await ws_get_parameter(client, "MyInt32ArrayPar")
        assert_equal("WS MyInt32ArrayPar", ws_r, [10, 20, 30])
        rest_r = rest_get_parameter(rest_base, "MyInt32ArrayPar")
        assert_equal("REST MyInt32ArrayPar", rest_r, [10, 20, 30])
        log("MyInt32ArrayPar OK")

        # --- MyStringArrayPar ---
        log("Testing MyStringArrayPar")
        ws_r = await ws_get_parameter(client, "MyStringArrayPar")
        assert_equal("WS MyStringArrayPar", ws_r, ["first", "second", "third"])
        rest_r = rest_get_parameter(rest_base, "MyStringArrayPar")
        assert_equal("REST MyStringArrayPar", rest_r, ["first", "second", "third"])
        log("MyStringArrayPar OK")

        # --- MyEntityIdArrayPar ---
        log("Testing MyEntityIdArrayPar")
        ws_r = await ws_get_parameter(client, "MyEntityIdArrayPar")
        assert_equal("WS MyEntityIdArrayPar length", len(ws_r), 2)
        assert_equal("WS MyEntityIdArrayPar[0] typeId",     ws_r[0]["typeId"],     "Safir.Dob.Entity")
        assert_equal("WS MyEntityIdArrayPar[0] instanceId", ws_r[0]["instanceId"], "one")
        assert_equal("WS MyEntityIdArrayPar[1] typeId",     ws_r[1]["typeId"],     "Safir.Dob.Entity")
        assert_equal("WS MyEntityIdArrayPar[1] instanceId", ws_r[1]["instanceId"], "two")
        rest_r = rest_get_parameter(rest_base, "MyEntityIdArrayPar")
        assert_equal("REST MyEntityIdArrayPar length", len(rest_r), 2)
        assert_equal("REST MyEntityIdArrayPar[0] typeId",     rest_r[0]["typeId"],     "Safir.Dob.Entity")
        assert_equal("REST MyEntityIdArrayPar[0] instanceId", rest_r[0]["instanceId"], "one")
        assert_equal("REST MyEntityIdArrayPar[1] typeId",     rest_r[1]["typeId"],     "Safir.Dob.Entity")
        assert_equal("REST MyEntityIdArrayPar[1] instanceId", rest_r[1]["instanceId"], "two")
        log("MyEntityIdArrayPar OK")

        # --- MyEnumArrayPar ---
        log("Testing MyEnumArrayPar")
        ws_r = await ws_get_parameter(client, "MyEnumArrayPar")
        assert_equal("WS MyEnumArrayPar", ws_r, ["Normal", "Warning", "Low"])
        rest_r = rest_get_parameter(rest_base, "MyEnumArrayPar")
        assert_equal("REST MyEnumArrayPar", rest_r, ["Normal", "Warning", "Low"])
        log("MyEnumArrayPar OK")

        # --- MyObjectArrayPar ---
        log("Testing MyObjectArrayPar")
        ws_r = await ws_get_parameter(client, "MyObjectArrayPar")
        assert_equal("WS MyObjectArrayPar length", len(ws_r), 2)
        assert_equal("WS MyObjectArrayPar[0] _DouType", ws_r[0].get("_DouType"), "Safir.Dob.ResponseErrorInfo")
        assert_equal("WS MyObjectArrayPar[0] Member",   ws_r[0].get("Member"),   1)
        assert_equal("WS MyObjectArrayPar[0] Index",    ws_r[0].get("Index"),    0)
        assert_equal("WS MyObjectArrayPar[0] Code",     ws_r[0].get("Code"),     "SafirReqErr")
        assert_equal("WS MyObjectArrayPar[1] _DouType", ws_r[1].get("_DouType"), "Safir.Dob.ResponseErrorInfo")
        assert_equal("WS MyObjectArrayPar[1] Member",   ws_r[1].get("Member"),   2)
        assert_equal("WS MyObjectArrayPar[1] Index",    ws_r[1].get("Index"),    1)
        assert_equal("WS MyObjectArrayPar[1] Code",     ws_r[1].get("Code"),     "SafirNullMember")
        rest_r = rest_get_parameter(rest_base, "MyObjectArrayPar")
        assert_equal("REST MyObjectArrayPar length", len(rest_r), 2)
        assert_equal("REST MyObjectArrayPar[0] _DouType", rest_r[0].get("_DouType"), "Safir.Dob.ResponseErrorInfo")
        assert_equal("REST MyObjectArrayPar[0] Member",   rest_r[0].get("Member"),   1)
        assert_equal("REST MyObjectArrayPar[0] Index",    rest_r[0].get("Index"),    0)
        assert_equal("REST MyObjectArrayPar[0] Code",     rest_r[0].get("Code"),     "SafirReqErr")
        assert_equal("REST MyObjectArrayPar[1] _DouType", rest_r[1].get("_DouType"), "Safir.Dob.ResponseErrorInfo")
        assert_equal("REST MyObjectArrayPar[1] Member",   rest_r[1].get("Member"),   2)
        assert_equal("REST MyObjectArrayPar[1] Index",    rest_r[1].get("Index"),    1)
        assert_equal("REST MyObjectArrayPar[1] Code",     rest_r[1].get("Code"),     "SafirNullMember")
        log("MyObjectArrayPar OK")

        # ===== Dictionary parameters =====

        # --- MyStringToFloat64DictPar ---
        log("Testing MyStringToFloat64DictPar")
        ws_r = await ws_get_parameter(client, "MyStringToFloat64DictPar")
        ws_dict = {e["key"]: e["value"] for e in ws_r}
        assert_equal("WS MyStringToFloat64DictPar len",  len(ws_dict), 2)
        assert_close("WS MyStringToFloat64DictPar Hot",  ws_dict["Hot"],  65.4321)
        assert_close("WS MyStringToFloat64DictPar Cold", ws_dict["Cold"], -12.3456)
        rest_r = rest_get_parameter(rest_base, "MyStringToFloat64DictPar")
        rest_dict = {e["key"]: e["value"] for e in rest_r}
        assert_equal("REST MyStringToFloat64DictPar len",  len(rest_dict), 2)
        assert_close("REST MyStringToFloat64DictPar Hot",  rest_dict["Hot"],  65.4321)
        assert_close("REST MyStringToFloat64DictPar Cold", rest_dict["Cold"], -12.3456)
        log("MyStringToFloat64DictPar OK")

        # --- MyInt32ToStringDictPar ---
        log("Testing MyInt32ToStringDictPar")
        ws_r = await ws_get_parameter(client, "MyInt32ToStringDictPar")
        ws_dict = {e["key"]: e["value"] for e in ws_r}
        assert_equal("WS MyInt32ToStringDictPar len", len(ws_dict), 2)
        assert_equal("WS MyInt32ToStringDictPar[1]",  ws_dict[1], "one")
        assert_equal("WS MyInt32ToStringDictPar[2]",  ws_dict[2], "two")
        rest_r = rest_get_parameter(rest_base, "MyInt32ToStringDictPar")
        rest_dict = {e["key"]: e["value"] for e in rest_r}
        assert_equal("REST MyInt32ToStringDictPar len", len(rest_dict), 2)
        assert_equal("REST MyInt32ToStringDictPar[1]",  rest_dict[1], "one")
        assert_equal("REST MyInt32ToStringDictPar[2]",  rest_dict[2], "two")
        log("MyInt32ToStringDictPar OK")

        # --- MyEnumToInt32DictPar ---
        log("Testing MyEnumToInt32DictPar")
        ws_r = await ws_get_parameter(client, "MyEnumToInt32DictPar")
        ws_dict = {e["key"]: e["value"] for e in ws_r}
        assert_equal("WS MyEnumToInt32DictPar len",     len(ws_dict), 3)
        assert_equal("WS MyEnumToInt32DictPar Normal",  ws_dict["Normal"],  100)
        assert_equal("WS MyEnumToInt32DictPar Warning", ws_dict["Warning"], 80)
        assert_equal("WS MyEnumToInt32DictPar Low",     ws_dict["Low"],     50)
        rest_r = rest_get_parameter(rest_base, "MyEnumToInt32DictPar")
        rest_dict = {e["key"]: e["value"] for e in rest_r}
        assert_equal("REST MyEnumToInt32DictPar len",     len(rest_dict), 3)
        assert_equal("REST MyEnumToInt32DictPar Normal",  rest_dict["Normal"],  100)
        assert_equal("REST MyEnumToInt32DictPar Warning", rest_dict["Warning"], 80)
        assert_equal("REST MyEnumToInt32DictPar Low",     rest_dict["Low"],     50)
        log("MyEnumToInt32DictPar OK")

        log("All parameter tests passed!")

    finally:
        if client is not None:
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
        asyncio.run(run_parameter_tests())

    if not env.ReturnCodesOk():
        log("Some process exited with an unexpected value")
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
