#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013, 2025 (http://safirsdkcore.com)
#
# Created by: Lars Hagström / lars.hagstrom@consoden.se
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
import subprocess, os, time, sys, signal, re, argparse, logging, socket, struct
from safe_print import *
from testenv import TestEnv, TestEnvStopper, log
import asyncio, json, websockets
from contextlib import contextmanager, aclosing
import traceback

failed_tests = set()

def parse_arguments():
    parser = argparse.ArgumentParser("test script")
    parser.add_argument("--sender", required=True)
    parser.add_argument("--backdoor", required=True)
    parser.add_argument("--safir-control", required=True)
    parser.add_argument("--dose-main", required=True)
    parser.add_argument("--dope-main", required=True)
    parser.add_argument("--safir-show-config", required=True)
    parser.add_argument("--safir-websocket", required=True)
    parser.add_argument("--safir-status", required=True)
    parser.add_argument("--dobexplorer", required=True)
    return parser.parse_args()

@contextmanager
def test_case(name, args):
    global failed_tests

    env = None
    try:
        log("=== Start: " + name + " ===")
        env = TestEnv(args.safir_control,
                      args.dose_main,
                      args.dope_main,
                      args.safir_show_config)
        env.launchProcess("safir_websocket", args.safir_websocket)
        #env.launchProcess("dobexplorer", args.dobexplorer)
        yield env

    except Exception as e:
        failed_tests.add(name)
        logging.exception('Got exception')
        log(e)
        log("*** Test failed: " + name)
    finally:
        log("--- Finished: " + name + " ---")
        time.sleep(1.0)
        if env is not None:
            env.killprocs()


def fail(message, output):
    log("Failed!", message)
    if output is not None:
        log("OUTPUT:", output)
    raise AssertionError(message)

def dict_to_sorted_list(d):
    l = list()
    for k, v in d.items():
        l.append(k + " - " + v)
    l.sort()
    return l

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

def check_error(t):
    if not t.cancelled() and t.exception() is not None:
        e = t.exception()
        log("".join(traceback.format_exception(None, e, e.__traceback__)))
        sys.exit(1)

# ==============================================================================================
# Simple Safir application. The constructor will connect to the DOB and subscribe for all
# entites and all registrations.
# It will start a running task that handles send and receive to the Dob. Stop will end the task
# ==============================================================================================
class SafirApp:
    def __init__(self):
        self.sendQueue = asyncio.Queue()
        self.entities = dict()
        self.registrations = dict()
        self.last_pool_update = time.time()
        self.stopped = False
        self._setup_dob()
        self.task = asyncio.create_task(self._run())
        self.task.add_done_callback(check_error)


    async def aclose(self):
        if not self.stopped:
            await self.sendQueue.put(None)
            await self.sendQueue.join()
            await self.task
            self.stopped = True

    async def send(self, msg):
        await self.sendQueue.put(msg)

    async def wait_for_reg(self, typeAndHandler):
        while typeAndHandler not in self.registrations:
            log("--- Waiting for reg", typeAndHandler)
            await asyncio.sleep(1)

    async def wait_for_inst(self, typeAndInstance):
        while typeAndInstance not in self.entities:
            log("--- Waiting for instance", typeAndInstance)
            await asyncio.sleep(1)

    def prefix(self, typeAndInstance, prefix):
        if typeAndInstance not in self.entities:
            return None
        ent = json.loads(self.entities[typeAndInstance])
        for p in ent["Prefixes"]:
            if p["key"] == prefix:
                return p["value"]
        return None

    async def wait_for_prefix(self, typeAndInstance, prefix, expectedValue):
        while True:
            val = self.prefix(sender_tracer_status_entityid, prefix)
            log(f"Waiting for prefix {prefix} to change to {expectedValue}, currently {val}")
            if val == expectedValue:
                log(" - got it")
                return
            await asyncio.sleep(1)

    def dump_pool(self):
        print("=== Pool dump ===")
        print("  Registrations:")
        for val in dict_to_sorted_list(self.registrations):
            print("    " + val)
        print("  Entities:")
        for val in dict_to_sorted_list(self.entities):
            print("    " + val)
        log("--------------")

    async def _run(self):
        uri = "ws://localhost:10000"
        for connectTry in range(10):
            try:
                async with websockets.connect(uri) as ws:
                    self.ws = ws
                    log("--- Node is connected to the DOB")
                    await asyncio.gather(self._reader(), self._sender())
                    break;
            except Exception as e:
                log ("Caught Exception while connecting, will retry")
                await asyncio.sleep(5)
                if connectTry == 9:
                    log("*** Failed to connect, Exception! uri= " + uri, e)
                    log(traceback.format_exc())
                    raise

    async def _reader(self):
        async for message in self.ws:
            try:
                msg = json.loads(message)
                callback = method(msg)
                if callback in ["onNewEntity", "onUpdatedEntity"]:
                    self.last_pool_update = time.time()
                    entity_id = dou_type(msg) + ":" + str(instance_id(msg))
                    self.entities[entity_id] = json.dumps(entity(msg))
                    log("--- " + callback + ":" + entity_id + ":" + json.dumps(entity(msg)))
                elif callback == "onDeletedEntity":
                    self.last_pool_update = time.time()
                    entity_id = dou_type(msg) + ":" + str(instance_id(msg))
                    self.entities.pop(entity_id, None)
                elif callback == "onRegistered":
                    self.last_pool_update = time.time()
                    handler = dou_type(msg) + ":" + str(handler_id(msg))
                    self.registrations[handler] = "Registered"
                    log("--- onRegistered: " + handler)
                elif callback == "onUnregistered":
                    self.last_pool_update = time.time()
                    handler = dou_type(msg) + ":" + str(handler_id(msg))
                    self.registrations.pop(handler, None)
                    log("--- onUnregistered: " + handler)
                elif "result" in msg and msg["result"] != "OK":
                    result_id = msg["id"] if "id" in msg else "None"
                    log("--- received result " + str(msg["result"]) + " with id=" + str(result_id))
                elif "error" in msg:
                    log("*** received error response from safir_websocket:", message)
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
        app_name = "app"
        self.sendQueue.put_nowait('{"method": "open", "params": {"connectionName": "' + app_name + '"}, "id": 1}')
        self.sendQueue.put_nowait('{"method": "subscribeEntity", "params": {"typeId": "Safir.Dob.Entity"}, "id": 2}')
        self.sendQueue.put_nowait('{"method": "subscribeRegistration", "params": {"typeId": "Safir.Dob.Entity"}, "id": 3}')

class TracerReceiver(asyncio.DatagramProtocol):
    """
    Small helper that listens for tracer UDP datagrams on port 49500.
    Received (data, addr) tuples are queued so tests can await them.
    """
    def __init__(self, loop: asyncio.AbstractEventLoop | None = None, port: int = 49500):
        # Resolve the currently running loop automatically if none supplied
        self._loop = loop or asyncio.get_running_loop()
        self._port = port
        self._transport: asyncio.DatagramTransport | None = None
        self._queue: asyncio.Queue = asyncio.Queue()

    # asyncio.DatagramProtocol callbacks ----------------------------------
    def connection_made(self, transport: asyncio.BaseTransport) -> None:
        self._transport = transport  # type: ignore[assignment]

    def datagram_received(self, data: bytes, addr) -> None:  # noqa: ANN001
        # Parse according to TracerDataHeader layout defined in TracerData.h
        # Updated packet layout (see TracerDataHeader in C++ code):
        #  int64  incarnationId
        #  int64  senderId
        #  uint32 sequenceNumber
        #  uint32 payloadLength
        #  uint64 timestampUsec
        #  uint8  programNameLength
        #  uint8  nodeNameLength
        #  uint16 padding
        HEADER_SIZE = 36  # bytes
        if len(data) < HEADER_SIZE:
            log(f"TracerReceiver: short datagram ({len(data)} bytes) from {addr}")
            return

        (incarnation_id,
         sender_id,
         sequence_number,
         payload_length,
         timestamp_usec,
         program_len,
         node_len,
         _padding) = struct.unpack_from("<qqIIQBBH", data)

        names_total_len = program_len + node_len
        payload_start   = HEADER_SIZE + names_total_len

        if len(data) < payload_start + payload_length:
            log(f"TracerReceiver: truncated payload from {addr}")
            return

        # Extract variable-length strings and payload
        program_name_bytes = data[HEADER_SIZE : HEADER_SIZE + program_len]
        node_name_bytes    = data[HEADER_SIZE + program_len : payload_start]
        payload_bytes      = data[payload_start : payload_start + payload_length]

        try:
            program_name = program_name_bytes.decode("utf-8", errors="replace")
            node_name    = node_name_bytes.decode("utf-8", errors="replace")
            payload      = payload_bytes.decode("utf-8", errors="replace")
        except UnicodeDecodeError:
            # This should never happen with errors="replace", but be safe
            payload = payload_bytes.decode("utf-8", errors="replace")

        header = {
            "incarnationId": incarnation_id,
            "senderId": sender_id,
            "sequenceNumber": sequence_number,
            "payloadLength": payload_length,
            "timestampUsec": timestamp_usec,
            "programName": program_name,
            "nodeName": node_name,
        }

        # Expose decoded text instead of raw bytes
        self._queue.put_nowait((header, payload, addr))

    def error_received(self, exc: Exception) -> None:  # noqa: D401
        log(f"TracerReceiver socket error: {exc}")

    # public helpers -------------------------------------------------------
    async def start(self) -> None:
        """
        Bind the UDP socket to the loop-back interface, join the multicast
        group 224.11.11.12 and start listening.
        """
        # Create socket manually so we can tweak multicast options
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

        sock.bind(("", self._port))

        # Join multicast group on the loop-back interface
        mreq = struct.pack(
            "4s4s",
            socket.inet_aton("224.11.11.12"),
            socket.inet_aton("127.0.0.1"),
        )
        sock.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)

        await self._loop.create_datagram_endpoint(
            lambda: self,
            sock=sock,
        )

    async def recv(self) -> tuple[dict, str, tuple[str, int]]:
        """
        Await one received datagram.

        Returns:
            (header_dict, payload_str, (ip, port))
        """
        return await self._queue.get()

    async def wait_for_output(self, expected: str) -> str:
        """
        Block until a payload containing `expected` is received.

        Returns:
            The matched payload string.
        """
        while True:
            header, payload, _ = await self.recv()
            if expected in payload:
                return payload

    # ---------------------------------------------------------------
    # Async context-manager helpers so we can use `async with TracerReceiver()`
    # ---------------------------------------------------------------
    async def __aenter__(self):
        await self.start()
        return self

    async def __aexit__(self, exc_type, exc, tb):
        self.aclose()
        # propagate exception (if any) to outer scope
        return False

    def aclose(self) -> None:
        if self._transport is not None:
            self._transport.close()
            self._transport = None


async def bd_all_on_off(args):
    with test_case("bd_all_on_off",args) as env:
        env.launchProcess("sender", args.sender)
        output = env.WaitForOutput("sender", "Have logged 10 times")
        if output.count("blahonga") != 0 or output.count("foobar") != 0:
            fail("Unexpected logs before enabling prefixes", output)

        #turn the prefixes on
        subprocess.call((args.backdoor, "all", "on"))
        env.WaitForOutput("sender", "blahonga, blahonga, blahonga")
        env.WaitForOutput("sender", "foobar")
        #if these outputs don't arrive we'll time out.

        subprocess.call((args.backdoor, "all", "off"))

        #reset the output so we don't trigger on the output from the start of the test
        env.ResetOutput("sender")
        #this is two of the wcout logs in a row, meaning the tracers are off
        env.WaitForOutput("sender", "times.\nHave")

async def bd_all_on_off_udp_tracer(args):
    with test_case("bd_all_on_off_udp_tracer",args) as env:
        env.launchProcess("sender", args.sender)
        env.WaitForOutput("sender", "Have logged 10 times")
        async with TracerReceiver() as receiver:
            #turn the prefixes on
            subprocess.call((args.backdoor, "all", "on"))
            await receiver.wait_for_output("blahonga, blahonga, blahonga")
            await receiver.wait_for_output("foobar")

async def bd_prefix_on_off(args):
    with test_case("bd_prefix_on_off",args) as env:
        #test turning individual prefix on
        env.launchProcess("sender", args.sender)
        output = env.WaitForOutput("sender", "Have logged 10 times")
        if output.count("blahonga") != 0 or output.count("foobar") != 0:
            fail("Unexpected logs before enabling prefixes", output)

        #turn the prefixes on
        subprocess.call((args.backdoor, "Razor", "on"))

        #wait for (at least) two razor outputs
        output = env.WaitForOutput("sender", "foobar")
        env.ResetOutput("sender")
        output += env.WaitForOutput("sender", "foobar")

        #check that we don't have any Rymdbörje output
        if output.count("blahonga") != 0:
            fail("unexpected got logs that should not be on", output)

        subprocess.call((args.backdoor, "Razor", "off"))

        #no need to reset, since we did it already.
        #this is two of the wcout logs in a row, meaning the tracers are off
        env.WaitForOutput("sender", "times.\nHave")


async def bd_conn_on_off(args):
    with test_case("bd_conn_on_off",args) as env:
        env.launchProcess("sender", args.sender)
        output = env.WaitForOutput("sender", "Have logged 10 times")
        subprocess.call((args.backdoor, "-c", "incorrect_connection_name", "all", "on"))
        output = env.WaitForOutput("sender", "Have logged 20 times")
        if output.count("blahonga") != 0 or output.count("foobar") != 0:
            fail("Unexpected logs before enabling prefixes", output)

        #turn the prefixes on
        subprocess.call((args.backdoor, "-c", "sender_connection_name", "all", "on"))
        env.WaitForOutput("sender", "blahonga, blahonga, blahonga")
        env.WaitForOutput("sender", "foobar")
        #if these outputs don't arrive we'll time out.

        subprocess.call((args.backdoor, "-c", "tracer_backdoor_sender", "all", "off"))

        #reset the output so we don't trigger on the output from the start of the test
        env.ResetOutput("sender")
        #this is two of the wcout logs in a row, meaning the tracers are off
        env.WaitForOutput("sender", "times.\nHave")

sender_tracer_status_entityid = "Safir.Application.TracerStatus:910291948783220530"
sender_tracer_status_instanceid = "910291948783220530"

async def tracer_status_entity_creation(args):
    with test_case("tracer_status_entity_creation",args) as env:
        env.launchProcess("safir_status", args.safir_status)
        async with aclosing(SafirApp()) as app:
            await app.wait_for_reg("Safir.Application.TracerStatus:DEFAULT_HANDLER")
            if "Safir.Application.TracerStatus:910291948783220530" in app.registrations:
                fail("There should not be a tracer status entity here now")
            env.launchProcess("sender", args.sender)
            await app.wait_for_inst(sender_tracer_status_entityid)

            output = env.WaitForOutput("sender", "Have logged 10 times")
            if output.count("blahonga") != 0 or output.count("foobar") != 0:
                fail("Unexpected logs before enabling prefixes", output)

async def tracer_status_entity_updated(args):
    with test_case("tracer_status_entity_updated",args) as env:
        env.launchProcess("safir_status", args.safir_status)
        async with aclosing(SafirApp()) as app:
            env.launchProcess("sender", args.sender)
            await app.wait_for_inst(sender_tracer_status_entityid)

            env.launchProcess("sender", args.sender)
            output = env.WaitForOutput("sender", "Have logged 10 times")
            if output.count("blahonga") != 0 or output.count("foobar") != 0:
                fail("Unexpected logs before enabling prefixes", output)

            #turn the prefixes on
            subprocess.call((args.backdoor, "Razor", "on"))
            while not app.prefix(sender_tracer_status_entityid, "Razor"):
                log("Waiting for updated prefix")
                await asyncio.sleep(1)

            #wait for (at least) two razor outputs
            output = env.WaitForOutput("sender", "foobar")
            env.ResetOutput("sender")
            output += env.WaitForOutput("sender", "foobar")

            #check that we don't have any Rymdbörje output
            if output.count("blahonga") != 0:
                fail("unexpected got logs that should not be on", output)

            subprocess.call((args.backdoor, "Razor", "off"))

            while app.prefix(sender_tracer_status_entityid, "Razor"):
                log("Waiting for updated prefix")
                await asyncio.sleep(1)

            #no need to reset, since we did it already.
            #this is two of the wcout logs in a row, meaning the tracers are off
            env.WaitForOutput("sender", "times.\nHave")

async def bd_prefix_on_off_entity(args):
    with test_case("bd_prefix_on_off_entity",args) as env:
        env.launchProcess("safir_status", args.safir_status)
        async with aclosing(SafirApp()) as app:
            #test turning individual prefix on
            env.launchProcess("sender", args.sender)
            output = env.WaitForOutput("sender", "Have logged 10 times")
            if output.count("blahonga") != 0 or output.count("foobar") != 0:
                fail("Unexpected logs before enabling prefixes", output)

            while app.prefix(sender_tracer_status_entityid, "Razor") != False:
                log("Waiting for updated prefix")
                await asyncio.sleep(1)

            #turn the prefixes on
            await app.send('{"method": "updateRequest", "params": {"entity": {"_DouType": "Safir.Application.TracerStatus", "Prefixes": [{"key": "Razor", "value": true}]}, "instanceId": ' + sender_tracer_status_instanceid + '}, "id": 501}')

            await app.wait_for_prefix(sender_tracer_status_instanceid, "Razor", True)

            #wait for (at least) two razor outputs
            output = env.WaitForOutput("sender", "foobar")
            env.ResetOutput("sender")
            output += env.WaitForOutput("sender", "foobar")

            #check that we don't have any Rymdbörje output
            if output.count("blahonga") != 0:
                fail("unexpected got logs that should not be on", output)

            await app.send('{"method": "updateRequest", "params": {"entity": {"_DouType": "Safir.Application.TracerStatus", "Prefixes": [{"key": "Razor", "value": false}]}, "instanceId": ' + sender_tracer_status_instanceid + '}, "id": 505}')

            await app.wait_for_prefix(sender_tracer_status_instanceid, "Razor", False)

            #no need to reset, since we did it already.
            #this is two of the wcout logs in a row, meaning the tracers are off
            env.WaitForOutput("sender", "times.\nHave")


async def main(args):
    await bd_all_on_off(args)
    await bd_all_on_off_udp_tracer(args)
    await bd_prefix_on_off(args)
    await bd_conn_on_off(args)
    await tracer_status_entity_creation(args)
    await tracer_status_entity_updated(args)
    await bd_prefix_on_off_entity(args)

if __name__ == "__main__":
    asyncio.run(main(parse_arguments()))
    for failed in failed_tests:
        log("*** Failed: ", failed)
    sys.exit(1 if len(failed_tests) > 0 else 0)
