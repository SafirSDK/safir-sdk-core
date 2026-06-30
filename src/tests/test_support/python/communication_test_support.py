"""Shared helpers for tests that drive Communication's DebugCommandServer.

DebugCommandServer (in libdose_communication) listens on a multicast
group/port and flips Parameters::NetworkEnabled for the safir_control whose
session_id matches. Tests use this to simulate a network outage without
touching real network state.

When the multicast packet can't be delivered (firewall blocking the port,
multicast routing dropping the group), tests that call set_network_state will
simply time out at ~10 minutes per subtest while waiting for state transitions
that will never happen. Call check_multicast_loopback() once at the top of a
test script to fail in ~2s with a clear diagnostic instead.
"""
import asyncio
import socket
import struct
import sys
import time
import uuid

# Must match DebugCommandServer.h in src/distribution/communication.ss/src/
DEBUG_COMMAND_GROUP = "239.6.6.6"
DEBUG_COMMAND_PORT = 16666


def _build_cmd(state, session_id):
    return bytes(("up " if state else "down ") + session_id, "utf-8")


def set_network_state(state, session_id):
    """Send the up/down control packet to DebugCommandServer (3x for reliability)."""
    cmd = _build_cmd(state, session_id)
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        for _ in range(3):
            sock.sendto(cmd, (DEBUG_COMMAND_GROUP, DEBUG_COMMAND_PORT))
            time.sleep(0.05)
    finally:
        sock.close()


async def async_set_network_state(state, session_id):
    """Async variant of set_network_state; yields between sends with asyncio.sleep."""
    cmd = _build_cmd(state, session_id)
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        for _ in range(3):
            sock.sendto(cmd, (DEBUG_COMMAND_GROUP, DEBUG_COMMAND_PORT))
            await asyncio.sleep(0.05)
    finally:
        sock.close()


def check_multicast_loopback(timeout=2.0):
    """Confirm a packet sent to DEBUG_COMMAND_GROUP:PORT loops back to a local joiner.

    Tests the same kernel path DebugCommandServer relies on. Raises
    AssertionError with a diagnostic if no loopback is observed within
    `timeout` seconds.
    """
    nonce = uuid.uuid4().bytes
    recv = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    recv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    recv.bind(("0.0.0.0", DEBUG_COMMAND_PORT))
    mreq = struct.pack("4sl", socket.inet_aton(DEBUG_COMMAND_GROUP), socket.INADDR_ANY)
    recv.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)
    send = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        send.sendto(nonce, (DEBUG_COMMAND_GROUP, DEBUG_COMMAND_PORT))
        deadline = time.monotonic() + timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                break
            recv.settimeout(remaining)
            try:
                data, _ = recv.recvfrom(4096)
            except socket.timeout:
                break
            if data == nonce:
                return
            # Stray traffic from a concurrent test or process; keep waiting for our nonce.
        sys.exit(
            "Multicast loopback check failed: sent {n}B to {g}:{p} but it did "
            "not come back within {t}s.\n"
            "  This test uses multicast on {g}:{p} to flip "
            "Parameters::NetworkEnabled in safir_control (DebugCommandServer). "
            "Without it, every disconnect/reconnect subtest will simply time "
            "out (~10 min each) instead of doing anything.\n"
            "  Likely cause: firewall blocking UDP {p}, or multicast routing "
            "on this host (rp_filter, missing route for {g}) dropping the "
            "packet.".format(
                n=len(nonce),
                g=DEBUG_COMMAND_GROUP,
                p=DEBUG_COMMAND_PORT,
                t=timeout,
            )
        )
    finally:
        send.close()
        recv.close()
