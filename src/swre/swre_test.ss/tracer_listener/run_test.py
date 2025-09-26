#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2025 (http://safirsdkcore.com)
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
import argparse
import random
import re
import socket
import struct
import subprocess
import sys
import time
import threading
import queue
from typing import List, Tuple
from testenv import log

# ---------------------------------------------------------------------------

TRACER_HDR_FMT = "<qqIIQBBH"   # layout of TracerDataHeader (little-endian)
TRACER_HDR_SIZE = 36
DEFAULT_GROUP = "239.255.0.1"  # administratively-scoped multicast address


def _build_datagram(inc_id: int,
                    sender_id: int,
                    seq_no: int,
                    program: str,
                    node: str,
                    payload: str) -> bytes:
    """
    Build a single tracer UDP datagram.
    """
    prog_b = program.encode("utf-8")
    node_b = node.encode("utf-8")
    pay_b = payload.encode("utf-8")

    header = struct.pack(
        TRACER_HDR_FMT,
        inc_id,
        sender_id,
        seq_no,
        len(pay_b),
        int(time.time() * 1_000_000),  # timestampUsec
        len(prog_b),
        len(node_b),
        0  # padding
    )
    return header + prog_b + node_b + pay_b


# --------------------------------------------------------------------------- helpers


def _wait_for_pattern(stream,
                      pattern: str,
                      timeout: float = 2.0) -> str:
    """
    Read lines from *stream* until *pattern* (regex) matches or timeout expires.
    Returns the buffer containing the match.
    """
    deadline = time.time() + timeout
    rx = re.compile(pattern)
    buf = ""
    log(f"_wait_for_pattern: waiting for pattern {pattern!r} (timeout {timeout}s)")
    q = queue.Queue()

    def _reader():
        for _line in iter(stream.readline, ''):
            q.put(_line)
        q.put(None)  # EOF sentinel

    threading.Thread(target=_reader, daemon=True).start()

    while True:
        remaining = deadline - time.time()
        if remaining <= 0:
            break
        try:
            line = q.get(timeout=remaining)
        except queue.Empty:
            break  # timeout
        if line is None:  # EOF sentinel
            break
        buf += line
        log(f"_wait_for_pattern read: {line.rstrip()}")
        if rx.search(buf):
            log(f"_wait_for_pattern matched pattern {pattern!r}")
            return buf

    log(f"_wait_for_pattern timeout for pattern {pattern!r}")
    raise RuntimeError(f"Timed-out waiting for pattern {pattern!r}. Buffer:\n{buf}")


def _start_listener(exe: str,
                    extra_args: List[str],
                    group: str,
                    port: int) -> subprocess.Popen:
    # Enable verbose output so we can detect the ready message. Join on loopback by default.
    cmd = [exe, "-v", "-g", group, "-p", str(port)] + extra_args
    log(f"Starting tracer_listener: {' '.join(cmd)}")
    proc = subprocess.Popen(cmd,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE,
                            text=True,
                            bufsize=1)
    _wait_for_pattern(proc.stderr, r"Listening on .*", timeout=5.0)
    log("tracer_listener reported ready")
    return proc


def _send(group: str, port: int, data: bytes) -> None:
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    # Ensure packets loop back to the local host and leave via 127.0.0.1
    sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, 1)
    sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_LOOP, 1)
    sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_IF, socket.inet_aton("127.0.0.1"))
    log(f"Sending {len(data)} bytes to {group}:{port} via loopback")
    sock.sendto(data, (group, port))
    sock.close()


def _stop(proc: subprocess.Popen) -> None:
    if proc.poll() is not None:
        return
    log("Terminating tracer_listener")
    proc.terminate()
    try:
        proc.wait(timeout=2.0)
    except subprocess.TimeoutExpired:
        proc.kill()
        log("Killed tracer_listener after timeout")


# --------------------------------------------------------------------------- individual tests


def test_basic(listener: str):
    group, port = DEFAULT_GROUP, random.randint(50000, 60000)
    proc = _start_listener(listener, [], group, port)

    payload = "Hello tracer\n"
    _send(group, port, _build_datagram(1, 10, 1, "prog", "node", payload))

    _wait_for_pattern(proc.stdout, re.escape(payload))
    _stop(proc)


def test_program_regex(listener: str):
    group, port = DEFAULT_GROUP, random.randint(50000, 60000)
    proc = _start_listener(listener,
                           ["--program-regex", "^accept"],
                           group, port)

    _send(group, port, _build_datagram(1, 1, 1, "rejectme", "n", "NO\n"))
    _send(group, port, _build_datagram(1, 1, 2, "acceptProg", "n", "YES\n"))

    _wait_for_pattern(proc.stdout, r"YES")
    try:
        _wait_for_pattern(proc.stdout, r"NO", timeout=1.0)
        raise AssertionError("--program-regex failed to filter")
    except RuntimeError:
        pass
    _stop(proc)


def test_node_regex(listener: str):
    group, port = DEFAULT_GROUP, random.randint(50000, 60000)
    proc = _start_listener(listener,
                           ["--node-regex", "node42$"],
                           group, port)

    _send(group, port, _build_datagram(1, 2, 1, "p", "evil", "BAD\n"))
    _send(group, port, _build_datagram(1, 2, 2, "p", "goodnode42", "GOOD\n"))

    _wait_for_pattern(proc.stdout, r"GOOD")
    try:
        _wait_for_pattern(proc.stdout, r"BAD", timeout=1.0)
        raise AssertionError("--node-regex failed to filter")
    except RuntimeError:
        pass
    _stop(proc)


def test_show_program_name(listener: str):
    group, port = DEFAULT_GROUP, random.randint(50000, 60000)
    proc = _start_listener(listener, ["--show-program-name"], group, port)

    _send(group, port, _build_datagram(1, 3, 1, "MyProg", "n", "Line\n"))
    _wait_for_pattern(proc.stdout, r"^MyProg Line")
    _stop(proc)


def test_show_node_name(listener: str):
    group, port = DEFAULT_GROUP, random.randint(50000, 60000)
    proc = _start_listener(listener, ["--show-node-name"], group, port)

    _send(group, port, _build_datagram(1, 4, 1, "p", "NODE_X", "abc\n"))
    _wait_for_pattern(proc.stdout, r"^NODE_X abc")
    _stop(proc)


def test_show_send_time(listener: str):
    group, port = DEFAULT_GROUP, random.randint(50000, 60000)
    proc = _start_listener(listener, ["--show-send-time"], group, port)

    _send(group, port, _build_datagram(1, 5, 1, "p", "n", "time\n"))
    _wait_for_pattern(proc.stdout, r"\d{4}-\d{2}-\d{2} .* time")
    _stop(proc)


def test_show_recv_time(listener: str):
    group, port = DEFAULT_GROUP, random.randint(50000, 60000)
    proc = _start_listener(listener, ["--show-recv-time"], group, port)

    _send(group, port, _build_datagram(1, 6, 1, "p", "n", "recv\n"))
    _wait_for_pattern(proc.stdout, r"\d{4}-\d{2}-\d{2} .* recv")
    _stop(proc)


def test_csv(listener: str):
    group, port = DEFAULT_GROUP, random.randint(50000, 60000)
    proc = _start_listener(listener, ["--csv"], group, port)

    _send(group, port, _build_datagram(1, 7, 1, "CSVprog", "CSVnode", "msg\n"))
    _wait_for_pattern(proc.stdout, r'"CSVprog","CSVnode","msg"')
    _stop(proc)


def test_sequence_warning(listener: str):
    """Send sequence numbers 1 and 3 – expect missing-packet message."""
    group, port = DEFAULT_GROUP, random.randint(50000, 60000)
    proc = _start_listener(listener, [], group, port)

    sender = 1234
    _send(group, port, _build_datagram(1, sender, 1, "p", "n", "one\n"))
    _send(group, port, _build_datagram(1, sender, 3, "p", "n", "three\n"))

    _wait_for_pattern(proc.stdout,
                      r"MISSING packet detected from p@n")
    _stop(proc)


# --------------------------------------------------------------------------- test runner

TEST_FUNCS = [
    test_basic,
    test_program_regex,
    test_node_regex,
    test_show_program_name,
    test_show_node_name,
    test_show_send_time,
    test_show_recv_time,
    test_csv,
    test_sequence_warning,
]


def _parse_args():
    p = argparse.ArgumentParser(description="Test suite for tracer_listener")
    p.add_argument("--listener", required=True,
                   help="Path to tracer_listener executable")
    return p.parse_args()


def main():
    args = _parse_args()

    failures: List[Tuple[str, str]] = []

    for func in TEST_FUNCS:
        name = func.__name__
        log(f"[ RUN      ] {name}")
        try:
            func(args.listener)
            log(f"[       OK ] {name}")
        except Exception as exc:
            failures.append((name, str(exc)))
            log(f"[  FAILED  ] {name}: {exc}")

    if failures:
        log("\n================ FAILURES ================")
        for name, reason in failures:
            log(f"{name}: {reason}")
        sys.exit(1)

    log("\nAll tests passed.")
    sys.exit(0)


if __name__ == "__main__":
    main()
