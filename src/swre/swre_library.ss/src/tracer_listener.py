#!/usr/bin/env python3
"""
Simple tracer listener.

Listens on UDP multicast group 224.11.11.12:49500, interprets the incoming
datagrams according to the TracerDataHeader definition in
include/Safir/SwReports/Internal/TracerData.h and prints the UTF-8 payload
to standard output.

Run:
TODO: translate this to c++
    python3 src/swre/swre_library.ss/src/tracer_listener.py
"""
import socket
import struct
import sys
import argparse
from typing import Tuple, Optional, Dict

MCAST_GRP: str = "224.11.11.12"
MCAST_PORT: int = 49500

# C++ `TracerDataHeader` (little-endian, packed on 4-byte boundary)
#   std::int64_t  incarnationId;
#   std::int64_t  nodeId;
#   std::uint32_t sequenceNumber;
#   std::uint32_t payloadLength;
_HEADER_FMT: str = "<qqII2BH"
_HEADER_SIZE: int = struct.calcsize(_HEADER_FMT)


def _create_socket(group: str, port: int, bind_addr: str) -> socket.socket:
    """
    Create and return a UDP socket joined to the given multicast group and bound
    to the supplied local interface address.
    """
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
    # Allow multiple listeners on the same machine
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

    # Bind to specific local interface
    sock.bind(("", port))

    mreq = struct.pack(
        "4s4s",
        socket.inet_aton(group),
        socket.inet_aton(bind_addr),
    )
    sock.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)
    return sock


def _parse_datagram(data: bytes) -> Tuple[bytes, int, int, int]:
    """
    Validate and split datagram into header fields and payload.
    Returns (payload, sequenceNumber, incarnationId, nodeId).
    Raises ValueError if the datagram is malformed.
    """
    if len(data) < _HEADER_SIZE:
        raise ValueError("Datagram shorter than header")

    (
        incarnation_id,
        node_id,
        seq_no,
        payload_len,
        prog_len,
        node_len,
        _padding,
    ) = struct.unpack_from(_HEADER_FMT, data)

    header_and_names_sz = _HEADER_SIZE + prog_len + node_len
    if len(data) - header_and_names_sz < payload_len:
        raise ValueError("Datagram payload length mismatch")

    payload = data[header_and_names_sz : header_and_names_sz + payload_len]
    return payload, seq_no, incarnation_id, node_id


def main() -> None:
    parser = argparse.ArgumentParser(description="Listen for tracer multicast data")
    parser.add_argument(
        "-g",
        "--group",
        default=MCAST_GRP,
        help=f"Multicast group address (default: {MCAST_GRP})",
    )
    parser.add_argument(
        "-p",
        "--port",
        type=int,
        default=MCAST_PORT,
        help=f"UDP port number (default: {MCAST_PORT})",
    )
    parser.add_argument(
        "-i",
        "--incarnation-id",
        type=int,
        help="Only show messages that match this incarnationId",
    )
    parser.add_argument(
        "-n",
        "--node-id",
        type=int,
        help="Only show messages that match this nodeId",
    )
    parser.add_argument(
        "--interface",
        default="127.0.0.1",
        metavar="ADDR",
        help="Interface address to join the multicast group on "
             "(default 127.0.0.1 – only local traffic). "
             "Use 0.0.0.0 to receive from all interfaces.",
    )
    args = parser.parse_args()

    sock = _create_socket(args.group, args.port, args.interface)
    print(
        f"Listening on {args.group}:{args.port} (interface {args.interface})",
        file=sys.stderr,
    )
    # nodeId -> last seen sequence number (nodeIds are globally unique)
    seq_tracker: Dict[int, int] = {}

    try:
        while True:
            data, addr = sock.recvfrom(65535)
            try:
                payload, seq_no, inc_id, nd_id = _parse_datagram(data)
                if (args.incarnation_id is not None and inc_id != args.incarnation_id):
                    continue
                if (args.node_id is not None and nd_id != args.node_id):
                    continue
            except ValueError as err:
                print(f"Malformed datagram from {addr}: {err}", file=sys.stderr)
                continue

            try:
                text = payload.decode("utf-8", errors="replace")
            except Exception:  # pragma: no cover
                text = "<binary payload>"

            # Gap / out-of-order detection on sequence numbers (per nodeId)
            key = nd_id
            last_seq = seq_tracker.get(key)
            if last_seq is not None:
                if seq_no == last_seq + 1:
                    pass  # in-order packet
                elif seq_no > last_seq + 1:
                    print(f"### MISSING packet detected from node {nd_id}. Expected {last_seq + 1}, got {seq_no} ###")
                else:  # seq_no < last_seq + 1  → out-of-order
                    print(f"### OUT-OF-ORDER packet from node {nd_id}. Got {seq_no} after {last_seq} ###")
                    text += "##### End out-of-order packet ###\n"
            # update last seen sequence number only if this is the highest so far
            if last_seq is None or seq_no > last_seq:
                seq_tracker[key] = seq_no

            # Write payload without adding an extra newline
            print(text, end="")
    except KeyboardInterrupt:
        print("\nInterrupted – exiting.", file=sys.stderr)


if __name__ == "__main__":
    main()
