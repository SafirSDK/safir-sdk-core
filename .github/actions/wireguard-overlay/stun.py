#!/usr/bin/env python3
"""Dependency-free STUN (RFC 5389) client, bound to a fixed UDP source port.

Prints "<public-ip>:<public-port> <local-port>": the endpoint the outside world
maps our source port to, plus the local port we actually bound. We bind to the
SAME port WireGuard will listen on so the NAT mapping we learn is the one the
peer will actually see. On a cone NAT WireGuard reuses this mapping (the punch
works); on a symmetric NAT the mapping differs per destination and the punch
fails - in practice GitHub-hosted (Azure) runners punch cleanly.

Several candidate local ports may be given, tried in order. This matters on
Windows runners: a port inside the dynamic range (49152-65535 by default) can be
reserved by WinNAT/Hyper-V, and binding it then fails with WSAEACCES
("[WinError 10013] ... forbidden by its access permissions") no matter which
STUN server we ask - the bind fails before a packet is ever sent. Which ranges
are reserved depends on runner state, so the same port works most days and
fails occasionally; falling back to another port turns that into a non-event.
Prefer candidates BELOW 49152, which cannot be caught by that reservation.

With no explicit host given we try a list of public STUN servers in turn, so a
single server being down doesn't sink the whole rendezvous. Passing a host (and
optionally port) overrides the list and queries only that server.

Usage: stun.py <local-udp-port>[,<port>...] [stun-host] [stun-port]
"""
import socket
import struct
import sys
import secrets

PORTS = [int(p) for p in sys.argv[1].split(",") if p.strip()]
MAGIC = 0x2112A442

# Queried in order until one answers. Overridden by an explicit host arg.
DEFAULT_SERVERS = [
    ("stun.l.google.com", 19302),
    ("stun1.l.google.com", 19302),
    ("stun.cloudflare.com", 3478),
    ("stun.nextcloud.com", 3478),
]
if len(sys.argv) > 2:
    SERVERS = [(sys.argv[2], int(sys.argv[3]) if len(sys.argv) > 3 else 3478)]
else:
    SERVERS = DEFAULT_SERVERS


def query(sock, host, port):
    """Ask one STUN server for our mapped address, over an already-bound socket."""
    req = struct.pack(">HHI", 0x0001, 0, MAGIC) + secrets.token_bytes(12)
    dst = (socket.gethostbyname(host), port)
    data = None
    for _ in range(3):
        sock.sendto(req, dst)
        try:
            data, _ = sock.recvfrom(2048)
            break
        except socket.timeout:
            continue
    if not data:
        return None

    i, mapped = 20, None
    while i + 4 <= len(data):
        atype, alen = struct.unpack(">HH", data[i:i + 4])
        val = data[i + 4:i + 4 + alen]
        if atype == 0x0020 and len(val) >= 8:        # XOR-MAPPED-ADDRESS
            port_ = struct.unpack(">H", val[2:4])[0] ^ (MAGIC >> 16)
            ip = socket.inet_ntoa(struct.pack(">I", struct.unpack(">I", val[4:8])[0] ^ MAGIC))
            return f"{ip}:{port_}"
        if atype == 0x0001 and len(val) >= 8 and mapped is None:  # MAPPED-ADDRESS
            mapped = f"{socket.inet_ntoa(val[4:8])}:{struct.unpack('>H', val[2:4])[0]}"
        i += 4 + alen + ((4 - alen % 4) % 4)
    return mapped


def bind(port):
    """Bind UDP <port>, or return None if the OS won't let us have it."""
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind(("0.0.0.0", port))
        sock.settimeout(3)
        return sock
    except OSError as exc:
        # WSAEACCES (10013) / EACCES / EADDRINUSE all mean "pick another port".
        sys.stderr.write(f"cannot bind UDP port {port}: {exc}\n")
        sock.close()
        return None


def discover(port):
    """Return the public endpoint for <port>, or None if this port is unusable."""
    sock = bind(port)
    if sock is None:
        return None
    try:
        for host, sport in SERVERS:
            try:
                mapped = query(sock, host, sport)
            except (socket.gaierror, OSError) as exc:
                sys.stderr.write(f"STUN {host}:{sport} failed: {exc}\n")
                continue
            if mapped:
                sys.stderr.write(f"STUN mapping from {host}:{sport} on local port {port}\n")
                return mapped
            sys.stderr.write(f"STUN {host}:{sport} gave no mapped address\n")
        return None
    finally:
        sock.close()


for candidate in PORTS:
    result = discover(candidate)
    if result:
        print(f"{result} {candidate}")
        sys.exit(0)
    sys.stderr.write(f"local port {candidate} yielded no mapping, trying the next one\n")

sys.exit(f"no STUN server returned a mapped address on any of {PORTS}")
