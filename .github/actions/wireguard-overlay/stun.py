#!/usr/bin/env python3
"""Dependency-free STUN (RFC 5389) client, bound to a fixed UDP source port.

Prints the public "IP:port" the outside world maps that source port to. We bind
to the SAME port WireGuard will listen on so the NAT mapping we learn is the one
the peer will actually see. On a cone NAT WireGuard reuses this mapping (the
punch works); on a symmetric NAT the mapping differs per destination and the
punch fails - in practice GitHub-hosted (Azure) runners punch cleanly.

With no explicit host given we try a list of public STUN servers in turn, so a
single server being down doesn't sink the whole rendezvous. Passing a host (and
optionally port) overrides the list and queries only that server.

Usage: stun.py <local-udp-port> [stun-host] [stun-port]
"""
import socket
import struct
import sys
import secrets

PORT = int(sys.argv[1])
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


def query(host, port):
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(("0.0.0.0", PORT))
    s.settimeout(3)
    try:
        req = struct.pack(">HHI", 0x0001, 0, MAGIC) + secrets.token_bytes(12)
        dst = (socket.gethostbyname(host), port)
        data = None
        for _ in range(3):
            s.sendto(req, dst)
            try:
                data, _ = s.recvfrom(2048)
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
    finally:
        s.close()


mapped = None
for host, port in SERVERS:
    try:
        mapped = query(host, port)
    except (socket.gaierror, OSError) as e:
        sys.stderr.write(f"STUN {host}:{port} failed: {e}\n")
        continue
    if mapped:
        sys.stderr.write(f"STUN mapping from {host}:{port}\n")
        break
    sys.stderr.write(f"STUN {host}:{port} gave no mapped address\n")

if not mapped:
    sys.exit("no STUN server returned a mapped address")
print(mapped)
