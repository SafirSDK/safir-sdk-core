#!/usr/bin/env python3
"""Birthday-paradox UDP punch between an easy NAT and a hard NAT. Dependency-free.

The plain WireGuard punch needs both NATs to map endpoint-independently: each
side publishes the public port a STUN server saw, and the peer sends to it. The
runners GitHub has hosted on AWS since late 2026 sit behind a NAT gateway that
maps per destination, with a random public port each time ("hard"), so the port
they publish is not the one the peer's traffic arrives from, and the Azure side
("easy": endpoint-independent mapping, but it filters by address and port) drops
it. Nothing is ever received on either side.

This is the technique from Tailscale's "How NAT traversal works":
- The hard side opens many sockets (256), each sending to the easy side's
  published endpoint. Each one gets its own random public port on the hard NAT.
- The easy side sends probes from its published port to random ports on the
  hard side's public IP. Each probe also opens the easy side's own filter for
  that port.
- As soon as a probe lands on a port that one of the hard side's sockets is
  mapped to, packets get through both ways. With 256 sockets, 1024 probes give
  about a 98% chance, 2048 about 99.9%.

Neither side has to tell the other anything out of band. When a hard socket
first hears from the easy side it answers with CHOSEN from that socket, and keeps
doing so until the easy side ACKs. The easy side learns the hard side's public
port from where CHOSEN came from. The hard side's local port for that socket is
then what WireGuard has to listen on: the NAT keeps the mapping for that local
port and destination, so WireGuard's traffic reuses the public port that got
through. The easy side keeps its own port, which is endpoint-independent anyway.

Both sides must be ready to bring WireGuard up straight after this, while the
mappings are fresh (the AWS NAT gateway drops idle UDP mappings after 350 s).

Usage: punch.py easy <local-port> <hard-ip> <token> [timeout-seconds]
       punch.py hard <easy-ip> <easy-port> <token> [timeout-seconds]
On success the easy side prints "<hard-ip>:<hard-public-port>" and the hard side
prints "<local-port>", and both exit 0. On timeout both exit 1.
"""
import random
import select
import socket
import sys
import time

HARD_SOCKETS = 256
PROBES_PER_SECOND = 200
HELLO_INTERVAL = 0.5
CHOSEN_INTERVAL = 0.1
# Once a pair is found, how long to keep confirming it. The other side may still
# be retrying, and a lost ACK or CHOSEN would otherwise leave it waiting.
LINGER = 2.0


def log(msg):
    sys.stderr.write(f"punch: {msg}\n")
    sys.stderr.flush()


def udp_socket(port):
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.bind(("0.0.0.0", port))
    sock.setblocking(False)
    return sock


def send(sock, data, addr):
    try:
        sock.sendto(data, addr)
    except OSError:
        # A full buffer, or an ICMP error surfacing from an earlier probe to a
        # closed port. Neither matters: the next packet will try again.
        pass


def receive(sock):
    """Return (data, addr) for one waiting datagram, or None."""
    try:
        return sock.recvfrom(2048)
    except OSError:
        # Includes the ECONNREFUSED/WSAECONNRESET that some stacks report for an
        # earlier probe that hit a closed port. Not an error for us.
        return None


def easy(local_port, hard_ip, token, timeout):
    sock = udp_socket(local_port)
    probe, seen, ack = b"PROBE " + token, b"SEEN " + token, b"ACK " + token
    ports = list(range(1024, 65536))
    random.shuffle(ports)
    deadline = time.monotonic() + timeout
    start = time.monotonic()
    sent = 0
    found = None  # the hard side's public port, once CHOSEN arrived
    found_at = None
    heard = set()
    while time.monotonic() < deadline:
        now = time.monotonic()
        if found is not None and now - found_at > LINGER:
            log(f"done after {sent} probes in {found_at - start:.1f} s")
            print(f"{hard_ip}:{found}")
            return 0
        # Pace the probes, and stop probing once we have a pair.
        if found is None:
            due = int((now - start) * PROBES_PER_SECOND) + 1
            while sent < min(due, len(ports)):
                send(sock, probe, (hard_ip, ports[sent]))
                sent += 1
        readable, _, _ = select.select([sock], [], [], 0.01)
        if not readable:
            continue
        while True:
            got = receive(sock)
            if got is None:
                break
            data, (ip, port) = got
            if ip != hard_ip or not data.endswith(token):
                continue
            if port not in heard:
                heard.add(port)
                log(f"heard from {ip}:{port} after {sent} probes ({data.split()[0].decode()})")
            if data.startswith(b"CHOSEN "):
                if found is None:
                    found, found_at = port, time.monotonic()
                    log(f"hard side chose its public port {port}")
                if port == found:
                    send(sock, ack, (ip, port))
            elif found is None:
                # A HELLO (or a stray PROBE echo) got through our filter: tell that
                # socket we hear it, so the hard side chooses it.
                send(sock, seen, (ip, port))
    log(f"gave up after {timeout} s and {sent} probes; heard from {sorted(heard) or 'nobody'}")
    return 1


def hard(easy_ip, easy_port, token, timeout):
    socks = []
    try:
        for _ in range(HARD_SOCKETS):
            socks.append(udp_socket(0))
    except OSError as exc:
        log(f"could only open {len(socks)} sockets: {exc}")
        if not socks:
            return 1
    dest = (easy_ip, easy_port)
    hello, chosen = b"HELLO " + token, b"CHOSEN " + token
    deadline = time.monotonic() + timeout
    start = time.monotonic()
    next_hello = 0.0
    winner = None  # the socket that got through
    acked_at = None
    next_chosen = 0.0
    while time.monotonic() < deadline:
        now = time.monotonic()
        if acked_at is not None and now - acked_at > LINGER:
            port = winner.getsockname()[1]
            log(f"done in {acked_at - start:.1f} s, local port {port}")
            print(port)
            return 0
        if winner is None and now >= next_hello:
            # Keeps every socket's mapping alive, and is what gets through once
            # the easy side has probed the right port.
            for sock in socks:
                send(sock, hello, dest)
            next_hello = now + HELLO_INTERVAL
        if winner is not None and now >= next_chosen:
            send(winner, chosen, dest)
            next_chosen = now + CHOSEN_INTERVAL
        readable, _, _ = select.select(socks if winner is None else [winner], [], [], 0.01)
        for sock in readable:
            while True:
                got = receive(sock)
                if got is None:
                    break
                data, addr = got
                if addr != dest or not data.endswith(token):
                    continue
                if winner is None:
                    winner = sock
                    log(f"local port {sock.getsockname()[1]} heard from the easy side "
                        f"({data.split()[0].decode()}) after {now - start:.1f} s")
                    send(winner, chosen, dest)
                    next_chosen = now + CHOSEN_INTERVAL
                elif sock is winner and data.startswith(b"ACK ") and acked_at is None:
                    acked_at = time.monotonic()
                    log("easy side acknowledged")
    if winner is not None:
        log("found a port, but the easy side never acknowledged it")
    else:
        log(f"gave up after {timeout} s; the easy side never got through")
    return 1


def main(argv):
    if len(argv) >= 4 and argv[0] == "easy":
        timeout = float(argv[4]) if len(argv) > 4 else 90
        return easy(int(argv[1]), argv[2], argv[3].encode(), timeout)
    if len(argv) >= 4 and argv[0] == "hard":
        timeout = float(argv[4]) if len(argv) > 4 else 90
        return hard(argv[1], int(argv[2]), argv[3].encode(), timeout)
    sys.stderr.write(__doc__)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
