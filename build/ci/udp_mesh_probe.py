#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2026 (http://safirsdkcore.com)
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
"""Check that every multicomputer test node can exchange UDP with every other.

The multicomputer dose tests put one node on one runner and three in containers
on another, joined by a WireGuard tunnel and a docker bridge. When cross-node
traffic does not get through, nothing in the product says so: the test sequencer
waits for its partners until the job times out (#619). This is run on all four
nodes before any of them starts the Dob, so that a hang later can be read
against a known answer to "could the nodes talk at all?".

It tries to look like the traffic it stands in for. Every node binds the Dob's
own control and data ports on the address the Dob will bind, and sends from
those ports, so the packets have the same addresses and ports as the real ones.
Each datagram is sent at the size given, with the don't-fragment bit set where
the platform lets us; pass the configured FragmentSize, which is the largest
datagram the Dob sends.

Run the same command on every node, with the same --nodes list and that node's
own address as --me:

    udp_mesh_probe.py --me 10.68.1.1 --nodes 10.68.1.1 10.68.0.2 10.68.0.3 10.68.0.4 \
                      --light-nodes 10.68.0.3 10.68.0.4

Light nodes talk to the normal nodes but never to each other, so no leg is
probed between two nodes that are both listed in --light-nodes.

A leg is one peer, one port and one size. A PING is sent on every leg twice a
second, and the peer echoes each one back as a PONG. The leg passes when --count
of this node's PINGs have come back intact and at least one PING from the peer
has arrived, so each leg covers both directions. The nodes start at different
times, so each one keeps trying until every leg has passed or --timeout runs
out, and a node that is finished keeps answering until its peers are finished
too.

For a leg that passed, the loss figure is counted from the first PING that was
answered to the last one needed: everything before the first answer is the peer
not being up yet, which is not loss.

Exits 0 when every leg passed and 1 otherwise, after printing one line per leg.
"""
import argparse
import os
import select
import socket
import struct
import sys
import time
import zlib

MAGIC = b"SFRPROBE"
PING = 1
PONG = 2

# magic, kind, sender-is-finished flag, sender address, datagram size, sequence
# number of the PING (a PONG carries that of the PING it answers), crc32 of the
# padding that follows the header.
HEADER = struct.Struct("!8sBB4sHII")

# Ports and FragmentSize of the multicomputer test configuration, see
# src/tests/dose_test.ss/test_config/multicomputer/parameters.
DEFAULT_PORTS = (30000, 40000)
DEFAULT_SIZES = (64, 1392)
DEFAULT_COUNT = 10


def set_dont_fragment(sock):
    """Set the don't-fragment bit on what sock sends. Returns False if we can't."""
    try:
        if sys.platform.startswith("linux"):
            # IP_MTU_DISCOVER = IP_PMTUDISC_DO. Not every python exposes the names.
            sock.setsockopt(socket.IPPROTO_IP, getattr(socket, "IP_MTU_DISCOVER", 10),
                            getattr(socket, "IP_PMTUDISC_DO", 2))
            return True
        if sys.platform == "win32":
            sock.setsockopt(socket.IPPROTO_IP, getattr(socket, "IP_DONTFRAGMENT", 14), 1)
            return True
    except OSError:
        pass
    return False


class Leg:
    """What we know about one peer, port and size."""

    def __init__(self, size, count):
        self.padding = os.urandom(size - HEADER.size)
        self.count = count
        self.sent = 0
        self.sent_at = {}
        self.send_error = None
        self.pings_seen = 0
        self.answered = {}

    def next_ping(self):
        """Account for another PING being sent. Returns its sequence number."""
        self.sent += 1
        if not self.answered_enough():
            self.sent_at[self.sent] = time.monotonic()
        return self.sent

    def pong(self, sequence):
        """Account for the PONG answering PING number `sequence`."""
        sent_at = self.sent_at.get(sequence)
        if sent_at is not None and sequence not in self.answered and not self.answered_enough():
            self.answered[sequence] = time.monotonic() - sent_at

    def answered_enough(self):
        """Have as many of our PINGs come back as we asked for?"""
        return len(self.answered) >= self.count

    def passed(self):
        """Both directions seen: the peer's PINGs arrive and enough of ours were echoed back."""
        return self.pings_seen > 0 and self.answered_enough()

    def window(self):
        """Number of PINGs sent from the first one answered to the last one answered."""
        return max(self.answered) - min(self.answered) + 1 if self.answered else 0

    def lost(self):
        """Number of PINGs in window() that were not answered."""
        return self.window() - len(self.answered)


class Probe:
    """One node's side of the mesh probe."""

    # pylint: disable=too-many-instance-attributes,too-many-arguments,too-many-positional-arguments
    # pylint: disable=too-few-public-methods

    def __init__(self,
                 me,
                 nodes,
                 ports=DEFAULT_PORTS,
                 sizes=DEFAULT_SIZES,
                 light_nodes=(),
                 count=DEFAULT_COUNT,
                 timeout=600.0,
                 interval=0.5,
                 grace=30.0,
                 dont_fragment=True,
                 out=sys.stdout):
        if me not in nodes:
            raise ValueError(f"--me {me} is not in the list of nodes")
        if min(sizes) < HEADER.size:
            raise ValueError(f"The smallest possible datagram size is {HEADER.size}")
        self.me = me
        if not set(light_nodes).issubset(nodes):
            raise ValueError("--light-nodes has an address that is not in the list of nodes")
        # Two light nodes are not expected to be able to reach each other.
        self.peers = [node for node in nodes if node != me and not (me in light_nodes and node in light_nodes)]
        self.ports = tuple(ports)
        self.sizes = tuple(sorted(sizes))
        self.timeout = timeout
        self.interval = interval
        self.grace = grace
        self.dont_fragment = dont_fragment
        self.out = out
        self.count = count
        self.legs = {
            (peer, port, size): Leg(size, count)
            for peer in self.peers
            for port in self.ports
            for size in self.sizes
        }
        self.peers_finished = set()
        self.oddities = {}
        self.sockets = {}

    def _log(self, text):
        print(f"[{self.me}] {text}", file=self.out, flush=True)

    def _oddity(self, text):
        """Remember something unexpected, once, however often it happens."""
        self.oddities[text] = self.oddities.get(text, 0) + 1

    def _open(self):
        for port in self.ports:
            sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            try:
                sock.bind((self.me, port))
            except OSError:
                sock.close()
                raise
            sock.setblocking(False)
            if self.dont_fragment and not set_dont_fragment(sock):
                self._oddity("could not set the don't-fragment bit on this platform")
            self.sockets[sock] = port

    def _close(self):
        for sock in self.sockets:
            sock.close()
        self.sockets = {}

    def _datagram(self, kind, sequence, padding, finished):
        header = HEADER.pack(MAGIC, kind, 1 if finished else 0, socket.inet_aton(self.me), HEADER.size + len(padding),
                             sequence, zlib.crc32(padding))
        return header + padding

    def _send_pings(self, finished):
        for sock, port in self.sockets.items():
            for peer in self.peers:
                for size in self.sizes:
                    leg = self.legs[(peer, port, size)]
                    if finished:
                        # All that is left to say is that we are finished, and
                        # the smallest datagram says it as well as any.
                        if size != self.sizes[0]:
                            continue
                    elif leg.answered_enough():
                        continue
                    sequence = leg.next_ping()
                    try:
                        sock.sendto(self._datagram(PING, sequence, leg.padding, finished), (peer, port))
                    except OSError as exc:
                        # With don't-fragment set, a size that does not fit the
                        # local route's MTU fails right here ("Message too long").
                        leg.send_error = str(exc)

    def _parse(self, data, port, source, source_port):
        """Check a received datagram. Returns (kind, leg, sequence, padding), or None if it is no use."""
        if len(data) < HEADER.size:
            self._oddity(f"{len(data)} byte datagram from {source}:{source_port} on port {port}, not ours")
            return None
        magic, kind, peer_finished, sender, size, sequence, crc = HEADER.unpack_from(data)
        if magic != MAGIC:
            self._oddity(f"unrecognised datagram from {source}:{source_port} on port {port}")
            return None
        sender = socket.inet_ntoa(sender)
        padding = data[HEADER.size:]
        leg = self.legs.get((sender, port, size))
        if leg is None:
            self._oddity(f"datagram from {sender}, size {size}, which is not a leg we were told about")
            return None
        if len(data) != size or zlib.crc32(padding) != crc:
            self._oddity(f"damaged datagram from {sender} on port {port}: header says {size} bytes, got {len(data)}")
            return None
        if source != sender:
            # The peer's address was rewritten on the way. The Dob identifies
            # nodes by address, so this breaks it even though we can answer.
            self._oddity(f"datagram from {sender} arrived with source address {source} (NAT on the path?)")
        if source_port != port:
            self._oddity(f"datagram from {sender} to port {port} arrived from source port {source_port}")
        if peer_finished:
            self.peers_finished.add(sender)
        return kind, leg, sequence, padding

    def _receive(self, sock, finished):
        port = self.sockets[sock]
        try:
            data, (source, source_port) = sock.recvfrom(65535)
        except (BlockingIOError, InterruptedError):
            return
        except OSError:
            # Windows reports an ICMP port-unreachable for something we sent
            # earlier as an error on the next receive. A peer that is not up
            # yet is expected, so there is nothing to do about it.
            return

        parsed = self._parse(data, port, source, source_port)
        if parsed is None:
            return
        kind, leg, sequence, padding = parsed

        if kind == PING:
            leg.pings_seen += 1
            try:
                sock.sendto(self._datagram(PONG, sequence, padding, finished), (source, source_port))
            except OSError as exc:
                leg.send_error = str(exc)
        elif kind == PONG:
            if padding != leg.padding:
                self._oddity(f"PONG from {source} on port {port} does not match the PING we sent")
            else:
                leg.pong(sequence)

    def _report(self):
        for (peer, port, size), leg in sorted(self.legs.items()):
            if leg.passed():
                times = [seconds * 1000 for seconds in leg.answered.values()]
                verdict = (f"PASS  lost {leg.lost()} of {leg.window()} ({100 * leg.lost() / leg.window():.0f}%), "
                           f"first answer to PING #{min(leg.answered)}, "
                           f"round trip {min(times):.0f}-{max(times):.0f} ms")
            else:
                verdict = (f"FAIL  sent {leg.sent}, {len(leg.answered)} of the {leg.count} PONGs needed, "
                           f"{leg.pings_seen} PINGs from peer")
                if leg.answered:
                    verdict += f", lost {leg.lost()} of {leg.window()} since the first answer"
                if leg.send_error:
                    verdict += f", last send error: {leg.send_error}"
            self._log(f"{peer:>15} port {port:<5} size {size:<5} {verdict}")
        for text, count in sorted(self.oddities.items()):
            self._log(f"NOTE ({count}x): {text}")

    def _summarise_failure(self):
        """Say which of the usual shapes the failure has, to save reading the table."""
        failed = [key for key, leg in self.legs.items() if not leg.passed()]
        silent = [
            peer for peer in self.peers
            if not any(leg.pings_seen or leg.answered for (p, _, _), leg in self.legs.items() if p == peer)
        ]
        for peer in silent:
            self._log(f"Nothing at all was received from {peer}: it is unreachable, or its probe never ran.")
        rest = [key for key in failed if key[0] not in silent]
        if rest and all(size == self.sizes[-1] for (_, _, size) in rest) and len(self.sizes) > 1:
            self._log(f"Only {self.sizes[-1]} byte datagrams fail where smaller ones pass: this is an MTU problem.")
        failed_ports = {port for (_, port, _) in rest}
        if rest and len(failed_ports) < len(self.ports):
            self._log(f"Only port(s) {sorted(failed_ports)} fail: something on the path treats the ports differently.")

    def run(self):
        """Run the probe to completion. Returns the process exit code."""
        self._log(f"UDP mesh probe: peers {self.peers}, ports {list(self.ports)}, sizes {list(self.sizes)}, "
                  f"{self.count} round trips per leg, timeout {self.timeout:.0f} s")
        try:
            self._open()
        except OSError as exc:
            self._log(f"FAILED: could not bind {self.me}: {exc}")
            return 1
        try:
            return self._loop()
        finally:
            self._close()

    def _loop(self):
        start = time.monotonic()
        finished_at = None
        next_send = start
        while True:
            now = time.monotonic()
            if finished_at is None and all(leg.passed() for leg in self.legs.values()):
                finished_at = now
                self._log(f"All {len(self.legs)} legs passed after {now - start:.1f} s.")
                self._report()
            if finished_at is None and now - start > self.timeout:
                self._log(f"FAILED: not every node could be reached within {self.timeout:.0f} s.")
                self._report()
                self._summarise_failure()
                return 1
            if finished_at is not None:
                # Stay around to answer the peers that are not finished yet,
                # but not for ever: one that never finishes fails on its own.
                everyone = self.peers_finished.issuperset(self.peers)
                if (everyone and now - finished_at > 2 * self.interval) or now - finished_at > self.grace:
                    return 0
            if now >= next_send:
                self._send_pings(finished_at is not None)
                next_send = now + self.interval
            readable, _, _ = select.select(list(self.sockets), [], [], max(0.0, next_send - time.monotonic()))
            for sock in readable:
                self._receive(sock, finished_at is not None)


def main(argv=None):
    """Command line entry point. Returns the process exit code."""
    parser = argparse.ArgumentParser(description=__doc__.split("\n", maxsplit=1)[0])
    parser.add_argument("--me", required=True, help="This node's address, which is also the address to bind")
    parser.add_argument("--nodes", required=True, nargs="+", help="Addresses of all nodes, this one included")
    parser.add_argument("--light-nodes",
                        nargs="*",
                        default=(),
                        help="The nodes among --nodes that are light nodes, which are not probed against each other")
    parser.add_argument("--ports", type=int, nargs="+", default=DEFAULT_PORTS, help="UDP ports to bind and probe")
    parser.add_argument("--sizes", type=int, nargs="+", default=DEFAULT_SIZES, help="UDP payload sizes to send")
    parser.add_argument("--count", type=int, default=DEFAULT_COUNT, help="Number of answered PINGs a leg needs to pass")
    parser.add_argument("--timeout", type=float, default=600.0, help="Seconds to keep trying before giving up")
    parser.add_argument("--allow-fragmentation",
                        action="store_true",
                        help="Do not set the don't-fragment bit on what is sent")
    args = parser.parse_args(argv)
    probe = Probe(args.me,
                  args.nodes,
                  ports=args.ports,
                  sizes=args.sizes,
                  light_nodes=args.light_nodes,
                  count=args.count,
                  timeout=args.timeout,
                  dont_fragment=not args.allow_fragmentation)
    return probe.run()


if __name__ == "__main__":
    sys.exit(main())
