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
"""Unit tests for build/ci/udp_mesh_probe.py.

Lives next to the other CI-tooling tests in build/ (not in build/ci/) because
the warnings-summary job runs `unittest discover -s build`, which does not
descend into directories that are not packages.

The nodes are 127.0.0.1, .2 and .3, which only Linux lets us bind without
configuring anything, so the tests are skipped elsewhere.
"""
# pylint: disable=missing-class-docstring,missing-function-docstring
import io
import os
import socket
import sys
import threading
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "ci"))
import udp_mesh_probe  # pylint: disable=wrong-import-position

NODES = ["127.0.0.1", "127.0.0.2", "127.0.0.3"]


def free_ports(count):
    """Ports that are free on 127.0.0.1 right now, and so most likely on the other nodes too."""
    sockets = []
    try:
        for _ in range(count):
            sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            sock.bind(("127.0.0.1", 0))
            sockets.append(sock)
        return [sock.getsockname()[1] for sock in sockets]
    finally:
        for sock in sockets:
            sock.close()


def run_nodes(nodes, running, **kwargs):
    """Run a probe for each of `running` in a thread. Returns {node: (exit code, output)}."""
    results = {}

    def run(node):
        out = io.StringIO()
        code = udp_mesh_probe.Probe(node, nodes, out=out, interval=0.05, **kwargs).run()
        results[node] = (code, out.getvalue())

    threads = [threading.Thread(target=run, args=(node, )) for node in running]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join(60)
    return results


@unittest.skipUnless(sys.platform.startswith("linux"), "needs the whole of 127.0.0.0/8 on loopback")
class MeshProbe(unittest.TestCase):

    def test_all_nodes_up_passes_every_leg(self):
        results = run_nodes(NODES, NODES, ports=free_ports(2), sizes=(64, 1392), timeout=20, grace=5)
        self.assertEqual(sorted(results), NODES)
        for node, (code, output) in results.items():
            self.assertEqual(code, 0, output)
            # 2 peers x 2 ports x 2 sizes
            self.assertEqual(output.count("PASS"), 8, output)
            self.assertEqual(output.count("PASS  lost 0 of 10 (0%)"), 8, output)
            self.assertNotIn("FAIL", output, f"{node}: {output}")
            self.assertNotIn("NOTE", output, f"{node}: {output}")

    def test_missing_node_fails_and_is_named(self):
        results = run_nodes(NODES, NODES[:2], ports=free_ports(2), sizes=(64, ), timeout=1, grace=1)
        for node, (code, output) in results.items():
            other = NODES[1] if node == NODES[0] else NODES[0]
            self.assertEqual(code, 1, output)
            self.assertIn("Nothing at all was received from 127.0.0.3", output)
            self.assertNotIn(f"Nothing at all was received from {other}", output)
            # The node that is up passes its legs; the missing one fails both ports.
            self.assertEqual(output.count("PASS"), 2, output)
            self.assertEqual(output.count("FAIL  sent"), 2, output)
            self.assertIn("0 of the 10 PONGs needed, 0 PINGs from peer", output)

    def test_finished_node_keeps_answering_a_late_starter(self):
        ports = free_ports(1)
        results = {}

        def early():
            results.update(run_nodes(NODES[:2], NODES[:1], ports=ports, sizes=(64, ), timeout=20, grace=5))

        thread = threading.Thread(target=early)
        thread.start()
        threading.Event().wait(1.0)
        results.update(run_nodes(NODES[:2], NODES[1:2], ports=ports, sizes=(64, ), timeout=20, grace=5))
        thread.join(60)
        for code, output in results.values():
            self.assertEqual(code, 0, output)

    def test_port_in_use_is_reported(self):
        port = free_ports(1)[0]
        with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as holder:
            holder.bind(("127.0.0.1", port))
            out = io.StringIO()
            code = udp_mesh_probe.Probe("127.0.0.1", NODES, ports=[port], out=out, timeout=1).run()
        self.assertEqual(code, 1)
        self.assertIn("could not bind 127.0.0.1", out.getvalue())

    def test_light_nodes_are_not_probed_against_each_other(self):
        # .2 and .3 are light and nothing is running on .3, so .2 can only pass
        # if it does not wait for the other light node.
        nodes = NODES[:2]
        results = run_nodes(NODES, nodes, ports=free_ports(1), sizes=(64, ), light_nodes=NODES[1:], timeout=2, grace=1)
        code, output = results[NODES[1]]
        self.assertEqual(code, 0, output)
        self.assertEqual(output.count("PASS"), 1, output)
        self.assertNotIn("127.0.0.3 port", output)
        # The normal node still needs every node, light or not.
        code, output = results[NODES[0]]
        self.assertEqual(code, 1, output)
        self.assertIn("Nothing at all was received from 127.0.0.3", output)

    def test_light_nodes_must_be_among_the_nodes(self):
        with self.assertRaises(ValueError):
            udp_mesh_probe.Probe("127.0.0.1", NODES, light_nodes=["127.0.0.9"])

    def test_count_is_configurable(self):
        results = run_nodes(NODES[:2], NODES[:2], ports=free_ports(1), sizes=(64, ), count=3, timeout=20, grace=5)
        for code, output in results.values():
            self.assertEqual(code, 0, output)
            self.assertIn("PASS  lost 0 of 3 (0%)", output)

    def test_me_must_be_one_of_the_nodes(self):
        with self.assertRaises(ValueError):
            udp_mesh_probe.Probe("127.0.0.9", NODES)


class LegLoss(unittest.TestCase):

    @staticmethod
    def leg_with_answers(count, sent, answered):
        leg = udp_mesh_probe.Leg(64, count)
        for _ in range(sent):
            leg.next_ping()
        for sequence in answered:
            leg.pong(sequence)
        return leg

    def test_pings_before_the_first_answer_are_not_loss(self):
        leg = self.leg_with_answers(3, 20, [15, 16, 17])
        self.assertTrue(leg.answered_enough())
        self.assertEqual((leg.lost(), leg.window()), (0, 3))

    def test_gaps_after_the_first_answer_are_loss(self):
        leg = self.leg_with_answers(3, 20, [5, 8, 9])
        self.assertEqual((leg.lost(), leg.window()), (2, 5))

    def test_duplicates_unknown_and_surplus_pongs_do_not_count(self):
        leg = self.leg_with_answers(3, 10, [2, 2, 99, 3])
        self.assertFalse(leg.answered_enough())
        leg.pong(4)
        leg.pong(7)
        self.assertEqual(sorted(leg.answered), [2, 3, 4])
        self.assertEqual((leg.lost(), leg.window()), (0, 3))

    def test_passing_needs_a_ping_from_the_peer_too(self):
        leg = self.leg_with_answers(2, 5, [1, 2])
        self.assertFalse(leg.passed())
        leg.pings_seen = 1
        self.assertTrue(leg.passed())


if __name__ == "__main__":
    unittest.main()
