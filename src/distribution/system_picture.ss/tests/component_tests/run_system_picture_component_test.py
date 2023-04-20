#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013-2014,2022-2023 (http://safirsdkcore.com)
#
# Created by: Lars Hagstrom (lars.hagstrom@consoden.se)
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
import datetime
import sys
import argparse
import subprocess
import signal
import time
import os
import shutil
import socket
from contextlib import closing
from queue import Queue
from threading import Thread
import json
import logging
import uuid
import itertools
FORMATTER = logging.Formatter('%(asctime)s %(levelname)s %(message)s')

class Failure(Exception):
    pass

#pylint: disable=line-too-long

def permutations(states):
    result = [[]]
    if isinstance(states, str):
        states = [states,]

    for level_one in states:
        if isinstance(level_one, str):
            for res in result:
                res.append(level_one)
        else:
            bases = result
            result = []
            for level_two, base in itertools.product(level_one, bases):
                if len(base) == 0:
                    result.append([level_two,])
                elif level_two is None:
                    result.append(base)
                else:
                    result.append(base + [level_two,])
    return result

def test_permutations():
    #Test the permutations function to make sure we can get proper states
    assert permutations("A") == [['A']]
    assert permutations(("A","B")) == [['A', 'B']]
    assert permutations((("A","B"),"C")) == [['A', 'C'], ['B', 'C']]
    assert permutations(("A",("B","C"))) == [['A', 'B'], ['A', 'C']]
    assert permutations((("A","B"),("C","D"))) == [['A', 'C'], ['B', 'C'], ['A', 'D'], ['B', 'D']]
    assert permutations(("A",(None, "B","C"), "D")) == [['A', 'D'], ['A', 'B', 'D'], ['A', 'C', 'D']]

def enqueue_output(out, logger, ctrl_queue, main_queue):
    while True:
        line = out.readline()
        if not line:
            break
        line = line.rstrip("\n\r")
        if ctrl_queue is not None and line.startswith("ctrl: "):
            data = json.loads(line[6:])
            data.pop("election_id", None)
            ctrl_queue.put(data)
        if main_queue is not None and line.startswith("main: "):
            data = json.loads(line[6:])
            data.pop("election_id", None)
            main_queue.put(data)
        logger.info(line)
    if ctrl_queue is not None:
        ctrl_queue.put(None)
    if main_queue is not None:
        main_queue.put(None)
    out.close()


def log(*args, **kwargs):
    print(datetime.datetime.now().isoformat(), ":", *args, **kwargs)
    sys.stdout.flush()


def rmdir(directory):
    if os.path.exists(directory):
        try:
            shutil.rmtree(directory)
        except OSError:
            log("Failed to remove directory, will retry")
            time.sleep(0.2)
            shutil.rmtree(directory)


def mkdir(newdir):
    """works the way a good mkdir should :)
        - already exists, silently complete
        - regular file in the way, raise an exception
        - parent directory(ies) does not exist, make them as well
    """
    if os.path.isdir(newdir):
        pass
    elif os.path.isfile(newdir):
        raise OSError("a file with the same name as the desired " \
                      "dir, '%s', already exists." % newdir)
    else:
        head, tail = os.path.split(newdir)
        if head and not os.path.isdir(head):
            mkdir(head)
        if tail:
            os.mkdir(newdir)

class Node():

    def __init__(self, args, node_id, light_node, multicast, seeds=None):
        self.args = args
        self.node_id = node_id
        self.light_node = light_node
        self.multicast = multicast
        self.returncode = None
        self.seeds = seeds
        self.ignore_returncode = False  #can be used to ignore expected bad returncodes
        self.session_id = str(uuid.uuid4())
        self.actual_ctrl = None
        self.actual_main = None
        command = [args.exe, "--id", str(node_id)]
        if self.multicast:
            command.append("--use-multicast")
        if self.light_node:
            command.append("--light")
        if self.seeds is not None:
            command.append("--seeds")
            if isinstance(self.seeds, int):
                command.append(str(self.seeds))
            else:
                command += map(str, self.seeds)
        if self.args.only_control:
            command.append("--only-control")
        #if args.address is not None:
        #    command += ("--address", address)
        #if seed is not None:
        #    command += ("--seed", seed)
        env = os.environ.copy()

        env["SAFIR_COM_NETWORK_SIMULATION"] = self.session_id # Enable network simulation
        env["SAFIR_INSTANCE"] = str(self.node_id + 1000)

        self.logging_handler = logging.FileHandler("node_{0:03d}.output.txt".format(node_id))
        self.logging_handler.setFormatter(FORMATTER)

        self.logger = logging.getLogger("node_{0:03d}".format(node_id))
        self.logger.setLevel(logging.INFO)
        self.logger.addHandler(self.logging_handler)

        self.ctrl_states = []
        self.main_states = []
        log(f"  Launching {' '.join(command)}")
        self.proc = subprocess.Popen(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=env,
            preexec_fn=None if sys.platform == "win32" else os.setsid,
            creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if sys.platform == "win32" else 0,
            universal_newlines=True)
        self.main_queue = Queue()
        self.ctrl_queue = Queue()
        self.stdout_thread = Thread(target=enqueue_output,
                                    args=(self.proc.stdout, self.logger, self.ctrl_queue, self.main_queue))
        self.stderr_thread = Thread(target=enqueue_output, args=(self.proc.stderr, self.logger, None, None))
        self.stdout_thread.start()
        self.stderr_thread.start()

    def disconnect(self):
        log(f"Disconnecting node {self.node_id}")
        self.__set_network_state(False)

    def reconnect(self):
        log(f"Reconnecting node {self.node_id}")
        self.__set_network_state(True)

    def __set_network_state(self, state):
        cmd = ("up " if state else "down ") + self.session_id
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        #Send three times to make it super likely that it will get there.
        sock.sendto(bytes(cmd, "utf-8"), ("239.6.6.6", 16666))
        time.sleep(0.01)
        sock.sendto(bytes(cmd, "utf-8"), ("239.6.6.6", 16666))
        time.sleep(0.01)
        sock.sendto(bytes(cmd, "utf-8"), ("239.6.6.6", 16666))

    @staticmethod
    def __compare_states(state, expected):
        return state == expected

    def wait_for_form(self, allowed_states=[]):
        log(f"  Node {self.node_id} waiting for form")
        allowed_states = [json.loads(x) for x in allowed_states]
        try:
            while True:
                data = self.ctrl_queue.get()
                if data not in allowed_states:
                    break
            if data["form"] == 0:
                raise Failure("Unexpected 0 incarnation")
            return data["form"]
        except KeyError:
            log(f"Node {self.node_id} expected a form statement, but got \n{data}")
        raise Failure("Got unexpected state")

    def wait_for_join(self, incarnation_id, allow_detached_states=False):
        log(f"  Node {self.node_id} waiting for join")
        try:
            while True:
                data = self.ctrl_queue.get()
                if allow_detached_states and "is_detached" in data and data["is_detached"] == True:
                    self.ctrl_states.append(data)
                else:
                    break

            if data["join"] == 0:
                raise Failure("Unexpected 0 incarnation")
            if incarnation_id is None or data["join"] == incarnation_id:
                return data["join"]
            log(f"Node {self.node_id} expected a join to incarnation {incarnation_id}, but got \n{data}")
            raise Failure("Got unexpected incarnation_id")
        except KeyError:
            log(f"Node {self.node_id} expected a join statement, but got \n{data}")
        raise Failure("Got unexpected state")

    def wait_for_join_or_form(self, allowed_states=[]):
        allowed_states = [json.loads(x) for x in allowed_states]
        while True:
            data = self.ctrl_queue.get()
            if data in allowed_states:
                self.ctrl_states.append(data)
            else:
                break
        if "form" in data or "join" in data:
            return data
        else:
            log(f"Node {self.node_id} expected a form or join statement, but got \n{data}")
            raise Failure("Got unexpected state")

    def wait_for_states(self, expected_states, last_state_repeats=3, already_permutated=False):
        if already_permutated:
            perms = expected_states
        else:
            perms = permutations(expected_states)

        for ctrl_index in range(len(perms)):
            try:
                self.wait_for_states_ctrl(perms[ctrl_index], last_state_repeats)
                break
            except Failure:
                if ctrl_index + 1 == len(perms):
                    raise

        for main_index in range(len(perms)):
            try:
                self.wait_for_states_main(perms[main_index], last_state_repeats)
                break
            except Failure:
                if main_index + 1 == len(perms):
                    raise

        self.actual_ctrl = perms[ctrl_index]
        self.actual_main = perms[main_index]

    def wait_for_states_ctrl(self, expected_states, last_state_repeats):
        log(f"  Node {self.node_id} waiting for states on ctrl")
        Node.__wait_for_states_internal(self.node_id, self.ctrl_queue, self.ctrl_states, expected_states,
                                        last_state_repeats)

    def wait_for_states_main(self, expected_states, last_state_repeats):
        if not self.args.only_control:
            log(f"  Node {self.node_id} waiting for states on main")
            Node.__wait_for_states_internal(self.node_id, self.main_queue, self.main_states, expected_states,
                                            last_state_repeats)

    @staticmethod
    def __wait_for_states_internal(node_id, queue, states, expected_states, last_state_repeats):
        """
        Wait until all states have been seen, in order.
        expected_states can be a string or a collection of strings.
        """
        unchecked_states = states.copy()
        #make expected_states into a list we can manipulate
        if isinstance(expected_states, str):
            expected_states = [
                expected_states,
            ]
        else:
            expected_states = list(expected_states)
        while len(expected_states) != 0:
            expected = json.loads(expected_states.pop(0))
            found = False
            mismatch = False

            while len(unchecked_states) != 0:
                state = unchecked_states[0]
                if Node.__compare_states(state, expected):
                    unchecked_states.pop(0)
                    found = True
                    if len(expected_states) == 0:
                        last_state_repeats -= 1
                        if last_state_repeats == 0:
                            return
                else:
                    mismatch = True
                    break
            if found and mismatch:
                continue
            elif mismatch:
                log(f"Node {node_id} expected state \n{expected}, but got \n{state}")
                raise Failure("Got unexpected state")

            while True:
                state = queue.get()
                if state is not None:
                    states.append(state)
                if Node.__compare_states(state, expected):
                    found = True
                    if len(expected_states) == 0:
                        last_state_repeats -= 1
                        if last_state_repeats == 0:
                            return
                else:
                    mismatch = True
                    unchecked_states.append(state)

                if found and mismatch:
                    break
                elif mismatch:
                    log(f"Node {node_id} expected state \n{expected}, but got \n{state}")
                    raise Failure("Got unexpected state")

    def close_and_check(self):
        self.close_and_check_ctrl(self.actual_ctrl)
        self.close_and_check_main(self.actual_main)

    def forget_states(self):
        log(f"Forgetting all states for node {self.node_id}.")
        while not self.ctrl_queue.empty():
            state = self.ctrl_queue.get()
            if state is None:
                break
        while not self.main_queue.empty():
            state = self.main_queue.get()
            if state is None:
                break
        self.ctrl_states = []
        self.main_states = []

    def close_and_check_ctrl(self, expected_states):
        self.__close_and_check_internal(self.ctrl_queue, self.ctrl_states, expected_states)
    def close_and_check_main(self, expected_states):
        if not self.args.only_control:
            self.__close_and_check_internal(self.main_queue, self.main_states, expected_states)

    def __close_and_check_internal(self, queue, states, expected_states):
        if self.stderr_thread is not None:
            self.close()
        while not queue.empty():
            state = queue.get()
            if state is None:
                break
            states.append(state)

        unchecked_states = states.copy()

        #make expected_states into a list we can manipulate
        if isinstance(expected_states, str):
            expected_states = [
                expected_states,
            ]
        else:
            expected_states = list(expected_states)
        while len(expected_states) != 0:
            expected = json.loads(expected_states.pop(0))
            found = False
            mismatch = False

            while len(unchecked_states) != 0:
                state = unchecked_states[0]
                if Node.__compare_states(state, expected):
                    unchecked_states.pop(0)
                    found = True
                else:
                    mismatch = True
                    break
            if not found and mismatch:
                log(f"Node {self.node_id} expected state \n{expected}, but got \n{state}")
                raise Failure("Got unexpected state")
        if len(unchecked_states) != 0 or len(expected_states) != 0:
            log(f"Node {self.node_id} got an unexpected state at end: \n{state}")
            raise Failure("Got unexpected state")

    def close(self):
        if self.stdout_thread is None:
            return
        log(f"  Closing node {self.node_id}")
        try:
            if sys.platform == "win32":
                #can't send CTRL_C_EVENT to processes started with subprocess, unfortunately
                self.proc.send_signal(signal.CTRL_BREAK_EVENT)
            else:
                self.proc.terminate()

            self.proc.wait()

        except OSError as exc:
            log("Caught exception while terminating process:", exc)

        self.stdout_thread.join()
        self.stderr_thread.join()
        self.stdout_thread = None
        self.stderr_thread = None
        self.logging_handler.close()
        self.logger.removeHandler(self.logging_handler)
        if self.proc.returncode != 0 and not self.ignore_returncode:
            log(f"Node {self.node_id} exited with error code", self.proc.returncode)
        self.returncode = self.proc.returncode


def form_system(nodes):
    states = []
    if not any(not node.light_node for node in nodes):
        raise Failure("Need at least one normal node in form_system")
    for node in nodes:
        state = node.wait_for_join_or_form()
        if "form" in state and node.light_node:
            raise Failure(f"Light node tried to form system: {state}")
        states.append(state)
    forms = 0
    incarnation = 0
    for state in states:
        if "form" in state:
            forms += 1
            incarnation = state["form"]
            if incarnation == 0:
                raise Failure("Unexpected 0 incarnation")

    if forms != 1:
        raise Failure(f"Got {forms} form statements")
    for state in states:
        if "join" in state:
            if state["join"] != incarnation:
                raise Failure("Got incorrect join!")
    return incarnation


def test_one_normal(args):
    with closing(Node(args, 1, light_node=False, multicast=False)) as node:
        state = '{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1}]}'
        node.wait_for_form()
        node.wait_for_states(state)
        node.close_and_check()
    return node.returncode == 0


def test_one_light(args):
    with closing(Node(args, 1, light_node=True, multicast=False)) as node:
        state = '{"elected_id": 1, "is_detached": true, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 11}]}'
        node.wait_for_form()
        node.wait_for_states(state)
        node.close_and_check()
    return node.returncode == 0


def test_two_normal(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2:
        state = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                     {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''
        form_system((node1, node2))
        node1.wait_for_states(state)
        node2.wait_for_states(state)
        node1.close_and_check()
        node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0


def test_two_normal_disconnect_reconnect(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                     {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                     {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1}]}'''
        state3 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": true, "id": 1, "node_type_id": 1},
                                                                     {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''
        form_system((node1, node2))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node2.disconnect()
        node1.wait_for_states((state1,state2))
        node2.wait_for_states((state1,state3))
        node2.reconnect()
        node1.wait_for_states((state1,state2), last_state_repeats = 20)
        node2.wait_for_states((state1,state3))
        node1.close_and_check()
        node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0


def test_two_normal_multicast(args):
    with closing(Node(args,1,light_node = False, multicast = True)) as node1,\
         closing(Node(args,2,light_node = False, multicast = True, seeds = 1)) as node2:
        state = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                     {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 2}]}'''
        form_system((node1, node2))
        node1.wait_for_states(state)
        node2.wait_for_states(state)
        node1.close_and_check()
        node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0


def test_two_normal_then_start_third(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2:

        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''
        state2 = '''{"elected_id": 3, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1}]}'''
        incarnation = form_system((node1, node2))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)

        with closing(Node(args, 3, light_node=False, multicast=False, seeds=2)) as node3:
            node3.wait_for_join(incarnation)
            node1.wait_for_states((state1, state2))
            node2.wait_for_states((state1, state2))
            node3.wait_for_states(state2)
            node1.close_and_check()
            node2.close_and_check()
            node3.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0


def test_two_normal_restart_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2a:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                    {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1}]}'''
        state3 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''

        incarnation = form_system((node1, node2a))

        node1.wait_for_states(state1)
        node2a.wait_for_states(state1, 1)
        node2a.close_and_check()
        node1.wait_for_states((state1, state2))
        with closing(Node(args, 2, light_node=False, multicast=False, seeds=1)) as node2b:
            incarnation2 = node2b.wait_for_form()
            if incarnation == incarnation2:
                raise Failure("unexpecedly got same incarnation")
            node2b.wait_for_states(state3)
            node1.wait_for_states((state1, state2))
            node2b.close_and_check()
            node1.close_and_check()
    return node1.returncode == 0 and node2a.returncode == 0 and node2b.returncode == 0


def test_three_normal_kill_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2,\
         closing(Node(args,3,light_node = False, multicast = False, seeds = 1)) as node3:

        state1 = '''{"elected_id": 3, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 1}]}'''
        form_system((node1, node2, node3))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node3.close_and_check()
        node1.wait_for_states((state1, state2))
        node2.wait_for_states((state1, state2))
        node1.close_and_check()
        node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0


def test_two_light(args):
    with closing(Node(args,1,light_node = True, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False)) as node2:
        state1 = '''{"elected_id": 1, "is_detached": true, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        inc1 = node1.wait_for_form()
        inc2 = node2.wait_for_form()
        if inc1 == inc2:
            raise Failure("duplicate form")
        node1.wait_for_states(state1)
        node2.wait_for_states(state2)
        node1.close_and_check()
        node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0


def test_two_light_multicast(args):
    with closing(Node(args,1,light_node = True, multicast = True)) as node1,\
         closing(Node(args,2,light_node = True, multicast = True)) as node2:
        state1 = '''{"elected_id": 1, "is_detached": true, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 12}]}'''
        state2 = '''{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 12}]}'''
        inc1 = node1.wait_for_form()
        inc2 = node2.wait_for_form()
        if inc1 == inc2:
            raise Failure("duplicate form")
        node1.wait_for_states(state1)
        node2.wait_for_states(state2)
        node1.close_and_check()
        node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0


def test_one_normal_one_light(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = 1)) as node2:
        state = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                    {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        form_system((node1, node2))
        node1.wait_for_states(state)
        node2.wait_for_states(state)
        node1.close_and_check()
        node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0

def test_one_normal_one_light_disconnect_reconnect(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = 1)) as node2:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 11}]}'''
        state3 = '''{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        inc1 = form_system((node1, node2))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node2.disconnect()
        node1.wait_for_states((state1,state2))
        inc2 = node2.wait_for_form(allowed_states=(state1,))
        if inc1 == inc2:
            raise Failure("unexpeced incarnation")
        node2.wait_for_states((state1,state3))
        node2.reconnect()
        node1.wait_for_states((state1,state2,state1))
        node2.wait_for_join(inc1, allow_detached_states=True)
        node2.wait_for_states((state1,state3, state1))
        node1.close_and_check()
        node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0


def test_one_normal_one_light_multicast(args):
    with closing(Node(args,1,light_node = False, multicast = True)) as node1,\
         closing(Node(args,2,light_node = True, multicast = True, seeds = 1)) as node2:
        state = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                     {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 12}]}'''
        form_system((node1, node2))
        node1.wait_for_states(state)
        node2.wait_for_states(state)
        node1.close_and_check()
        node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0


def test_one_normal_then_one_light(args):
    with closing(Node(args, 1, light_node=False, multicast=False)) as node1:
        incarnation = node1.wait_for_form()
        state1 = '{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1}]}'
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''

        node1.wait_for_states(state1)
        with closing(Node(args, 2, light_node=True, multicast=False, seeds=1)) as node2:
            node2.wait_for_join(incarnation)
            node1.wait_for_states((state1, state2))
            node2.wait_for_states(state2)
            node1.close_and_check()
            node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0


def test_one_light_then_one_normal(args):
    with closing(Node(args, 1, light_node=True, multicast=False, seeds=2)) as node1:
        state1 = '{"elected_id": 1, "is_detached": true, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 11}]}'
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 11},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''

        inc1 = node1.wait_for_form()
        node1.wait_for_states(state1)
        with closing(Node(args, 2, light_node=False, multicast=False)) as node2:
            inc2 = node2.wait_for_form()
            node1.wait_for_join(inc2, allow_detached_states=True)
            if inc1 == inc2:
                raise Failure("unexpected incarnation")
            node1.wait_for_states((state1, state2))
            node2.wait_for_states(state2)
            node1.close_and_check()
            node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0


def test_one_normal_one_light_kill_normal(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = 1)) as node2:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        state2 = '{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'
        inc1 = form_system((node1, node2))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node1.close()
        inc2 = node2.wait_for_form(allowed_states=(state1, ))
        if inc1 == inc2:
            raise Failure("unexpected incarnation")
        node2.wait_for_states((state1, state2))
        node2.close_and_check()
        node1.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0


def test_one_normal_one_light_restart_normal(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1a,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = 1)) as node2:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                    {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        state2 = '{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'
        state3 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1}]}'''
        inc1 = form_system((node1a, node2))
        node2.wait_for_states(state1)
        node1a.wait_for_states(state1)
        node1a.close()
        inc2 = node2.wait_for_form(allowed_states=(state1, ))
        if inc1 == inc2:
            raise Failure("unexpected incarnation")
        node2.wait_for_states((state1, state2))
        node1a.close_and_check()
        with closing(Node(args, 1, light_node=False, multicast=False)) as node1b:
            inc3 = node1b.wait_for_form()
            node2.wait_for_join(inc3, allow_detached_states=True)
            if inc3 in (inc1, inc2):
                raise Failure("unexpected incarnation")
            node2.wait_for_states((state1, state2, state1))
            node1b.wait_for_states((state3, state1))
            node2.close_and_check()
            node1b.close_and_check()

    return node1a.returncode == 0 and node1b.returncode == 0 and node2.returncode == 0


def test_one_normal_one_light_restart_normal_multicast(args):
    with closing(Node(args,1,light_node = False, multicast = True)) as node1a,\
         closing(Node(args,2,light_node = True, multicast = True, seeds = 1)) as node2:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                                    {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 12}]}'''
        state2 = '{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 12}]}'
        state3 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2}]}'''
        inc1 = node1a.wait_for_form()
        node2.wait_for_join(inc1)
        node2.wait_for_states(state1)
        node1a.wait_for_states(state1)
        node1a.close()
        inc2 = node2.wait_for_form(allowed_states=(state1, ))
        if inc1 == inc2:
            raise Failure("unexpected incarnation")
        node2.wait_for_states((state1, state2))
        node1a.close_and_check()
        with closing(Node(args, 1, light_node=False, multicast=True)) as node1b:
            inc3 = node1b.wait_for_form()
            node2.wait_for_join(inc3, allow_detached_states=True)
            if inc3 in (inc1, inc2):
                raise Failure("unexpected incarnation")
            node2.wait_for_states((state1, state2, state1))
            node1b.wait_for_states((state3, state1))
            node2.close_and_check()
            node1b.close_and_check()
    return node1a.returncode == 0 and node1b.returncode == 0 and node2.returncode == 0


def test_one_normal_one_light_kill_light(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = 1)) as node2:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 11}]}'''
        form_system((node1, node2))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node2.close_and_check()
        node1.wait_for_states((state1, state2))
        node1.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0


def test_one_normal_one_light_restart_light(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = 1)) as node2a:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 11}]}'''
        inc = form_system((node1, node2a))
        node1.wait_for_states(state1)
        node2a.wait_for_states(state1)
        node2a.close_and_check()
        node1.wait_for_states((state1, state2))
        with closing(Node(args, 2, light_node=True, multicast=False, seeds=1)) as node2b:
            node2b.wait_for_join(inc)
            node1.wait_for_states((state1, state2, state1))
            node2b.wait_for_states(state1)
            node1.close_and_check()
            node2b.close_and_check()
    return node1.returncode == 0 and node2a.returncode == 0 and node2b.returncode == 0


def test_one_normal_one_light_kill_light_start_new_light(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = 1)) as node2:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 11}]}'''
        state3 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 11},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        inc = form_system((node1, node2))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node2.close_and_check()
        node1.wait_for_states((state1, state2))
        with closing(Node(args, 3, light_node=True, multicast=False, seeds=1)) as node3:
            node3.wait_for_join(inc)
            node3.wait_for_states(state3)
            node1.wait_for_states((state1,state2,state3))
            node3.close_and_check()
            node1.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0

def test_one_normal_one_light_kill_light_start_new_light_fast(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = 1)) as node2:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 11},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        inc = form_system((node1, node2))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node2.close_and_check()
        with closing(Node(args, 3, light_node=True, multicast=False, seeds=1)) as node3:
            node3.wait_for_join(inc)
            node1.wait_for_states((state1, state2))
            node3.wait_for_states(state2)
            node3.close_and_check()
            node1.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0

def test_one_normal_two_light_kill_normal(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seeds = 1)) as node3:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'
        state3 = '{"elected_id": 3, "is_detached": true, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'
        inc1 = form_system((node1, node2, node3))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node1.close()
        inc2 = node2.wait_for_form(allowed_states=(state1, ))
        inc3 = node3.wait_for_form(allowed_states=(state1, ))
        if inc1 in (inc2, inc3) or inc2 in (inc1, inc3):
            raise Failure("unexpected incarnation")
        node2.wait_for_states((state1, state2))
        node3.wait_for_states((state1, state3))
        node2.close_and_check()
        node3.close_and_check()
        node1.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0

def test_one_normal_two_light_restart_normal(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1a,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seeds = 1)) as node3:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'
        state3 = '{"elected_id": 3, "is_detached": true, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'
        state4 = '{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1}]}'
        state5a = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        state5b = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''

        inc1 = form_system((node1a, node2, node3))
        node1a.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node1a.close_and_check()
        inc2 = node2.wait_for_form(allowed_states=(state1, ))
        inc3 = node3.wait_for_form(allowed_states=(state1, ))
        if inc1 in (inc2, inc3) or inc2 in (inc1, inc3):
            raise Failure("unexpected incarnation")
        node2.wait_for_states((state1, state2))
        node3.wait_for_states((state1, state3))
        with closing(Node(args,1,light_node = False, multicast = False)) as node1b:
            inc4 = node1b.wait_for_form()
            node2.wait_for_join(inc4, allow_detached_states = True)
            node3.wait_for_join(inc4, allow_detached_states = True)
            if inc4 in (inc1, inc2, inc3):
                raise Failure("unexpected incarnation")
            log("  This testcase produces some spurious output since it checks for many possible outcomes")
            log("  So ignore any lines below talking about incorrect states unless the test fails.")
            try:
                actual1b = (state4, state1)
                node1b.wait_for_states(actual1b)
            except Failure:
                try:
                    actual1b = (state4, state5a, state1)
                    node1b.wait_for_states(actual1b)
                except Failure:
                    actual1b = (state4, state5b, state1)
                    node1b.wait_for_states(actual1b)

            try:
                actual2 = (state1, state2, state1)
                node2.wait_for_states(actual2)
            except Failure:
                try:
                    actual2 = (state1, state2, state5a, state1)
                    node2.wait_for_states(actual2)
                except Failure:
                    actual2 = (state1, state2, state1)
                    node2.wait_for_states(actual2)

            try:
                actual3 = (state1, state3, state1)
                node3.wait_for_states(actual3)
            except Failure:
                try:
                    actual3 = (state1, state3, state1)
                    node3.wait_for_states(actual3)
                except Failure:
                    actual3 = (state1, state3, state5b, state1)
                    node3.wait_for_states(actual3)

            node1b.close_and_check()
            node2.close_and_check()
            node3.close_and_check()
    return node1a.returncode == 0 and node1b.returncode == 0 and node2.returncode == 0 and node3.returncode == 0

def test_one_normal_two_light_restart_normal_multicast(args):
    with closing(Node(args,1,light_node = False, multicast = True)) as node1a,\
         closing(Node(args,2,light_node = True, multicast = True, seeds = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = True, seeds = 1)) as node3:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 12},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 12}]}'''
        state2 = '{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 12}]}'
        state3 = '{"elected_id": 3, "is_detached": true, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 12}]}'
        state4 = '{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2}]}'
        state5a = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 12}]}'''
        state5b = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 12}]}'''

        inc1 = form_system((node1a, node2, node3))
        node1a.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node1a.close_and_check()
        inc2 = node2.wait_for_form(allowed_states=(state1, ))
        inc3 = node3.wait_for_form(allowed_states=(state1, ))
        if inc1 in (inc2, inc3) or inc2 in (inc1, inc3):
            raise Failure("unexpected incarnation")
        node2.wait_for_states((state1, state2))
        node3.wait_for_states((state1, state3))
        with closing(Node(args,1,light_node = False, multicast = True)) as node1b:
            inc4 = node1b.wait_for_form()
            node2.wait_for_join(inc4, allow_detached_states = True)
            node3.wait_for_join(inc4, allow_detached_states = True)
            if inc4 in (inc1, inc2, inc3):
                raise Failure("unexpected incarnation")
            log("  This testcase produces some spurious output since it checks for many possible outcomes")
            log("  So ignore any lines below talking about incorrect states unless the test fails.")
            try:
                actual1b = (state4, state1)
                node1b.wait_for_states(actual1b)
            except Failure:
                try:
                    actual1b = (state4, state5a, state1)
                    node1b.wait_for_states(actual1b)
                except Failure:
                    actual1b = (state4, state5b, state1)
                    node1b.wait_for_states(actual1b)

            try:
                actual2 = (state1, state2, state1)
                node2.wait_for_states(actual2)
            except Failure:
                try:
                    actual2 = (state1, state2, state5a, state1)
                    node2.wait_for_states(actual2)
                except Failure:
                    actual2 = (state1, state2, state1)
                    node2.wait_for_states(actual2)

            try:
                actual3 = (state1, state3, state1)
                node3.wait_for_states(actual3)
            except Failure:
                try:
                    actual3 = (state1, state3, state1)
                    node3.wait_for_states(actual3)
                except Failure:
                    actual3 = (state1, state3, state5b, state1)
                    node3.wait_for_states(actual3)

            node1b.close_and_check()
            node2.close_and_check()
            node3.close_and_check()
    return node1a.returncode == 0 and node1b.returncode == 0 and node2.returncode == 0 and node3.returncode == 0

def test_two_normal_one_light_kill_light(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seeds = 1)) as node3:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11}]}'''
        form_system((node1, node2, node3))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node3.close_and_check()
        node1.wait_for_states((state1, state2))
        node2.wait_for_states((state1, state2))
        node1.close_and_check()
        node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0


def test_two_normal_one_light_restart_light(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seeds = 1)) as node3a:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11}]}'''
        inc = form_system((node1, node2, node3a))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3a.wait_for_states(state1)
        node3a.close_and_check()
        node1.wait_for_states((state1, state2))
        node2.wait_for_states((state1, state2))
        with closing(Node(args, 3, light_node=True, multicast=False, seeds=1)) as node3b:
            node3b.wait_for_join(inc)
            node1.wait_for_states((state1, state2, state1))
            node2.wait_for_states((state1, state2, state1))
            node3b.wait_for_states(state1)
            node1.close_and_check()
            node2.close_and_check()
            node3b.close_and_check()

    return node1.returncode == 0 and node2.returncode == 0 and node3a.returncode == 0 and node3b.returncode == 0


def test_two_normal_one_light_kill_normal(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seeds = 2)) as node3:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": true, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        form_system((node1, node2, node3))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node1.close_and_check()
        node2.wait_for_states((state1, state2))
        node3.wait_for_states((state1, state2))
        node2.close_and_check()
        node3.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0


def test_two_normal_one_light_kill_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seeds = 2)) as node3:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        form_system((node1, node2, node3))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node2.close_and_check()
        node1.wait_for_states((state1, state2))
        node3.wait_for_states((state1, state2))
        node1.close_and_check()
        node3.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0


def test_two_normal_one_light_restart_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2a,\
         closing(Node(args,3,light_node = True, multicast = False, seeds = 2)) as node3:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state3a = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": true, "id": 1, "node_type_id": 1},
                                                                       {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''
        state3b = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''
        state4a = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": true, "id": 1, "node_type_id": 1},
                                                                       {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                       {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11}]}'''
        state4b = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                       {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11}]}'''

        inc1 = form_system((node1, node2a, node3))
        node1.wait_for_states(state1)
        node2a.wait_for_states(state1)
        node3.wait_for_states(state1)
        node2a.close_and_check()
        node1.wait_for_states((state1, state2))
        node3.wait_for_states((state1, state2))
        with closing(Node(args, 2, light_node=False, multicast=False, seeds=1)) as node2b:
            inc2 = node2b.wait_for_form()
            if inc1 == inc2:
                raise Failure("unexpected incarnation")
            node2b.wait_for_states(((state3a, state4a),(state3b, state4b)), already_permutated = True)
            node1.wait_for_states((state1, state2), last_state_repeats=20)
            node3.wait_for_states((state1, state2), last_state_repeats=20)
            node1.close_and_check()
            node3.close_and_check()
            node2b.close_and_check()
    return node1.returncode == 0 and node2a.returncode == 0 and node2b.returncode == 0 and node3.returncode == 0

def test_two_normal_one_light_kill_both_normal_and_start_new_normals(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seeds = (2,4))) as node3:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 3, "is_detached": true, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state3 = '''{"elected_id": 5, "is_detached": false, "nodes": [{"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 1},
                                                                      {"name": "node_005", "is_dead": false, "id": 5, "node_type_id": 1}]}'''
        state4 = '''{"elected_id": 5, "is_detached": false, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 1},
                                                                      {"name": "node_005", "is_dead": false, "id": 5, "node_type_id": 1}]}'''
        inc1 = form_system((node1, node2, node3))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node1.close_and_check()
        node2.close_and_check()
        inc2 = node3.wait_for_form(allowed_states=(state1,))
        with closing(Node(args,4,light_node = False, multicast = False, seeds = 5)) as node4,\
             closing(Node(args,5,light_node = False, multicast = False, seeds = 4)) as node5:
            inc3 = form_system((node4, node5))
            if inc3 in (inc1, inc2) or inc2 in (inc1, inc3):
                raise Failure("unexpected incarnation")
            node3.wait_for_join(inc3, allow_detached_states=True)
            node3.wait_for_states((state1,state2,state4))
            node4.wait_for_states(state4)
            node5.wait_for_states(state4)
            node3.close_and_check()
            node4.close_and_check()
            node5.close_and_check()
        return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and \
            node4.returncode == 0 and node5.returncode == 0


def test_two_normal_one_light_kill_both_normal_and_start_new_normals_fast(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seeds = (2,4))) as node3:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 3, "is_detached": true, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state3 = '''{"elected_id": 5, "is_detached": false, "nodes": [{"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 1},
                                                                      {"name": "node_005", "is_dead": false, "id": 5, "node_type_id": 1}]}'''
        state4 = '''{"elected_id": 5, "is_detached": false, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 1},
                                                                      {"name": "node_005", "is_dead": false, "id": 5, "node_type_id": 1}]}'''
        inc1 = form_system((node1, node2, node3))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node1.close_and_check()
        node2.close_and_check()
        with closing(Node(args,4,light_node = False, multicast = False, seeds = 5)) as node4,\
             closing(Node(args,5,light_node = False, multicast = False, seeds = 4)) as node5:
            inc2 = node3.wait_for_form(allowed_states=(state1,))
            inc3 = form_system((node4, node5))
            if inc3 in (inc1, inc2) or inc2 in (inc1, inc3):
                raise Failure("unexpected incarnation")
            node3.wait_for_join(inc3, allow_detached_states=True)
            node3.wait_for_states((state1,state2,state4))
            try:
                actual45=(state4,)
                node4.wait_for_states(actual45)
                node5.wait_for_states(actual45)
            except Failure:
                actual45=(state3, state4)
                node4.wait_for_states(actual45)
                node5.wait_for_states(actual45)
            node3.close_and_check()
            node4.close_and_check()
            node5.close_and_check()
        return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and \
            node4.returncode == 0 and node5.returncode == 0

def test_two_normal_two_light(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seeds = 1)) as node3, \
         closing(Node(args,4,light_node = True, multicast = False, seeds = 1)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        form_system((node1, node2, node3, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node1.close_and_check()
        node2.close_and_check()
        node3.close_and_check()
        node4.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0


def test_two_normal_two_light_disconnect_reconnect_both_light(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seeds = 1)) as node3, \
         closing(Node(args,4,light_node = True, multicast = False, seeds = 1)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state2a = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": true, "id": 4, "node_type_id": 11}]}'''
        state2b = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state2c = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": true, "id": 4, "node_type_id": 11}]}'''
        state3 = '''{"elected_id": 3, "is_detached": true, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state4 = '''{"elected_id": 4, "is_detached": true, "nodes": [{"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state5a = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": true, "id": 4, "node_type_id": 11}]}'''
        state5b = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        inc1 = form_system((node1, node2, node3, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node3.disconnect()
        node4.disconnect()
        node2.wait_for_states((state1,(None,state2b,state2c),state2a))
        node1.wait_for_states((state1,(None,state2b,state2c),state2a))

        inc2 = node3.wait_for_form(allowed_states=(state1,))
        inc3 = node4.wait_for_form(allowed_states=(state1,))
        if inc3 in (inc1, inc2) or inc2 in (inc1, inc3):
            raise Failure("unexpected incarnation")
        node3.wait_for_states((state1,state3))
        node4.wait_for_states((state1,state4))
        node3.reconnect()
        node4.reconnect()
        node3.wait_for_join(inc1, allow_detached_states=True)
        node4.wait_for_join(inc1, allow_detached_states=True)
        node3.wait_for_states((state1,state3,(None,state5a),state1), last_state_repeats=80)
        node4.wait_for_states((state1,state4,(None,state5b),state1), last_state_repeats=80)
        node2.wait_for_states((state1,(None,state2b,state2c),state2a,(None,state5a,state5b),state1), last_state_repeats=80)
        node1.wait_for_states((state1,(None,state2b,state2c),state2a,(None,state5a,state5b),state1), last_state_repeats=80)

        node1.close_and_check()
        node2.close_and_check()
        node3.close_and_check()
        node4.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0


def test_two_normal_two_light_multicast(args):
    with closing(Node(args,1,light_node = False, multicast = True, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = True, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = True, seeds = 1)) as node3, \
         closing(Node(args,4,light_node = True, multicast = True, seeds = 1)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 2},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 12},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 12}]}'''

        form_system((node1, node2, node3, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node1.close_and_check()
        node2.close_and_check()
        node3.close_and_check()
        node4.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0


def test_two_normal_two_light_kill_light(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seeds = 1)) as node3, \
         closing(Node(args,4,light_node = True, multicast = False, seeds = 1)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''

        form_system((node1, node2, node3, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node3.close_and_check()
        node1.wait_for_states((state1, state2))
        node2.wait_for_states((state1, state2))
        node4.wait_for_states((state1, state2))
        node1.close_and_check()
        node2.close_and_check()
        node4.close_and_check()

    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0


def test_two_normal_two_light_restart_light(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seeds = 1)) as node3a, \
         closing(Node(args,4,light_node = True, multicast = False, seeds = 1)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        inc = form_system((node1, node2, node3a, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3a.wait_for_states(state1)
        node4.wait_for_states(state1)
        node3a.close_and_check()
        node1.wait_for_states((state1, state2))
        node2.wait_for_states((state1, state2))
        node4.wait_for_states((state1, state2))
        with closing(Node(args, 3, light_node=True, multicast=False, seeds=1)) as node3b:
            node3b.wait_for_join(inc)
            node3b.wait_for_states(state1)
            node1.wait_for_states((state1, state2, state1), last_state_repeats=10)
            node2.wait_for_states((state1, state2, state1), last_state_repeats=10)
            node4.wait_for_states((state1, state2, state1), last_state_repeats=10)
            node3b.close_and_check()
            node1.close_and_check()
            node2.close_and_check()
            node4.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3a.returncode == 0 \
        and node3b.returncode == 0 and node4.returncode == 0


def test_two_normal_two_light_restart_light_multicast(args):
    with closing(Node(args,1,light_node = False, multicast = True, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = True, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = True, seeds = 1)) as node3a, \
         closing(Node(args,4,light_node = True, multicast = True, seeds = 1)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 2},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 12},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 12}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 2},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 12},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 12}]}'''

        inc = form_system((node1, node2, node3a, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3a.wait_for_states(state1)
        node4.wait_for_states(state1)
        node3a.close_and_check()
        node1.wait_for_states((state1, state2))
        node2.wait_for_states((state1, state2))
        node4.wait_for_states((state1, state2))
        with closing(Node(args, 3, light_node=True, multicast=True, seeds=1)) as node3b:
            node3b.wait_for_join(inc)
            node3b.wait_for_states(state1)
            node1.wait_for_states((state1, state2, state1), last_state_repeats=10)
            node2.wait_for_states((state1, state2, state1), last_state_repeats=10)
            node4.wait_for_states((state1, state2, state1), last_state_repeats=10)
            node3b.close_and_check()
            node1.close_and_check()
            node2.close_and_check()
            node4.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3a.returncode == 0 \
        and node3b.returncode == 0 and node4.returncode == 0


def test_two_normal_two_light_kill_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seeds = 1)) as node3, \
         closing(Node(args,4,light_node = True, multicast = False, seeds = 1)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        form_system((node1, node2, node3, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node2.close_and_check()
        node1.wait_for_states((state1, state2))
        node3.wait_for_states((state1, state2))
        node4.wait_for_states((state1, state2))
        node1.close_and_check()
        node3.close_and_check()
        node4.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0

def test_two_normal_two_light_disconnect_reconnect_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seeds = 2)) as node3, \
         closing(Node(args,4,light_node = True, multicast = False, seeds = 2)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state3 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": true, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": true, "id": 4, "node_type_id": 11}]}'''


        form_system((node1, node2, node3, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node2.disconnect()
        node1.wait_for_states((state1, state2))
        node3.wait_for_states((state1, state2))
        node4.wait_for_states((state1, state2))
        node2.reconnect()
        node1.wait_for_states((state1, state2), last_state_repeats=20)
        node3.wait_for_states((state1, state2), last_state_repeats=20)
        node4.wait_for_states((state1, state2), last_state_repeats=20)
        #node2 will have excluded all the other nodes in some random order, making it
        #tedious to check exactly what happened. So we just check the other nodes.
        node1.close_and_check()
        node3.close_and_check()
        node4.close_and_check()
        node2.close()
    return node1.returncode == 0 and node2.returncode == 0 \
        and node3.returncode == 0 and node4.returncode == 0

def test_two_normal_two_light_disconnect_reconnect_elected_multicast(args):
    with closing(Node(args,1,light_node = False, multicast = True, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = True, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = True, seeds = 2)) as node3, \
         closing(Node(args,4,light_node = True, multicast = True, seeds = 2)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 2},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 12},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 12}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 2},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 12},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 12}]}'''

        form_system((node1, node2, node3, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node2.disconnect()
        node1.wait_for_states((state1, state2))
        node3.wait_for_states((state1, state2))
        node4.wait_for_states((state1, state2))
        node2.reconnect()
        node1.wait_for_states((state1, state2), last_state_repeats=20)
        node3.wait_for_states((state1, state2), last_state_repeats=20)
        node4.wait_for_states((state1, state2), last_state_repeats=20)
        #node2 will have excluded all the other nodes in some random order, making it
        #tedious to check exactly what happened. So we just check the other nodes.
        node1.close_and_check()
        node3.close_and_check()
        node4.close_and_check()
        node2.close()
    return node1.returncode == 0 and node2.returncode == 0 \
        and node3.returncode == 0 and node4.returncode == 0

def test_two_normal_two_light_kill_elected_and_start_new_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seeds = (1,2,22))) as node3, \
         closing(Node(args,4,light_node = True, multicast = False, seeds = (1,2,22))) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state3 = '''{"elected_id": 22, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11},
                                                                      {"name": "node_022", "is_dead": false, "id": 22, "node_type_id": 1}]}'''

        inc = form_system((node1, node2, node3, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node2.close_and_check()
        node1.wait_for_states((state1, state2))
        node3.wait_for_states((state1, state2))
        node4.wait_for_states((state1, state2))
        with closing(Node(args, 22, light_node=False, multicast = False, seeds=1)) as node22:
            node22.wait_for_join(inc)
            node22.wait_for_states(state3)
            node1.wait_for_states((state1, state2, state3))
            node3.wait_for_states((state1, state2, state3))
            node4.wait_for_states((state1, state2, state3))
            node1.close_and_check()
            node3.close_and_check()
            node4.close_and_check()

    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0 and node22.returncode == 0

def test_two_normal_two_light_kill_elected_and_start_new_elected_fast(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seeds = (1,2,22))) as node3, \
         closing(Node(args,4,light_node = True, multicast = False, seeds = (1,2,22))) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state3 = '''{"elected_id": 22, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11},
                                                                      {"name": "node_022", "is_dead": false, "id": 22, "node_type_id": 1}]}'''

        inc = form_system((node1, node2, node3, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node2.close_and_check()
        with closing(Node(args, 22, light_node=False, multicast = False, seeds=1)) as node22:
            node22.wait_for_join(inc)
            node22.wait_for_states(state3)
            node1.wait_for_states((state1, state3))
            node3.wait_for_states((state1, state3))
            node4.wait_for_states((state1, state3))
            node1.close_and_check()
            node3.close_and_check()
            node4.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0 and node22.returncode == 0

def test_two_normal_two_light_kill_elected_and_start_new_elected_multicast(args):
    with closing(Node(args,1,light_node = False, multicast = True, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = True, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = True, seeds = (1,2,22))) as node3, \
         closing(Node(args,4,light_node = True, multicast = True, seeds = (1,2,22))) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 2},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 12},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 12}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 2},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 12},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 12}]}'''
        state3 = '''{"elected_id": 22, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 2},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 12},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 12},
                                                                      {"name": "node_022", "is_dead": false, "id": 22, "node_type_id": 2}]}'''

        inc = form_system((node1, node2, node3, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node2.close_and_check()
        node1.wait_for_states((state1, state2))
        node3.wait_for_states((state1, state2))
        node4.wait_for_states((state1, state2))
        with closing(Node(args, 22, light_node=False, multicast=True, seeds=1)) as node22:
            node22.wait_for_join(inc)
            node22.wait_for_states(state3)
            node1.wait_for_states((state1, state2, state3))
            node3.wait_for_states((state1, state2, state3))
            node4.wait_for_states((state1, state2, state3))
            node1.close_and_check()
            node3.close_and_check()
            node4.close_and_check()

    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0 and node22.returncode == 0

def test_two_normal_two_light_kill_elected_and_start_new_elected_multicast_fast(args):
    with closing(Node(args,1,light_node = False, multicast = True, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = True, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = True, seeds = (1,2,22))) as node3, \
         closing(Node(args,4,light_node = True, multicast = True, seeds = (1,2,22))) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 2},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 12},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 12}]}'''
        state3 = '''{"elected_id": 22, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 2},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 2},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 12},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 12},
                                                                      {"name": "node_022", "is_dead": false, "id": 22, "node_type_id": 2}]}'''

        inc = form_system((node1, node2, node3, node4))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node2.close_and_check()
        with closing(Node(args, 22, light_node=False, multicast=True, seeds=1)) as node22:
            node22.wait_for_join(inc)
            node22.wait_for_states(state3)
            node1.wait_for_states((state1, state3))
            node3.wait_for_states((state1, state3))
            node4.wait_for_states((state1, state3))
            node1.close_and_check()
            node3.close_and_check()
            node4.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0 and node22.returncode == 0

def test_four_normal_four_light_mixedcast(args):
    with closing(Node(args,1,light_node = False, multicast = False, seeds = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = True, seeds = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seeds = 1)) as node3, \
         closing(Node(args,4,light_node = True, multicast = True, seeds = 1)) as node4, \
         closing(Node(args,5,light_node = False, multicast = False, seeds = 2)) as node5, \
         closing(Node(args,6,light_node = False, multicast = True, seeds = 1)) as node6, \
         closing(Node(args,7,light_node = True, multicast = False, seeds = 1)) as node7, \
         closing(Node(args,8,light_node = True, multicast = True, seeds = 1)) as node8:
        state1 = '''{"elected_id": 6, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 2},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 12},
                                                                      {"name": "node_005", "is_dead": false, "id": 5, "node_type_id": 1},
                                                                      {"name": "node_006", "is_dead": false, "id": 6, "node_type_id": 2},
                                                                      {"name": "node_007", "is_dead": false, "id": 7, "node_type_id": 11},
                                                                      {"name": "node_008", "is_dead": false, "id": 8, "node_type_id": 12}]}'''
        form_system((node1, node2, node3, node4, node5, node6, node7, node8))
        node1.wait_for_states(state1, last_state_repeats=30)
        node2.wait_for_states(state1, last_state_repeats=30)
        node3.wait_for_states(state1, last_state_repeats=30)
        node4.wait_for_states(state1, last_state_repeats=30)
        node5.wait_for_states(state1, last_state_repeats=30)
        node6.wait_for_states(state1, last_state_repeats=30)
        node7.wait_for_states(state1, last_state_repeats=30)
        node8.wait_for_states(state1, last_state_repeats=30)
        #We don't use close_and_check here, since sometimes nodes pick up others being killed.
        #The order of which is difficult to predict.
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0 \
        and node5.returncode == 0 and node6.returncode == 0 and node7.returncode == 0 and node8.returncode == 0


def test_move_lightnode_to_other_new_node(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = (1,3))) as node2:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        state2 = '{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'
        state3 = '''{"elected_id": 3, "is_detached": false, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1}]}'''
        inc1 = form_system((node1, node2))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)

        node1.close_and_check()
        inc2 = node2.wait_for_form(allowed_states=(state1, ))
        if inc1 == inc2:
            raise Failure("unexpected incarnation")
        with closing(Node(args, 3, light_node=False, multicast=False)) as node3:
            inc3 = node3.wait_for_form()
            node2.wait_for_join(inc3, allow_detached_states=True)
            if inc3 in (inc1, inc2):
                raise Failure("unexpected incarnation")
            node3.wait_for_states(state3)
            node2.wait_for_states((state1, state2, state3))
            node3.close_and_check()
            node2.close_and_check()

    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0


def test_move_lightnode_to_other_new_node_fast(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = (1,3))) as node2:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        state2 = '{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'
        state4 = '''{"elected_id": 3, "is_detached": false, "nodes": [{"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 11},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1}]}'''
        state5 = '''{"elected_id": 3, "is_detached": false, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1}]}'''
        inc1 = form_system((node1, node2))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)

        node1.close_and_check()
        with closing(Node(args, 3, light_node=False, multicast=False)) as node3:
            inc2 = node3.wait_for_form()
            inc3 = node2.wait_for_form(allowed_states=(state1,))
            if inc3 in (inc1, inc2) or inc2 in (inc1, inc3):
                raise Failure("unexpected incarnation")
            node2.wait_for_join(inc2, allow_detached_states=True)
            node3.wait_for_states((state4, state5), last_state_repeats=40)
            node2.wait_for_states((state1, state2, state5))
            node3.close_and_check()
            node2.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0


def test_two_normal_one_light_weird_seed(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seeds = (1,2))) as node3:
        inc1 = node1.wait_for_form()
        inc2 = node2.wait_for_form()
        inc3 = node3.wait_for_join(None)
        if inc3 == inc1:
            state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                         {"name": "<unknown>", "is_dead": true, "id": 2, "node_type_id": 0},
                                                                         {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
            state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                          {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11}]}'''
            node2.wait_for_states(state2, last_state_repeats=20)
            node1.wait_for_states(state1)
            node3.wait_for_states(state1)
            node3.close_and_check()
            node1.close_and_check()
            node2.ignore_returncode = True
            return node1.returncode == 0 and node3.returncode == 0
        elif inc3 == inc2:
            state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "<unknown>", "is_dead": true, "id": 1, "node_type_id": 0},
                                                                         {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                         {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
            state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                          {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11}]}'''
            node1.wait_for_states(state2, last_state_repeats=20)
            node2.wait_for_states(state1)
            node3.wait_for_states(state1)
            node3.close_and_check()
            node2.close_and_check()
            node1.ignore_returncode = True
            return node2.returncode == 0 and node3.returncode == 0
        else:
            raise Failure("unexpected incarnation")

def test_move_lightnode_to_other_system(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2,\
         closing(Node(args,3,light_node = False, multicast = False)) as node3,\
         closing(Node(args,5,light_node = True, multicast = False, seeds = (1,4))) as node5:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_005", "is_dead": false, "id": 5, "node_type_id": 11}]}'''
        state2 = '{"elected_id": 3, "is_detached": false, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1}]}'
        state3 = '{"elected_id": 5, "is_detached": true, "nodes": [{"name": "node_005", "is_dead": false, "id": 5, "node_type_id": 11}]}'
        state4 = '''{"elected_id": 4, "is_detached": false, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 1}]}'''
        state5 = '''{"elected_id": 4, "is_detached": false, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 1},
                                                                      {"name": "node_005", "is_dead": false, "id": 5, "node_type_id": 11}]}'''

        inc1 = form_system((node1, node2, node5))
        inc2 = node3.wait_for_form()
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node5.wait_for_states(state1)
        node3.wait_for_states(state2)

        node1.close_and_check()
        node2.close_and_check()
        inc3 = node5.wait_for_form(allowed_states=(state1, ))
        with closing(Node(args, 4, light_node=False, multicast=False, seeds=3)) as node4:
            node4.wait_for_join(inc2)
            node5.wait_for_join(inc2, allow_detached_states = True)
            if inc3 in (inc1, inc2) or inc2 in (inc1, inc3):
                raise Failure("unexpected incarnation")
            node5.wait_for_states((state1, state3, state5), last_state_repeats=30)
            try:
                node3_actual_ctrl = (state2, state5)
                node3.wait_for_states_ctrl(node3_actual_ctrl, last_state_repeats=30)
            except Failure:
                node3_actual_ctrl = (state2, state4, state5)
                node3.wait_for_states_ctrl(node3_actual_ctrl, last_state_repeats=30)
            try:
                node4_actual_ctrl = state5
                node4.wait_for_states_ctrl(node4_actual_ctrl, last_state_repeats=30)
            except Failure:
                node4_actual_ctrl = (state4, state5)
                node4.wait_for_states_ctrl(node4_actual_ctrl, last_state_repeats=30)
            try:
                node3_actual_main = (state2, state5)
                node3.wait_for_states_main(node3_actual_main, last_state_repeats=30)
            except Failure:
                node3_actual_main = (state2, state4, state5)
                node3.wait_for_states_main(node3_actual_main, last_state_repeats=30)
            try:
                node4_actual_main = state5
                node4.wait_for_states_main(node4_actual_main, last_state_repeats=30)
            except Failure:
                node4_actual_main = (state4, state5)
                node4.wait_for_states_main(node4_actual_main, last_state_repeats=30)
            node3.close_and_check_ctrl(node3_actual_ctrl)
            node3.close_and_check_main(node3_actual_main)
            node4.close_and_check_ctrl(node4_actual_ctrl)
            node4.close_and_check_main(node4_actual_main)
            node5.close_and_check()
        return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and \
            node4.returncode == 0 and node5.returncode == 0

def test_move_lightnode_to_other_system_fast(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seeds = 1)) as node2,\
         closing(Node(args,3,light_node = False, multicast = False)) as node3,\
         closing(Node(args,5,light_node = True, multicast = False, seeds = (1,4))) as node5:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_005", "is_dead": false, "id": 5, "node_type_id": 11}]}'''
        state2 = '{"elected_id": 3, "is_detached": false, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1}]}'
        state3 = '{"elected_id": 5, "is_detached": true, "nodes": [{"name": "node_005", "is_dead": false, "id": 5, "node_type_id": 11}]}'
        state4a = '''{"elected_id": 4, "is_detached": false, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 1},
                                                                      {"name": "node_005", "is_dead": true, "id": 5, "node_type_id": 11}]}'''
        state4b = '''{"elected_id": 4, "is_detached": false, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 1}]}'''
        state5 = '''{"elected_id": 4, "is_detached": false, "nodes": [{"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 1},
                                                                      {"name": "node_005", "is_dead": false, "id": 5, "node_type_id": 11}]}'''

        inc1 = form_system((node1, node2, node5))
        inc2 = node3.wait_for_form()
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node5.wait_for_states(state1)
        node3.wait_for_states(state2)

        node1.close_and_check()
        node2.close_and_check()
        with closing(Node(args, 4, light_node=False, multicast=False, seeds=3)) as node4:
            node4.wait_for_join(inc2)
            inc3 = node5.wait_for_form(allowed_states=(state1, ))
            node5.wait_for_join(inc2, allow_detached_states=True)
            if inc3 in (inc1, inc2) or inc2 in (inc1, inc3):
                raise Failure("unexpected incarnation")
            try:
                inc4 = node5.wait_for_form(allowed_states=(state1,))
                node5.wait_for_states((state1,state3), last_state_repeats = 30)
                node3.wait_for_states((state2, state4b), last_state_repeats=30)
                node4.wait_for_states(state4b, last_state_repeats=30)
                node5.close_and_check()
                node3.close_and_check()
                node4.close_and_check()
            except Failure:
                node5.wait_for_states((state1, state3, state5), last_state_repeats=30)
                try:
                    node3.wait_for_states((state2, state4a, state5), last_state_repeats=30)
                    node4.wait_for_states((state4a, state5), last_state_repeats=30)
                    node3.close_and_check()
                    node4.close_and_check()
                    node5.close_and_check()
                except Failure:
                    node3.wait_for_states((state2, state4b, state5), last_state_repeats=30)
                    node4.wait_for_states((state4b, state5), last_state_repeats=30)
                    node3.close_and_check()
                    node4.close_and_check()
                    node5.close_and_check()
        return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0 and node5.returncode == 0

def test_one_normal_one_light_kill_on_reconnect(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seeds = 1)) as node2:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 11}]}'''
        state3 = '''{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        inc1 = form_system((node1, node2))
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node1.disconnect()
        node1.wait_for_states((state1,state2))
        node2.wait_for_form(allowed_states=(state1,))
        node2.wait_for_states((state1,state3))
        node1.reconnect()
        node1.wait_for_states((state1,state2,state1))
        node2.close()
        node1.wait_for_states((state1,state2,state1,state2))
        node1.close_and_check()
    return node1.returncode == 0 and node2.returncode == 0

TEST_CASES = (
    "one_normal",
    "one_light",
    "two_normal",
    "two_normal_disconnect_reconnect",
    "two_normal_multicast",
    "two_normal_then_start_third",
    "two_normal_restart_elected",
    "three_normal_kill_elected",
    "two_light",
    "two_light_multicast",
    "one_normal_one_light",
    "one_normal_one_light_disconnect_reconnect",
    "one_normal_one_light_multicast",
    "one_normal_then_one_light",
    "one_light_then_one_normal",
    "one_normal_one_light_kill_normal",
    "one_normal_one_light_restart_normal",
    "one_normal_one_light_restart_normal_multicast",
    "one_normal_one_light_kill_light",
    "one_normal_one_light_restart_light",
    "one_normal_one_light_kill_light_start_new_light",
    "one_normal_one_light_kill_light_start_new_light_fast",
    "one_normal_two_light_kill_normal",
    "one_normal_two_light_restart_normal",
    "one_normal_two_light_restart_normal_multicast",
    "two_normal_one_light_kill_light",
    "two_normal_one_light_restart_light",
    "two_normal_one_light_kill_normal",
    "two_normal_one_light_kill_elected",
    "two_normal_one_light_restart_elected",
    "two_normal_one_light_kill_both_normal_and_start_new_normals",
    "two_normal_one_light_kill_both_normal_and_start_new_normals_fast",
    "two_normal_two_light",
    "two_normal_two_light_disconnect_reconnect_both_light",
    "two_normal_two_light_multicast",
    "two_normal_two_light_kill_light",
    "two_normal_two_light_restart_light",
    "two_normal_two_light_restart_light_multicast",
    "two_normal_two_light_kill_elected",
    "two_normal_two_light_disconnect_reconnect_elected",
    "two_normal_two_light_disconnect_reconnect_elected_multicast",
    "two_normal_two_light_kill_elected_and_start_new_elected",
    "two_normal_two_light_kill_elected_and_start_new_elected_fast",
    "two_normal_two_light_kill_elected_and_start_new_elected_multicast",
    "two_normal_two_light_kill_elected_and_start_new_elected_multicast_fast",
    "four_normal_four_light_mixedcast",
    "move_lightnode_to_other_new_node",
    "move_lightnode_to_other_new_node_fast",
    "two_normal_one_light_weird_seed",
    "move_lightnode_to_other_system",
    "one_normal_one_light_kill_on_reconnect",
    #"move_lightnode_to_other_system_fast",
)


def parse_arguments():
    parser = argparse.ArgumentParser(description='Component test suite for System Picture')
    parser.add_argument("--exe", help="The test node executable", required=True)
    parser.add_argument("-t",
                        "--test-case",
                        default="all",
                        choices=("all", ) + TEST_CASES,
                        help="Test case to run. The default, 'all' will run them all.")
    parser.add_argument("--only-control",
                        action="store_true",
                        default=False,
                        help="Don't run the main/data channel parts of the tests")
    return parser.parse_args()


def main():
    test_permutations()
    
    args = parse_arguments()

    rmdir("test_output")
    mkdir("test_output")
    olddir = os.getcwd()
    os.chdir("test_output")

    thisdir = os.path.normpath(os.path.dirname(os.path.realpath(__file__)))

    #Set up to use our own test configuration
    os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join(thisdir, "config")

    if args.test_case == "all":
        testcases = TEST_CASES
    else:
        testcases = (args.test_case, )
    failure = False
    try:
        for testcase in testcases:
            log(f"Performing testcase {testcase}")
            mkdir(testcase)
            outputdir = os.getcwd()
            os.chdir(testcase)
            os.environ["LLL_LOGDIR"] = os.path.normpath(os.path.join(os.getcwd(), "lll"))
            func = globals().get("test_" + testcase)
            if func is None:
                log(f" - '{testcase}': missing implementation")
                continue
            result = func(args)
            if result:
                log(f" - '{testcase}': Success")
            else:
                log(f" - '{testcase}': Failure")
                failure = True
            os.chdir(outputdir)
    except KeyboardInterrupt:
        log("Caught Ctrl-C, exiting")
        return 1
    return 1 if failure else 0


sys.exit(main())
