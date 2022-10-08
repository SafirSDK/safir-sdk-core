#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013-2014,2022 (http://safirsdkcore.com)
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
import datetime, sys, argparse, subprocess, signal, time, os, shutil, zipfile
from contextlib import closing
from queue import Queue, Empty
from threading import Thread
import json
import re

class Failure(Exception):
    pass

#pylint: disable=line-too-long

def enqueue_output(out, queue, label):
    while True:
        line = out.readline()
        if not line:
            break
        queue.put((label,line.rstrip("\n\r")))
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


def zipdir(archive_name, path):
    # ziph is zipfile handle
    with zipfile.ZipFile(archive_name, 'w', compression=zipfile.ZIP_DEFLATED, allowZip64=True) as zipf:
        for root, _, files in os.walk(path):
            for file in files:
                zipf.write(os.path.join(root, file))


class Node():
    def __init__(self, args, node_id, light_node, multicast, seed = None):
        self.args = args
        self.node_id = node_id
        self.light_node = light_node
        self.multicast = multicast
        self.returncode = None
        self.seed = seed
        command = [ args.exe,  "--id", str(node_id) ]
        if self.multicast:
            command.append("--use-multicast")
        if self.light_node:
            command.append("--light")
        if self.seed is not None:
            command += ("--seed", str(seed))
        #if args.address is not None:
        #    command += ("--address", address)
        #if seed is not None:
        #    command += ("--seed", seed)
        env = os.environ.copy()

        env["SAFIR_INSTANCE"] = str(self.node_id + 1000)

        self.output_file = open("node_{0:03d}.output.txt".format(node_id), "w")
        self.output_list = []
        self.states = []
        log(f"  Launching {' '.join(command)}")
        self.proc = subprocess.Popen(command,
                                     stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE,
                                     env=env,
                                     preexec_fn=None if sys.platform == "win32" else os.setsid,
                                     creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if sys.platform == "win32" else 0,
                                     universal_newlines=True)
        self.queue = Queue()
        self.stdout_thread = Thread(target=enqueue_output, args=(self.proc.stdout, self.queue, "out"))
        self.stderr_thread = Thread(target=enqueue_output, args=(self.proc.stderr, self.queue, "err"))
        self.stdout_thread.start()
        self.stderr_thread.start()

    def output(self):
        try:
            while True:
                (label, data) = self.queue.get_nowait()
                if label == "out":
                    self.states.append(json.loads(re.sub(r"\[[0-9:\.]*\] ", "", data)))
                self.output_file.write(data)
                self.output_file.write("\n")
                self.output_file.flush()
                self.output_list.append(data)
        except Empty:
            pass
        return "\n".join(self.output_list) + "\n"

    @staticmethod
    def __compare_states(state,expected):
        state.pop("election_id", None)
        return state == expected

    def wait_for_states(self, expected_states, last_state_repeats = 3):
        """
        Wait until all states have been seen, in order.
        expected_states can be a string or a collection of strings.
        """
        unchecked_states = self.states.copy()

        #make expected_states into a list we can manipulate
        if isinstance(expected_states,str):
            expected_states = [expected_states,]
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
                log(f"Node {self.node_id} expected state \n{expected}, but got \n{state}")
                raise Failure("Got unexpected state")

            while True:
                (label, data) = self.queue.get()
                self.output_file.write(data)
                self.output_file.write("\n")
                self.output_file.flush()
                self.output_list.append(data)
                if label == "out":
                    state = json.loads(re.sub(r"\[[0-9:\.]*\] ", "", data))
                    self.states.append(state)
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
                    log(f"Node {self.node_id} expected state \n{expected}, but got \n{state}")
                    raise Failure("Got unexpected state")

    def failed(self):
        if self.returncode is not None:
            return self.returncode != 0
        poll = self.proc.poll()
        return poll not in (None, 0)

    #def finished(self):
     #   return self.node.poll() == 0

    def close(self):
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
        self.output()
        self.output_file.close()
        if self.proc.returncode != 0:
            log(f"Node {self.node_id} exited with error code", self.proc.returncode)
        self.returncode = self.proc.returncode


def test_single_normal(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node:
        node.wait_for_states('{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1}]}')
    return node.returncode == 0


def test_single_light(args):
    with closing(Node(args,1,light_node = True, multicast = False)) as node:
        node.wait_for_states('{"elected_id": 1, "is_detached": true, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 11}]}')
    return node.returncode == 0

def test_two_normal(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2:
        node1.wait_for_states('''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                    {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}''')
        node2.wait_for_states('''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                    {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}''')
    return node1.returncode == 0 and node2.returncode == 0

def test_two_normal_restart_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2a:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                    {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1}]}'''
        state3 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''

        node1.wait_for_states(state1)
        node2a.wait_for_states(state1)
        node2a.close()
        node1.wait_for_states((state1,state2))
        with closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2b:
            node2b.wait_for_states(state3)
            node1.wait_for_states((state1,state2))

    return node1.returncode == 0 and node2a.returncode == 0 and node2b.returncode == 0

def test_two_normal_then_start_third(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2:

        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''
        state2 = '''{"elected_id": 3, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1}]}'''
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)

        with closing(Node(args,3,light_node = False, multicast = False, seed = 2)) as node3:
            node1.wait_for_states((state1,state2))
            node2.wait_for_states((state1,state2))
            node3.wait_for_states(state2)
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0

def test_three_normal_kill_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2,\
         closing(Node(args,3,light_node = False, multicast = False, seed = 1)) as node3:

        state1 = '''{"elected_id": 3, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 1}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 1}]}'''
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node3.close()
        node1.wait_for_states((state1,state2))
        node2.wait_for_states((state1,state2))
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0

def test_two_light(args):
    with closing(Node(args,1,light_node = True, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False)) as node2:
        node1.wait_for_states('''{"elected_id": 1, "is_detached": true, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 11}]}''')
        node2.wait_for_states('''{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}''')
    return node1.returncode == 0 and node2.returncode == 0

def test_one_normal_one_light(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seed = 1)) as node2:
        node1.wait_for_states('''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                    {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}''')
        node2.wait_for_states('''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                    {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}''')
    return node1.returncode == 0 and node2.returncode == 0

def test_one_normal_then_one_light(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1:
        node1.wait_for_states('{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1}]}')
        with closing(Node(args,2,light_node = True, multicast = False, seed = 1)) as node2:
            node1.wait_for_states(('{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1}]}',
                                   '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                       {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''))
            node2.wait_for_states('''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                       {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}''')
    return node1.returncode == 0 and node2.returncode == 0

def test_one_light_then_one_normal(args):
    with closing(Node(args,1,light_node = True, multicast = False, seed = 2)) as node1:
        node1.wait_for_states('{"elected_id": 1, "is_detached": true, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 11}]}')
        with closing(Node(args,2,light_node = False, multicast = False)) as node2:
            node1.wait_for_states(('{"elected_id": 1, "is_detached": true, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 11}]}',
                                   '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 11},
                                                                                        {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}'''))
            node2.wait_for_states('''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 11},
                                                                                       {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1}]}''')
    return node1.returncode == 0 and node2.returncode == 0

def test_one_normal_one_light_kill_normal(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seed = 1)) as node2:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node1.close()
        node2.wait_for_states((state1,
                               '{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'))

    return node1.returncode == 0 and node2.returncode == 0

def test_one_normal_one_light_restart_normal(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1a,\
         closing(Node(args,2,light_node = True, multicast = False, seed = 1)) as node2:
        node2.wait_for_states('''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                    {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}''')
        node1a.close()
        node2.wait_for_states(('''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                    {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}''',
                               '{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'))
        with closing(Node(args,1,light_node = False, multicast = False)) as node1b:
            node2.wait_for_states(('''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                        {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}''',
                                   '{"elected_id": 2, "is_detached": true, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}',
                                   '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                        {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''))
            node1b.wait_for_states('''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                        {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}''')

    return node1a.returncode == 0 and node1b.returncode == 0 and node2.returncode == 0


def test_one_normal_one_light_kill_light(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seed = 1)) as node2:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node2.close()
        node1.wait_for_states((state1,
                               '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                                    {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 11}]}'''))
    return node1.returncode == 0 and node2.returncode == 0

def test_one_normal_one_light_restart_light(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = True, multicast = False, seed = 1)) as node2a:
        state1 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 11}]}'''

        node1.wait_for_states(state1)
        node2a.wait_for_states(state1)
        node2a.close()
        node1.wait_for_states((state1, state2))

        with closing(Node(args,2,light_node = True, multicast = False, seed = 1)) as node2b:
            node1.wait_for_states((state1, state2, state1))
            node2b.wait_for_states(state1)
    return node1.returncode == 0 and node2a.returncode == 0 and node2b.returncode == 0

def test_two_normal_one_light_kill_light(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seed = 1)) as node3:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11}]}'''
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node3.close()
        node1.wait_for_states((state1,state2))
        node2.wait_for_states((state1,state2))

    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0

def test_two_normal_one_light_restart_light(args):
    with closing(Node(args,1,light_node = False, multicast = False)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seed = 1)) as node3a:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11}]}'''
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3a.wait_for_states(state1)
        node3a.close()
        node1.wait_for_states((state1,state2))
        node2.wait_for_states((state1,state2))
        with closing(Node(args,3,light_node = True, multicast = False, seed = 1)) as node3b:
            node1.wait_for_states((state1,state2,state1))
            node2.wait_for_states((state1,state2,state1))
            node3b.wait_for_states(state1)

    return node1.returncode == 0 and node2.returncode == 0 and node3a.returncode == 0 and node3b.returncode == 0

def test_two_normal_one_light_kill_normal(args):
    with closing(Node(args,1,light_node = False, multicast = False, seed = 2)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seed = 2)) as node3:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": true, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node1.close()
        node2.wait_for_states((state1,state2))
        node3.wait_for_states((state1,state2))
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0

def test_two_normal_one_light_kill_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False, seed = 2)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2,\
         closing(Node(args,3,light_node = True, multicast = False, seed = 2)) as node3:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node2.close()
        node1.wait_for_states((state1,state2))
        node3.wait_for_states((state1,state2))
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0

def test_two_normal_one_light_restart_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False, seed = 2)) as node1,\
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2a,\
         closing(Node(args,3,light_node = True, multicast = False, seed = 2)) as node3:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11}]}'''
        state3a = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": true, "id": 1, "node_type_id": 1},
                                                                       {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                       {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11}]}'''
        state3b = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                       {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11}]}'''
        node1.wait_for_states(state1)
        node2a.wait_for_states(state1)
        node3.wait_for_states(state1)
        node2a.close()
        node1.wait_for_states((state1,state2))
        node3.wait_for_states((state1,state2))
        with closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2b:
            log("This testcase produces some spurious output since it checks for two possible outcomes")
            log("So ignore any lines below talking about incorrect states unless the test fails.")
            try:
                node2b.wait_for_states(state3a)
            except Failure:
                node2b.wait_for_states(state3b)
            node1.wait_for_states((state1,state2), last_state_repeats = 20)
            node3.wait_for_states((state1,state2), last_state_repeats = 20)
    return node1.returncode == 0 and node2a.returncode == 0 and node2b.returncode == 0 and node3.returncode == 0

def test_two_normal_two_light(args):
    with closing(Node(args,1,light_node = False, multicast = False, seed = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seed = 1)) as node3, \
         closing(Node(args,4,light_node = True, multicast = False, seed = 1)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''

        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0

def test_two_normal_two_light_kill_light(args):
    with closing(Node(args,1,light_node = False, multicast = False, seed = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seed = 1)) as node3, \
         closing(Node(args,4,light_node = True, multicast = False, seed = 1)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''

        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node3.close()
        node1.wait_for_states((state1,state2))
        node2.wait_for_states((state1,state2))
        node4.wait_for_states((state1,state2))

    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0

def test_two_normal_two_light_restart_light(args):
    with closing(Node(args,1,light_node = False, multicast = False, seed = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seed = 1)) as node3a, \
         closing(Node(args,4,light_node = True, multicast = False, seed = 1)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''

        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3a.wait_for_states(state1)
        node4.wait_for_states(state1)
        node3a.close()
        node1.wait_for_states((state1,state2))
        node2.wait_for_states((state1,state2))
        node4.wait_for_states((state1,state2))
        with closing(Node(args,3,light_node = True, multicast = False, seed = 1)) as node3b:
            node3b.wait_for_states(state1)
            node1.wait_for_states((state1,state2,state1),last_state_repeats = 10)
            node2.wait_for_states((state1,state2,state1),last_state_repeats = 10)
            node4.wait_for_states((state1,state2,state1),last_state_repeats = 10)
    return node1.returncode == 0 and node2.returncode == 0 and node3a.returncode == 0 \
        and node3b.returncode == 0 and node4.returncode == 0

def test_two_normal_two_light_kill_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False, seed = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2, \
         closing(Node(args,3,light_node = True, multicast = False, seed = 1)) as node3, \
         closing(Node(args,4,light_node = True, multicast = False, seed = 1)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''

        node1.wait_for_states(state1)
        node2.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node2.close()
        node1.wait_for_states((state1,state2))
        node3.wait_for_states((state1,state2))
        node4.wait_for_states((state1,state2))
    return node1.returncode == 0 and node2.returncode == 0 and node3.returncode == 0 and node4.returncode == 0

def test_two_normal_two_light_restart_elected(args):
    with closing(Node(args,1,light_node = False, multicast = False, seed = 2)) as node1, \
         closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2a, \
         closing(Node(args,3,light_node = True, multicast = False, seed = 2)) as node3, \
         closing(Node(args,4,light_node = True, multicast = False, seed = 2)) as node4:
        state1 = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state2 = '''{"elected_id": 1, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": false, "id": 1, "node_type_id": 1},
                                                                      {"name": "node_002", "is_dead": true, "id": 2, "node_type_id": 1},
                                                                      {"name": "node_003", "is_dead": false, "id": 3, "node_type_id": 11},
                                                                      {"name": "node_004", "is_dead": false, "id": 4, "node_type_id": 11}]}'''
        state3a = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_001", "is_dead": true, "id": 1, "node_type_id": 1},
                                                                       {"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                       {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11},
                                                                       {"name": "node_004", "is_dead": true, "id": 4, "node_type_id": 11}]}'''
        state3b = '''{"elected_id": 2, "is_detached": false, "nodes": [{"name": "node_002", "is_dead": false, "id": 2, "node_type_id": 1},
                                                                       {"name": "node_003", "is_dead": true, "id": 3, "node_type_id": 11},
                                                                       {"name": "node_004", "is_dead": true, "id": 4, "node_type_id": 11}]}'''

        node1.wait_for_states(state1)
        node2a.wait_for_states(state1)
        node3.wait_for_states(state1)
        node4.wait_for_states(state1)
        node2a.close()
        node1.wait_for_states((state1,state2))
        node3.wait_for_states((state1,state2))
        node4.wait_for_states((state1,state2))
        with closing(Node(args,2,light_node = False, multicast = False, seed = 1)) as node2b:
            #node2b will produce one of two possible states, it is a bit random, but both are correct
            log("This testcase produces some spurious output since it checks for two possible outcomes")
            log("So ignore any lines below talking about incorrect states unless the test fails.")
            try:
                node2b.wait_for_states(state3a, last_state_repeats = 10)
            except Failure:
                node2b.wait_for_states(state3b, last_state_repeats = 10)
            node1.wait_for_states((state1,state2), last_state_repeats = 40)
            node3.wait_for_states((state1,state2), last_state_repeats = 40)
            node4.wait_for_states((state1,state2), last_state_repeats = 40)
    return node1.returncode == 0 and node2a.returncode == 0 and node2b.returncode == 0 \
        and node3.returncode == 0 and node4.returncode == 0


TEST_CASES = ("single_normal",
              "single_light",
              "two_normal",
              "two_normal_then_start_third",
              "two_normal_restart_elected",
              "three_normal_kill_elected",
              "two_light",
              "one_normal_one_light",
              "one_normal_then_one_light",
              "one_light_then_one_normal",
              "one_normal_one_light_kill_normal",
              "one_normal_one_light_restart_normal",
              "one_normal_one_light_kill_light",
              "one_normal_one_light_restart_light",
              "two_normal_one_light_kill_light",
              "two_normal_one_light_restart_light",
              "two_normal_one_light_kill_normal",
              "two_normal_one_light_kill_elected",
              "two_normal_one_light_restart_elected",
              "two_normal_two_light",
              "two_normal_two_light_kill_light",
              "two_normal_two_light_restart_light",
              "two_normal_two_light_kill_elected",
              "two_normal_two_light_restart_elected",
              #multicast!
              #quick detach reattach!
              )

def parse_arguments():
    parser = argparse.ArgumentParser(description='Component test suite for System Picture')
    parser.add_argument("--exe", help="The test node executable",required=True)
    parser.add_argument("-t", "--test-case", default="all", choices=("all",) + TEST_CASES,
                        help="Test case to run. The default, 'all' will run them all.")
    parser.add_argument("--zip-results",
                        action="store_true",
                        default=False,
                        help="Put the results into a zip file called results.zip")

    return parser.parse_args()

def main():
    args = parse_arguments()

    rmdir("output")
    mkdir("output")
    olddir = os.getcwd()
    os.chdir("output")

    thisdir = os.path.normpath(os.path.dirname(os.path.realpath(__file__)))

    #Set up to use our own test configuration
    os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join(thisdir, "config")

    if args.test_case == "all":
        testcases = TEST_CASES
    else:
        testcases = (args.test_case,)
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
    finally:
        if args.zip_results:
            os.chdir(olddir)
            zipdir(archive_name="output.zip", path="output")
    return 1 if failure else 0


sys.exit(main())
