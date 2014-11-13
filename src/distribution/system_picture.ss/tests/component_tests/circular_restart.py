#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013-2014 (http://safir.sourceforge.net)
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
from __future__ import print_function
import subprocess, os, time, sys, shutil, random, argparse, traceback

class Failure(Exception):
    pass

def log(*args, **kwargs):
    print(*args, **kwargs)
    sys.stdout.flush()

def rmdir(directory):
    if os.path.exists(directory):
        try:
            shutil.rmtree(directory)
        except OSError:
            log ("Failed to remove directory, will retry")
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

def launch_control(number, previous, id, env):
    command = (control_stub,) + ("--name",    "Node_{0:03d}".format(number),
                                  "--control-address", "127.0.0.1:33{0:03d}".format(number),
                                  "--data-address", "127.0.0.1:43{0:03d}".format(number),
                                  "--seed", "127.0.0.1:33{0:03d}".format(previous),
                                  "--force-id", str(id))

    output = open("control_{0:03d}.output.txt".format(number),"w")
    proc = subprocess.Popen(command,
                            stdout = output,
                            stderr = subprocess.STDOUT,
                            env = env)
    return proc

def launch_dose_main(number, previous, id, env):
    command = (dose_main_stub,) + ("--name", "Node_{0:03d}".format(number),
                                   "--data-address", "127.0.0.1:43{0:03d}".format(number),
                                   "--force-id", str(id),
                                   "--suicide-trigger", "Node_{0:03d}".format(previous))

    output = open("main_{0:03d}.output.txt".format(number),"w")
    proc = subprocess.Popen(command,
                            stdout = output,
                            stderr = subprocess.STDOUT,
                            env = env)
    return proc


def launch_node(number, total):
    id = random.getrandbits(63)

    previous = (number - 1) % total
    log("Launching node", number, "with previous set to", previous)
    env = os.environ.copy()
    env["SAFIR_INSTANCE"] = str(number+1000)

    control = launch_control(number, previous, id, env)
    main = launch_dose_main(number, previous, id, env)
    return (number,control,main)


def stop(proc):
    try:
        if proc.poll() is not None:
            return
        proc.terminate()
        for i in range(300): #30 seconds
            if proc.poll() is not None:
                return
            time.sleep(0.1)
        proc.kill()
        proc.wait()
    except ProcessLookupError:
        pass

def stop_node(i, control, main):
    stop(control)
    stop(main)
    if control.returncode != 0:
        log ("CONTROL RETURN CODE",control.returncode)
        raise Failure
    if main.returncode != 0:
        log ("DOSE_MAIN RETURN CODE",main.returncode)
        raise Failure

SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
if SAFIR_RUNTIME is None or not os.path.isdir(SAFIR_RUNTIME):
    log("SAFIR_RUNTIME must be set")
    sys.exit(1)
SAFIR_RUNTIME = os.path.normpath(SAFIR_RUNTIME)

control_stub = os.path.join(SAFIR_RUNTIME,"bin","control_stub")
dose_main_stub = os.path.join(SAFIR_RUNTIME,"bin","dose_main_stub")

parser = argparse.ArgumentParser(description='Run a lot of control_stub')
parser.add_argument('--start',type=int,
                    default = 0,
                    help = "Node number of first node to start")
parser.add_argument('--nodes', '-n', type=int,
                    default=10,
                    help='Number of controls to run')
parser.add_argument('--total-nodes', type=int,
                    default=10,
                    help='Total number of nodes that should run (if running this script on multiple computers)')
parser.add_argument("--revolutions", type=int,
                    default=3,
                    help="Number of times to restart each node. 0 means run forever")
args = parser.parse_args()

rmdir("circular_restart_output")
mkdir("circular_restart_output")
os.chdir("circular_restart_output")

nodes = list()

try:
    log("Starting some nodes")
    for i in range (args.start, args.start + args.nodes):
        nodes.append(launch_node(i,args.total_nodes))

    log("Sleeping for a while to let the nodes start up")
    time.sleep(10)

    #we need to kill the first node manually, after which the circle will start running...
    if args.start == 0:
        log("Stopping node 0")
        stop_node(*nodes[0])
    expected = args.start
    revolution = 0
    log("Expected is", expected)
    time.sleep(10)

    while True:
        time.sleep(1.0)
        living = list()
        dead = list()
        for i, control, main in nodes:
            if control.poll() is None and main.poll() is None:
                living.append((i,control,main))
            else:
                dead.append(i)
                log ("Node", i, "is stopping")
                if i != expected:
                    log ("Unexpected node died!")
                    time.sleep(1000000)
                if expected == args.start:
                    revolution += 1
                expected = args.start + (expected + 1) % args.nodes
                log("Next expected is", expected)
                stop_node(i, control, main)

        nodes = living

        for i in dead:
            if args.revolutions == 0 or revolution < args.revolutions:
                log ("Restarting node", i)
                nodes.append(launch_node(i,args.total_nodes))
            else:
                log("We've done our revolutions, not restarting node", i)

        if len(nodes) == 0:
            log("No nodes running, exiting")
            break;

except KeyboardInterrupt:
    pass
except:
    traceback.print_exc()

log ("Killing", len(nodes), "nodes")
for i, control, main in nodes:
    try:
        stop(main)
    except ProcessLookupError:
        pass
    try:
        stop(control)
    except ProcessLookupError:
        pass
