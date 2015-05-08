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
import subprocess, os, time, sys, shutil, random, argparse, traceback, platform, datetime, signal

class Failure(Exception):
    pass

def log(*args, **kwargs):
    print(datetime.datetime.now().isoformat(), ":", *args, **kwargs)
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

def launch_control(number, previous, id, env, ownip, seedip):
    command = ("sp_test_ctrl",) + ("--name",    "Node_{0:03d}".format(number),
                                   "--control-address", ownip + ":33{0:03d}".format(number),
                                   "--data-address", ownip + ":43{0:03d}".format(number),
                                   "--force-id", str(id),
                                   "--check-incarnation")
    if previous is not None:
        command += ("--seed", seedip + ":33999")

    output = open("control_{0:03d}.output.txt".format(number),"w")
    proc = subprocess.Popen(command,
                            stdout = output,
                            stderr = subprocess.STDOUT,
                            env = env,
                            creationflags = subprocess.CREATE_NEW_PROCESS_GROUP if sys.platform == "win32" else 0)
    return proc

def launch_dose_main(number, previous, id, env, ownip):
    command = ("sp_test_dm",) + ("--name", "Node_{0:03d}".format(number),
                                 "--data-address", ownip + ":43{0:03d}".format(number),
                                 "--force-id", str(id))
    if previous is not None:
        command += ("--suicide-trigger", "Node_{0:03d}".format(previous))

    output = open("main_{0:03d}.output.txt".format(number),"w")
    proc = subprocess.Popen(command,
                            stdout = output,
                            stderr = subprocess.STDOUT,
                            env = env,
                            creationflags = subprocess.CREATE_NEW_PROCESS_GROUP if sys.platform == "win32" else 0)
    return proc


def launch_node(number, args):
    id = random.getrandbits(63)

    previous = (number - 1) % args.total_nodes
    seed = args.seed_ip
    log("Launching node", number, "with previous set to", previous)
    env = os.environ.copy()
    env["SAFIR_INSTANCE"] = str(number+1000)

    control = launch_control(number, previous, id, env, args.own_ip, seed)
    main = launch_dose_main(number, previous, id, env, args.own_ip)
    return (number,control,main)

def launch_seeder(args):
    id = random.getrandbits(63)

    log("Launching seeder node")

    control = launch_control(999, previous = None, id = id, env = None, ownip = args.own_ip, seedip = None)
    main = launch_dose_main(999, previous = None, id = id, env = None, ownip = args.own_ip)
    return (control,main)


def stop(proc):
    try:
        if sys.platform == "win32":
            #can't send CTRL_C_EVENT to processes started with subprocess, unfortunately
            proc.send_signal(signal.CTRL_BREAK_EVENT)
        else:
            proc.terminate()

        proc.wait() #comment this line to get procs killed after 30s
        for i in range(300): #30 seconds
            if proc.poll() is not None:
                return
            time.sleep(0.1)
        proc.kill()
        proc.wait()
    except OSError:
        pass

def stop_node(i, control, main):
    log(" - Stopping control")
    stop(control)
    log(" - Stopping main")
    stop(main)
    if control.returncode != 0:
        log ("CONTROL", i, "RETURN CODE",control.returncode)
        raise Failure
    if main.returncode != 0:
        log ("DOSE_MAIN", i, "RETURN CODE",main.returncode)
        raise Failure

parser = argparse.ArgumentParser(description='Run a lot of test stub controls and dose_mains')
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
                    default=0,
                    help="Number of times to restart each node. 0 means run forever")
parser.add_argument("--own-ip",
                    default="127.0.0.1",
                    help="Ip adress to bind to")
parser.add_argument("--seed-ip",
                    default="127.0.0.1",
                    help="Ip address of seed node")

args = parser.parse_args()

rmdir("circular_restart_output")
mkdir("circular_restart_output")
os.chdir("circular_restart_output")


#This relies on the fact that we're installed in the bin directory on both linux and windows.
testdatadir = os.path.normpath(os.path.join(os.path.dirname(os.path.realpath(__file__)),"..","share", "safir-sdk-core", "test_data"))

#Set up to use our own test configuration
os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join(testdatadir,
                                                        "system_picture",
                                                        "config")

os.environ["LLL_LOGDIR"] = os.path.normpath(os.path.join(os.getcwd(),"lll"))

subprocess.call(("safir_show_config","--logging"))
if sys.platform == "win32":
    subprocess.call(("set"),shell=True)

seeder = launch_seeder(args) if args.start == 0 else None

nodes = list()

success = False

try:
    log("Starting some nodes")
    for i in range (args.start, args.start + args.nodes):
        nodes.append(launch_node(i,args))

    #we need to kill the first node manually, after which the circle will start running...
    if args.start == 0:
        log("Waiting for nodes to start up")
        while True:
            log("Counting nodes")
            output = subprocess.check_output(("system_picture_listener","--one")).decode("utf-8").splitlines()
            count = 0
            for line in output:
                if " Node_" in line and " D  Node_" not in line:
                    count += 1
            log(" found", count,"nodes")
            if count == args.total_nodes + 1:
                break

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
                log ("Node", i, "is stopping")
                if i != expected:
                    log ("Unexpected node died!")
                    expected = -1
                    if control.poll() is not None:
                        log("  control died with exit code", control.returncode)
                    if main.poll() is not None:
                        log("  main died with exit code", main.returncode)
                else:
                    dead.append(i)
                    if expected == args.start:
                        revolution += 1
                    expected = args.start + (expected + 1) % args.nodes
                    log("Next expected is", expected)
                    stop_node(i, control, main)

        nodes = living

        for i in dead:
            if args.revolutions == 0 or revolution < args.revolutions:
                log ("Restarting node",i, "in 9 seconds")
                time.sleep(9)
                nodes.append(launch_node(i,args))
            else:
                log("We've done our revolutions, not restarting node", i)
                success = True

        if len(nodes) == 0:
            log("No nodes running, exiting")
            break;
except KeyboardInterrupt:
    pass
except:
    traceback.print_exc()

if len(nodes) > 0:
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
if seeder is not None:
    (control,main) = seeder
    try:
        stop(main)
    except ProcessLookupError:
        pass
    try:
        stop(control)
    except ProcessLookupError:
        pass

if success:
    log("Test appears to have been successful")
else:
    log("Test failed")
sys.exit(0 if success else 1)
