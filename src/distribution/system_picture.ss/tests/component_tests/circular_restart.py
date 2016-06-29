#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013-2014 (http://safirsdkcore.com)
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
import datetime, sys, argparse, subprocess, signal, time, os, shutil, zipfile
from contextlib import closing

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

def zipdir(archive_name, path):
    # ziph is zipfile handle
    with zipfile.ZipFile(archive_name, 'w',
                         compression = zipfile.ZIP_DEFLATED,
                         allowZip64 = True) as zipf:
        for root,_ ,files in os.walk(path):
            for file in files:
                zipf.write(os.path.join(root, file))

def launch_node(master, ownip, nodetype, num_nodes, only_control, masterip = None, revolutions = None, number = None):
    command = ["system_picture_component_test_node",]
    env = os.environ.copy()
    if master:
        command+=("--master",
                  "--revolutions", str(revolutions))
    else:
        command += ("--seed", masterip,
                    "--number", str(number))
        env["SAFIR_INSTANCE"] = str(number+1000)

    if only_control:
        command.append("--only-control")

    command += ("--node-type", str(nodetype),
                "--address", ownip,
                "--number-of-nodes", str(num_nodes))

    if master:
        output = open("master.output.txt","w")
    else:
        output = open("slave_{0:03d}.output.txt".format(number),"w")

    proc = subprocess.Popen(command,
                            stdout = output,
                            stderr = subprocess.STDOUT,
                            env = env,
                            preexec_fn = None if sys.platform == "win32" else os.setsid,
                            creationflags = subprocess.CREATE_NEW_PROCESS_GROUP if sys.platform == "win32" else 0)
    return proc

def stop(proc):
    try:
        if sys.platform == "win32":
            #can't send CTRL_C_EVENT to processes started with subprocess, unfortunately
            #pylint: disable=E1101
            proc.send_signal(signal.CTRL_BREAK_EVENT)
        else:
            proc.terminate()

        proc.wait()

    except OSError:
        pass

class MasterNode(object):
    def __init__(self, args):
        if args.start != 0:
            log("Not launching master node...")
        else:
            log("Launching master node")
            self.node = launch_node(master = True,
                                    ownip = args.own_ip,
                                    nodetype=2,
                                    num_nodes=args.total_nodes,
                                    revolutions = args.revolutions,
                                    only_control = args.only_control)
        self.returncode = None
        self.launched = args.start == 0

    def failed(self):
        poll = self.node.poll()
        return poll != None and poll != 0

    def finished(self):
        return self.node.poll() == 0

    def close(self):
        if self.launched:
            stop(self.node)
            if self.node.returncode != 0:
                log("Master exited with error code", self.node.returncode)
            self.returncode = self.node.returncode


class SlaveNode(object):
    def __init__(self, args, number):
        self.number = number
        log("Launching slave node", number)
        self.node = launch_node(master = False,
                                number = number,
                                num_nodes = args.total_nodes,
                                ownip = args.own_ip,
                                masterip = args.seed_ip,
                                nodetype= number % 2 +1,
                                only_control = args.only_control)
        self.returncode = None

    def failed(self):
        poll = self.node.poll()
        return poll != None and poll != 0

    def finished(self):
        return self.node.poll() == 0

    def close(self):
        stop(self.node)
        if self.node.returncode != 0:
            log("Slave", self.number, "exited with error code", self.node.returncode)
        self.returncode = self.node.returncode

class Slaves(object):
    def __init__(self, args):
        self.slaves = [SlaveNode(args, i) for i in range(args.start, args.start + args.nodes)]
        self.ok = None

    def failed(self):
        for slave in self.slaves:
            if slave.failed():
                log("Slave",slave.number,"has failed!")
                return True
        return False

    def finished(self):
        for slave in self.slaves:
            if slave.finished():
                return True
        return False

    def close(self):
        self.ok = True
        for slave in self.slaves:
            slave.close()
            self.ok = self.ok and slave.returncode == 0

def parse_arguments():
    parser = argparse.ArgumentParser(description='Run a lot of test stub controls and dose_mains')
    parser.add_argument('--start',type=int,
                        default = 0,
                        help = "Node number of first node to start, if 0 the master will be started also")
    parser.add_argument('--nodes', '-n', type=int,
                        default=10,
                        help='Number of nodes to run')
    parser.add_argument('--total-nodes', type=int,
                        default=10,
                        help='Total number of nodes that should run (if running this script on multiple computers)')
    parser.add_argument("--revolutions", type=int,
                        default=1,
                        help="Number of times to restart each node. 0 means run forever")
    parser.add_argument("--own-ip",
                        default="127.0.0.1",
                        help="Ip adress to bind to")
    parser.add_argument("--seed-ip",
                        default="127.0.0.1",
                        help="Ip address of seed node")
    parser.add_argument("--only-control",
                        action="store_true",
                        default=False,
                        help="Start only the control part of the nodes")
    parser.add_argument("--zip-results",
                        action="store_true",
                        default=False,
                        help="Put the results into a zip file called results.zip")

    return parser.parse_args()

def main():
    args = parse_arguments()


    rmdir("circular_restart_output")
    mkdir("circular_restart_output")
    olddir = os.getcwd()
    os.chdir("circular_restart_output")


    #This relies on the fact that we're installed in the bin directory on both linux and windows.
    testdatadir = os.path.normpath(os.path.join(os.path.dirname(os.path.realpath(__file__)),".."))
    if sys.platform != "win32":
        testdatadir = os.path.join(testdatadir, "share", "safir-sdk-core")
    testdatadir = os.path.join(testdatadir, "test_data")

    #Set up to use our own test configuration
    os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join(testdatadir,
                                                            "system_picture",
                                                            "config")

    os.environ["LLL_LOGDIR"] = os.path.normpath(os.path.join(os.getcwd(),"lll"))

    log(args)
    try:
        with closing(MasterNode(args)) as master, closing(Slaves(args)) as slaves:
            try:
                while True:
                    if (master.launched and master.failed()) or slaves.failed():
                        log ("A process failed")
                        break

                    if (not master.launched or master.finished()) and slaves.finished():
                        log ("All processes are finished")
                        break

                    time.sleep(1)
            except KeyboardInterrupt:
                log("Caught Ctrl-C, exiting")
                raise
        if (not master.launched or master.returncode == 0) and slaves.ok:
            log("All processes returned success")
            return 0
        else:
            log("Something went wrong!")
            return 1
    except KeyboardInterrupt:
        pass
    finally:
        if args.zip_results:
            os.chdir(olddir)
            zipdir(archive_name = "result.zip", path = "circular_restart_output")
    return 1

sys.exit(main())
