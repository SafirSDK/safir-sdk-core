#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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

def rmdir(directory):
    if os.path.exists(directory):
        try:
            shutil.rmtree(directory)
        except OSError:
            print ("Failed to remove directory, will retry")
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

def launch_control(number, multicast, debug):
    command = (safir_control,) + ("--name",    "Node_{0:03d}".format(number), 
                                  "--address", "127.0.0.1:33{0:03d}".format(number),
                                  "--seed",    "127.0.0.1:33000")

    if multicast:
        command = command + ("-m", "224.100.123.123:20000")

    if debug:
        command = ("gdb", "--batch" , "--ex", "run", "--ex", "bt", "--args") + command
            
    print (command)
    e = os.environ.copy()
    e["SAFIR_INSTANCE"] = str(number+1000)
    output = open("Node_{0:03d}.output.txt".format(number),"w")
    c = subprocess.Popen(command,
                         stdout = output,
                         stderr = subprocess.STDOUT,
                         stdin = subprocess.PIPE if debug else None,
                         env = e)
    return c


SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
if SAFIR_RUNTIME is None or not os.path.isdir(SAFIR_RUNTIME):
    print("SAFIR_RUNTIME must be set")
    sys.exit(1)
SAFIR_RUNTIME = os.path.normpath(SAFIR_RUNTIME)

safir_control = os.path.join(SAFIR_RUNTIME,"bin","safir_control")

parser = argparse.ArgumentParser(description='Run a lot of safir_control')
parser.add_argument('--number', '-n', dest="num", action='store', type=int,
                    default=10,
                    help='Number of controls to run')
parser.add_argument('--terminate', '-t', dest='terminate', action='store_true',
                    default=False,
                    help='randomly terminate safir_controls every 10 seconds')
parser.add_argument('--skip-0', '-s', dest='skip', action='store_true',
                    default=False,
                    help='Dont start Node 0')
parser.add_argument('--singlecast', '-S', dest='multicast', action='store_false',
                    default=True,
                    help='Use only singlecast communication')
parser.add_argument('--gdb', '-d', dest='debug', action='store_true',
                    default=False,
                    help='Launch in GDB batch mode (needs manual cleanup after run!).')


args = parser.parse_args()

rmdir("start_many_output")
mkdir("start_many_output")
os.chdir("start_many_output")

controls = list()

try:
    for i in range (1 if args.skip else 0 ,args.num):
        controls.append((i,launch_control(i, args.multicast, args.debug)))

    killtime = time.time()
    while True:
        time.sleep(1.0)
        living_controls = list()
        for i, control in controls:
            if control.poll() is None:
                living_controls.append((i,control))
            else:
                print ("Control",i,"exited with return code", control.returncode)
        controls = living_controls

        if len(controls) != args.num - (1 if args.skip else 0):
            print ("Unexpected number of living controls:", len(controls))

        if not args.terminate:
            continue

        if killtime + 30 < time.time():
            killtime = time.time()
            #choose one to stop
            index = random.randint(1,len(controls)-1)
            i, control = controls.pop(index)
            print ("Stopping", i)
            control.terminate()
            control.wait()
            if control.returncode != 0:
                print ("RETURN CODE",control.returncode)
            controls.append((i,launch_control(i, args.multicast, args.debug)))
except KeyboardInterrupt:
    pass
except:
    traceback.print_exc()
print ("Killing all controls")
for i, control in controls:
    try:
        control.kill()
        control.wait()
    except ProcessLookupError:
        pass

