#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
#
# Created by: Lars Hagstrom (lars@foldspace.nu)
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
import subprocess, os, time, sys, shutil, re
from threading import Thread
import argparse
try:
    # 3.x name
    from queue import Queue, Empty
except ImportError:
    # 2.x name
    from Queue import Queue, Empty

def log(*args, **kwargs):
    print(*args, **kwargs)
    sys.stdout.flush()

def enqueue_output(out, queue):
    while True:
        line = out.readline()
        if not line:
            break
        queue.put(line.rstrip("\n\r"))
    out.close()

class LllProc:
    def __init__(self, cmd):
        self.proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,universal_newlines=True)
        self.queue = Queue()
        thread = Thread(target=enqueue_output, args=(self.proc.stdout, self.queue))
        thread.daemon = True #Thread dies with program, no need to stop explicitly
        thread.start()

    def wait_output(self, regex):
        pattern = re.compile(regex)
        now = time.time()
        future = now + 10
        while time.time() < future:
            try:
                d = self.queue.get(True,0.1)
                log(d)
                if pattern.match(d):
                    return True
                else:
                    return False
            except Empty:
                pass
        return False

    #returns the contents of the logfile so far.
    def logfile(self):
        if not os.path.isfile(logfilename(self.proc)):
            return None
        with open(logfilename(self.proc)) as logfile:
            return logfile.read()

    def kill(self):
        self.proc.kill()
        self.proc.wait()

if sys.platform == "win32":
    temp = os.environ.get("TEMP")
    if temp is None:
        temp = os.environ.get("TMP")
    if temp is None:
        log ("Failed to find temp dir!")
        sys.exit(1)
    logdir = os.path.join(temp, "safir-sdk-core", "log")
else:
    logdir = os.path.join("/", "tmp", "safir-sdk-core", "log")

log ("Logdir: ", logdir)

parser = argparse.ArgumentParser("test script")
parser.add_argument("--test-exe", required=True)

arguments = parser.parse_args()

def logfilename(proc):
    if sys.platform == "win32":
        fn = "check_correct_public_headers.exe-" + str(proc.pid) + ".txt"
    else:
        fn = "check_correct_public_headers-" + str(proc.pid) + ".txt"
    return os.path.join(logdir,fn)


p = LllProc(arguments.test_exe)
res = p.wait_output(".*START")
if not res:
    log("got unexpected output")
    sys.exit(1)
p.kill()
logfile = p.logfile()
if logfile is None:
    log("Logfile empty!")
    sys.exit(1)
elif not re.match(r"\[[0-9:.]*\] START\n",logfile):
    log("Failed to find expected output in logfile:")
    log(logfile)
    sys.exit(1)
else:
    log("success")
    sys.exit(0)
