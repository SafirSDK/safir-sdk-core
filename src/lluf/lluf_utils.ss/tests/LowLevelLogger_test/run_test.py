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


def rmdir(directory):
    if os.path.exists(directory):
        try:
            shutil.rmtree(directory)
        except OSError:
            log("Failed to remove directory",directory,", will retry")
            time.sleep(0.2)
            shutil.rmtree(directory)

def enqueue_output(out, queue):
    while True:
        line = out.readline()
        if not line:
            break
        queue.put(line.rstrip("\n\r"))
    out.close()

class LllProc:
    def __init__(self, wait_for_output = True):
        self.proc = subprocess.Popen(lll_test, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,universal_newlines=True)
        self.queue = Queue()
        thread = Thread(target=enqueue_output, args=(self.proc.stdout, self.queue))
        thread.daemon = True #Thread dies with program, no need to stop explicitly
        thread.start()
        if wait_for_output and not self.wait_output("^Logging at"):
            raise Exception("failed to launch lll_test properly")

    def wait_output(self, regex):
        pattern = re.compile(regex)
        now = time.time()
        future = now + 10
        while time.time() < future:
            try:
                if pattern.match(self.queue.get(True,0.1)):
                    return True
            except Empty:
                pass
        return False

    #returns any output that hasn't already been dequeued
    def output(self):
        data = list();
        try:
            while True:
                data.append(self.queue.get_nowait())
        except Empty:
            pass
        return "\n".join(data)

    #returns the contents of the logfile so far.
    def logfile(self):
        with open(logfilename(self.proc)) as logfile:
            return logfile.read()

    #returns true if the logfile matches
    def logfile_contains(self,regex):
        pattern = re.compile(regex, flags = re.MULTILINE)
        return pattern.search(self.logfile())

    def kill(self):
        self.proc.kill()
        self.proc.wait()

def call_logger_control(args):
    cmd = (logger_control,) + args
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,universal_newlines=True)
    return proc.communicate()[0]


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
parser.add_argument("--config-dir", required=True)

arguments = parser.parse_args()

lll_test = arguments.test_exe

def logfilename(proc):
    if sys.platform == "win32":
        fn = "lll_test.exe-" + str(proc.pid) + ".txt"
    else:
        fn = "lll_test-" + str(proc.pid) + ".txt"
    return os.path.join(logdir,fn)


configs_dir = arguments.config_dir
if not os.path.isdir(configs_dir):
    log ("arg is not a directory")
    sys.exit(1)

os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join (configs_dir,"level_0")
log ("testing level 0")
log ("Run the program that logs")
p = LllProc()

p.kill()
log ("check that we dont have a log file")
try:
    data = p.logfile()
    log("have unexpected logfile")
    sys.exit(1)
except:
    pass


os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join (configs_dir,"level_1")

log ("testing level 1")
log ("Run the program that logs")
p = LllProc()

timestamp_regex = r"\[[0-9\.:]*\] "
p.kill()
log ("check log file")
if not p.logfile_contains(timestamp_regex + r"1234567890"):
    log("could not find expected output")
    sys.exit(1)

if p.logfile_contains("Hello") or p.logfile_contains("Goodbye"):
    log("found unexpected output")
    sys.exit(1)


os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join (configs_dir,"level_9")

log ("testing level 9")
log ("Run the program that logs")
p = LllProc()

p.kill()
log ("check log file")
if not p.logfile_contains(timestamp_regex + r"1234567890"):
    log("could not find expected output 1")
    sys.exit(1)

if not p.logfile_contains(timestamp_regex + r"Hello, World!"):
    log("could not find expected output 2")
    sys.exit(1)

if not p.logfile_contains(timestamp_regex + r"Goodbye cruel world!"):
    log("could not find expected output 3")
    sys.exit(1)



os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join (configs_dir,"no_timestamps")

log ("testing disabled timestamps")
log ("Run the program that logs")
p = LllProc()

p.kill()
log ("check log file")
if p.logfile_contains(timestamp_regex):
    log("found unexpected output")
    log(p.logfile())
    sys.exit(1)

os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join (configs_dir,"no_flush")

log ("testing level 1")
log ("Run the program that logs")
p = LllProc()

p.kill()
log ("check log file")
data = p.logfile()
if len(data) != 0:
    log("logfile should be empty")
    log(data)
    sys.exit(1)



log("success")
sys.exit(0)
