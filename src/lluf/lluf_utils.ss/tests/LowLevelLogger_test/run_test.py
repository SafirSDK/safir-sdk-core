#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011 (http://www.safirsdk.com)
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
    
def rmdir(directory):
    if os.path.exists(directory):
        try:
            shutil.rmtree(directory)
        except OSError:
            print("Failed to remove directory, will retry")
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
        if wait_for_output and not self.wait_output(".*1234567890$"):
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

    
    def kill(self):
        self.proc.kill()
        self.proc.wait()

def call_logger_control(args):
    cmd = (logger_control,) + args
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,universal_newlines=True)
    return proc.communicate()[0]


if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
    temp = os.environ.get("TEMP")
    if temp is None:
        temp = os.environ.get("TMP")
    if temp is None:
        print ("Failed to find temp dir!")
        sys.exit(1)
    logdir = os.path.join(temp, "safir_sdk_core", "log")
else:
    exe_path = "."
    logdir = os.path.join("/", "tmp", "safir_sdk_core", "log")

print ("Logdir: ", logdir)

lll_test = os.path.join(exe_path,"lll_test")

SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")

logger_control = os.path.join(SAFIR_RUNTIME,"bin","logger_control")

def logfilename(proc):
    if sys.platform == "win32":
        fn = "lll_test.exe-" + str(proc.pid) + ".txt"
    else:
        fn = "lll_test-" + str(proc.pid) + ".txt"

    return os.path.join(logdir,fn)

if len(sys.argv) != 2:
    print ("Expect one argument")
    sys.exit(1)

configs_dir = sys.argv[1]
if not os.path.isdir(configs_dir):
    print ("arg is not a directory")
    sys.exit(1)

os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join (configs_dir,"enabled_off")

rmdir(logdir)

#Run the program that logs
p = LllProc()

#check that log file only contains error texts
found = False
for i in range(100):
    data = p.logfile()
    if data.count("123456789") >= 2:
        found = True
        break
    time.sleep(0.1)
    
if not found:
    print("failed to find error texts in log file")
    p.kill()
    sys.exit(1)


if data.find("Hello, World!") != -1 or data.find("Goodbye cruel world") != -1:
    print("found logs when logging should be off")
    p.kill()
    sys.exit(1)

#turn logging on
res = call_logger_control(("5",))
if res.find("Log level should now be 5") == -1:
    print("failed to turn logging on")
    p.kill()
    sys.exit(1)

#let it log for a short while
p.wait_output(".*Hello, World!$")

#turn logging off again
res = call_logger_control(("0",))
if res.find("Log level should now be 0") == -1:
    print("failed to turn logging off")
    proc.kill()
    sys.exit(1)


#check that log file isn't empty
data = p.logfile()
if data.find("Hello, World!") == -1 or data.find("1234567890") == -1:
    print("failed to find expected log data in log file")
    p.kill()
    sys.exit(1)
oldnum = data.count("Hello, World!")

if data.find("Goodbye cruel world!") != -1:
    print("found unexpected log data in log file")
    p.kill()
    sys.exit(1)


#sleep for a while and check that only errors are being logged
time.sleep(0.5)

newnum = p.logfile().count("Hello, World!")
if oldnum != newnum:
    print("log file appears to have grown with non-error messages even though logging is turned off!")
    p.kill()
    sys.exit(1)


#turn logging on again to level 9 and check that we get all logs
call_logger_control(("9",))
p.wait_output(".*Goodbye cruel world!$")
data = p.logfile()
if data.find("Hello, World!") == -1 or data.find("1234567890") == -1 or data.find("Goodbye cruel world") == -1:
    print("failed to find all expected log data in log file")
    p.kill()
    print(data)
    sys.exit(1)

p.kill()

#check that we get output on stdout
p = LllProc()
if not p.wait_output(".*1234567890$"):
    print("lllerr does not output on stdout")
    sys.exit(1)
p.kill()

#check that the timestamps can be turned off
p = LllProc()
if not p.wait_output("^\[[0-9.:]*\] 1234567890$"):
    print("could not find line with timestamp")
    sys.exit(1)
call_logger_control(("-t","0")) #turn timestamps off
if not p.wait_output("^1234567890$"):
    print("could not find line without timestamp")
    sys.exit(1)
p.kill()

#check that flushing can be turned off
p = LllProc()
call_logger_control(("-i","0")) #turn flushing off
time.sleep(0.1)
p2 = LllProc(False)
time.sleep(1.0)
p2.kill()
data = p2.output()
if len(data) != 0:
    print("Flushing doesnt seem to be possible to turn off")
    print(data)
    sys.exit(1)

#turn flushing on and stdout off
call_logger_control(("-s", "0"))
time.sleep(0.1)
p2 = LllProc(False)
time.sleep(2.0)
p2.kill()
data = p2.output()
if len(data) != 0:
    print("stdout logging doesnt seem to be possible to turn off")
    print(data)
    sys.exit(1)    

#turn file off
call_logger_control(("-f", "0"))
time.sleep(0.1)
p2 = LllProc()
time.sleep(2.0)
p2.kill()
data = p2.logfile()
if len(data) != 0:
    print("file logging doesnt seem to be possible to turn off")
    print(data)
    sys.exit(1)    

p.kill()

#check that disabling in inifile works
os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join (configs_dir,"disabled")

#Run the program that logs
p = LllProc()


#check that we dont have a log file
try:
    data = p.logfile()
    print("have unexpected logfile")
    sys.exit(1)
except:
    pass


#try to turn logging on
res = call_logger_control(("5",))
if res.find("LowLevelLogger is currently disabled") == -1:
    print("Doesnt seem to be disabled!")
    print(res)
    p.kill()
    sys.exit(1)

time.sleep(2.0)
data = p.output()
if data.find("orld!") != -1:
    print("there should not be any log output")
    p.kill()
    sys.exit(1)

p.kill()

#check that enabling and turning on in inifile works
os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join (configs_dir,"enabled_on")

#Run the program that logs
p = LllProc()

#check that we get some logs
p.wait_output(".*Goodbye cruel world!$")

data = p.logfile()
if data.find("Goodbye") == -1 or data.find("Hello") == -1 or data.find("1234567890") == -1:
    print("failed to find all the expected stuff in the log file")
    sys.exit(1)

#turn logging off
res = call_logger_control(("0",))
if res.find("Log level should now be 0") == -1:
    print("failed to turn logging off")
    proc.kill()
    sys.exit(1)

#clear output and check that new logs are only error logs
p.output()
time.sleep(0.5)
data = p.output()
if data.find("orld!") != -1:
    print ("failed to turn off ini enabled")
    sys.exit(1)
        
print("success")
sys.exit(0)
