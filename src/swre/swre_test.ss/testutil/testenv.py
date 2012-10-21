#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2012 (http://www.safirsdk.com)
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
import os, subprocess, sys, threading, time, signal

from threading import Thread

try:
    # 3.x name
    from queue import Queue, Empty
except ImportError:
    # 2.x name
    from Queue import Queue, Empty

SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
dose_main_cmd = (os.path.join(SAFIR_RUNTIME,"bin","dose_main"),)
dope_main_cmd = (os.path.join(SAFIR_RUNTIME,"bin","dope_main"),)
swre_logger_cmd = (os.path.join(SAFIR_RUNTIME,"bin","swre_logger"),"-logwhenstarted")

def enqueue_output(out, queue):
    while True:
        line = out.readline()
        if not line:
            break
        queue.put(line.rstrip("\n\r"))
    out.close()

class TestEnvStopper:
    def __init__(self, env):
        self.env = env

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        time.sleep(1.0)
        self.env.killprocs()


class TestEnv:
    def __init__(self):
        self.__procs = dict()
        self.__creationflags = 0
        if sys.platform == "win32":
            self.__creationflags= subprocess.CREATE_NEW_PROCESS_GROUP
        self.outputlock = threading.Lock()
        self.dose_main = self.launchProcess("dose_main", dose_main_cmd)
        self.launchProcess("dope_main", dope_main_cmd)
        self.swre_logger = self.launchProcess("swre_logger", swre_logger_cmd)

        start_time = time.time()
        print("Waiting for dose_main to be ready")
        dose_started = False
        swre_started = False
        while True:
            time.sleep(0.2)
            if self.Output("dose_main").find("persistence data is ready") != -1:
                print(" dose_main seems to be ready")
                dose_started = True
            if self.Output("swre_logger").find("Subscriptions have been set up") != -1:
                print(" swre_logger seems to be ready")
                self.fix_swre_output()
                swre_started = True
            if swre_started and dose_started:
                break
            if self.dose_main.poll() is not None:
                raise Exception(" dose_main appears to have failed to start!\n" +
                                "----- Output so far ----\n" + 
                                self.Output("dose_main") +
                                "\n---------------------")
            if self.swre_logger.poll() is not None:
                raise Exception(" swre_logger appears to have failed to start!\n" +
                                "----- Output so far ----\n" + 
                                self.Output("swre_logger") +
                                "\n---------------------")
            if time.time() - start_time > 90:
                start_time = time.time()
                print("dose_main seems slow to start. Here is some output:")
                print("----- dose_main output -----")
                print(self.Output("dose_main"))
                print("----- dope_main output -----")
                print(self.Output("dope_main"))
                print("---- swre_logger output ----")
                print(self.Output("swre_logger"))
                print("----------------------------")
                print("Will keep waiting")

    def launchProcess(self, name, cmd):
        print("Launching", name)
        proc = subprocess.Popen(cmd, 
                                stdout = subprocess.PIPE, 
                                stderr = subprocess.STDOUT,
                                creationflags = self.__creationflags,
                                universal_newlines = True)
        queue = Queue()
        thread = Thread(target=enqueue_output, args=(proc.stdout, queue))
        thread.daemon = True #Thread dies with program, no need to stop explicitly
        thread.start()

        self.__procs[name] = (proc,queue,list())
        return proc

    def __kill(self, name, proc):
        try:
            print(" Terminating", name)
            if sys.platform == "win32":
                #can't send CTRL_C_EVENT to processes started with subprocess, unfortunately
                proc.send_signal(signal.CTRL_BREAK_EVENT)
            else:
                proc.terminate()
            for i in range (100):
                if proc.poll() is not None:
                    print("   Terminate successful")
                    return
                time.sleep(0.1)
            if proc.poll() is None:
                print(" Killing", name)
                proc.kill()
                proc.wait()
        except OSError:
            pass

    def killprocs(self):
        print("Terminating all processes")
        self.__kill("dose_main", self.dose_main)

        polls = 0
        for name, (proc,queue,output) in self.__procs.items():
            while polls < 600 and proc.poll() is None:
                time.sleep(0.1)
                polls += 1

            if proc.returncode != 0:
                print(" ", name, "returncode is", proc.returncode)

    def Output(self,name):
        (proc,queue,output) = self.__procs[name]
        try:
            while True:
                output.append(queue.get_nowait())
        except Empty:
            pass
        return "\n".join(output) + "\n"

    def fix_swre_output(self):
        (proc,queue,output) = self.__procs["swre_logger"]
        if output[0] != "Subscriptions have been set up. Logging started.":
            raise Exception("Unexpected first line in swre logger window!\n" + 
                            "----- Output so far ----\n" + 
                            self.Output("swre_logger") +
                            "\n---------------------")
        output.pop(0)

    def ReturnCodesOk(self):
        ok = True
        for name, (proc,queue,output) in self.__procs.items():
            if proc.returncode != 0:
                ok = False
        return ok;
