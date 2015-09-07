#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
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
import syslog_server
from threading import Thread

try:
    # 3.x name
    from queue import Queue, Empty
except ImportError:
    # 2.x name
    from Queue import Queue, Empty

def enqueue_output(out, queue):
    while True:
        line = out.readline()
        if not line:
            break
        queue.put(line.rstrip("\n\r"))
    out.close()

class Unbuffered(object):
   def __init__(self, stream):
       self.stream = stream
   def write(self, data):
       self.stream.write(data)
       self.stream.flush()
   def __getattr__(self, attr):
       return getattr(self.stream, attr)

sys.stdout = Unbuffered(sys.stdout)

class TestEnvStopper:
    def __init__(self, env):
        self.env = env

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        time.sleep(1.0)
        self.env.killprocs()


class TestEnv:
    """
    safir_control: full path to safir_control
    dose_main: full path to dose_main
    dope_main: full path to dope_main
    safir_show_config: full path to safir_show_config

    If the exes are in the PATH, its okay to just use exe names.
    """
    def __init__(self, safir_control, dose_main, dope_main, safir_show_config, start_syslog_server = True, ignore_control_cmd = False ):
        self.__procs = dict()
        self.__creationflags = 0
        if sys.platform == "win32":
            self.__creationflags= subprocess.CREATE_NEW_PROCESS_GROUP
        self.safir_control = self.launchProcess("safir_control", (safir_control, "--dose-main-path", dose_main, "--ignore-control-cmd", str(ignore_control_cmd)))
        self.launchProcess("dope_main", (dope_main,))

        self.start_syslog_server = start_syslog_server
        if self.start_syslog_server == True:
            self.syslog = syslog_server.SyslogServer(safir_show_config)
        self.syslog_output = list()

        start_time = time.time()
        print("Waiting for safir_control to be ready")

        phrase="persistence data is ready"

        while True:
            time.sleep(0.2)
            if self.Output("safir_control").find(phrase) != -1:
                print(" dose_main seems to be ready")
                break
            if self.safir_control.poll() is not None:
                raise Exception(" safir_control terminated!\n" +
                                "----- Output so far ----\n" +
                                self.Output("safir_control") +
                                "\n---------------------")
            if time.time() - start_time > 90:
                start_time = time.time()
                print("safir_control and/or dose_main seems slow to start. Here is some output:")
                print("----- safir_control output -----")
                print(self.Output("safir_control"))
                print("----- dope_main output -----")
                print(self.Output("dope_main"))
                print("---- syslog output ----")
                if self.start_syslog_server == True:
                    print(self.syslog.get_data(0))
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

    def __kill(self, name, proc, timeout):
        try:
            print(" Terminating", name)
            if sys.platform == "win32":
                #can't send CTRL_C_EVENT to processes started with subprocess, unfortunately
                proc.send_signal(signal.CTRL_BREAK_EVENT)
            else:
                proc.terminate()
            #let it have a minute to die...
            for i in range (timeout * 10):
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
        self.__kill("safir_control", self.safir_control, timeout = 120)

        polls = 0
        for name, (proc,queue,output) in self.__procs.items():
            while polls < 600 and proc.poll() is None:
                time.sleep(0.1)
                polls += 1

            if proc.returncode is None:
                self.__kill(name,proc, timeout = 30)

            if proc.returncode != 0:
                print("--", name, "returncode is", proc.returncode, " -----")
                print(self.Output(name))
                print("----------------------------------")

        if self.start_syslog_server == True:
            self.syslog.stop()

    def Syslog(self):
        if self.start_syslog_server == True:
            data = self.syslog.get_data(0)
        self.syslog_output.append(data)
        return "".join(self.syslog_output)

    def Output(self,name):
        (proc,queue,output) = self.__procs[name]
        try:
            while True:
                output.append(queue.get_nowait())
        except Empty:
            pass
        return "\n".join(output) + "\n"

    def WaitForOutput(self, name, expected_output):
        while True:
            output = self.Output(name)
            #print(output)
            if output.find(expected_output) != -1:
                return output
            if not self.ProcessDied(): #reversed return value
                raise Exception("A process died")
            time.sleep(1)


    def ResetOutput(self, name):
        (proc,queue,output) = self.__procs[name]
        del output[:] #clear the list

    def ReturnCodesOk(self):
        ok = True
        for name, (proc,queue,output) in self.__procs.items():
            if proc.returncode != 0:
                print (" - Process", name, "exited with code", proc.returncode)
                print (" - Output:\n", self.Output(name))
                ok = False
        return ok;

    def ProcessDied(self):
        ok = True
        for name, (proc,queue,output) in self.__procs.items():
            ret = proc.poll()
            if ret is not None and ret != 0:
                print (" - Process", name, "exited with code", proc.returncode)
                print (" - Output:\n", self.Output(name))
                ok = False
        return ok;
