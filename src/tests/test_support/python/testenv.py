#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
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
import subprocess, sys, time, signal
import syslog_server
from threading import Thread

try:
    # 3.x name
    from queue import Queue, Empty
except ImportError:
    #pylint: disable=E0401
    # 2.x name
    from Queue import Queue, Empty

def enqueue_output(out, queue):
    while True:
        line = out.readline()
        if not line:
            break
        queue.put(line.rstrip("\n\r"))
    out.close()


def log(*args, **kwargs):
    print(*args, **kwargs)
    sys.stdout.flush()

class TestEnvStopper:
    def __init__(self, env):
        self.env = env

    def __enter__(self):
        return self

    def __exit__(self, t, value, traceback):
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
    def __init__(self, safir_control, dose_main, dope_main, safir_show_config, start_syslog_server = True, ignore_control_cmd = False, wait_for_persistence = True):
        self.__procs = dict()
        self.__creationflags = 0
        if sys.platform == "win32":
            self.__creationflags= subprocess.CREATE_NEW_PROCESS_GROUP
        self.safir_control = self.launchProcess("safir_control", (safir_control, "--dose-main-path", dose_main, "--ignore-control-cmd", str(ignore_control_cmd)))
        self.dope = dope_main is not None
        if self.dope:
            self.launchProcess("dope_main", (dope_main,))

        self.start_syslog_server = start_syslog_server
        if self.start_syslog_server == True:
            self.syslog = syslog_server.SyslogServer(safir_show_config)
        self.syslog_output = list()

        if wait_for_persistence:
            self.WaitForPersistence()

    def WaitForPersistence(self):
        start_time = time.time()
        log("Waiting for safir_control to be ready")

        phrase="persistence data is ready"

        while True:
            time.sleep(0.2)
            if self.Output("safir_control").find(phrase) != -1:
                log(" dose_main seems to be ready")
                break
            if self.safir_control.poll() is not None:
                raise Exception(" safir_control terminated!\n" +
                                "----- Output so far ----\n" +
                                self.Output("safir_control") +
                                "\n---------------------")
            if time.time() - start_time > 90:
                start_time = time.time()
                log("safir_control and/or dose_main seems slow to start. Here is some output:")
                log("----- safir_control output -----")
                log(self.Output("safir_control"))
                if self.dope:
                    log("----- dope_main output -----")
                    log(self.Output("dope_main"))
                log("---- syslog output ----")
                if self.start_syslog_server == True:
                    log(self.syslog.get_data(0))
                log("----------------------------")
                log("Will keep waiting")

    def launchProcess(self, name, cmd, collect_output = True):
        log("Launching", name)
        proc = subprocess.Popen(cmd,
                                stdout = subprocess.PIPE if collect_output else None,
                                stderr = subprocess.STDOUT if collect_output else None,
                                creationflags = self.__creationflags,
                                start_new_session = True,
                                universal_newlines = True)
        queue = Queue()
        if collect_output:
            thread = Thread(target=enqueue_output, args=(proc.stdout, queue))
            thread.daemon = True #Thread dies with program, no need to stop explicitly
            thread.start()

        self.__procs[name] = (proc,queue,list())
        return proc

    def __kill(self, name, proc, timeout):
        try:
            log(" Terminating", name)
            if sys.platform == "win32":
                #pylint: disable=E1101
                #can't send CTRL_C_EVENT to processes started with subprocess, unfortunately
                proc.send_signal(signal.CTRL_BREAK_EVENT)
            else:
                proc.terminate()
            #let it have a minute to die...
            for _ in range (timeout * 10):
                if proc.poll() is not None:
                    log("   Terminate successful")
                    return
                time.sleep(0.1)
            if proc.poll() is None:
                log(" Killing", name)
                proc.kill()
                proc.wait()
        except OSError:
            pass

    def killprocs(self):
        log("Terminating all processes")
        self.__kill("safir_control", self.safir_control, timeout = 120)

        polls = 0
        for name, (proc,queue,output) in self.__procs.items():
            while polls < 600 and proc.poll() is None:
                time.sleep(0.1)
                polls += 1

            if proc.returncode is None:
                self.__kill(name,proc, timeout = 30)

            if proc.returncode != 0:
                log("--", name, "returncode is", proc.returncode, " -----")
                log(self.Output(name))
                log("----------------------------------")

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
            #log(output)
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
                log (" - Process", name, "exited with code", proc.returncode)
                log (" - Output:\n", self.Output(name))
                ok = False
        return ok

    def ProcessDied(self):
        ok = True
        for name, (proc,queue,output) in self.__procs.items():
            ret = proc.poll()
            if ret is not None and ret != 0:
                log (" - Process", name, "exited with code", proc.returncode)
                log (" - Output:\n", self.Output(name))
                ok = False
        return ok

    def WaitForProcess(self,name):
        proc = self.__procs.get(name)
        return proc[0].wait()
    
    def SafirControlRunning(self):
        proc = self.__procs.get("safir_control")

        if proc is None:
            return False

        return proc[0].poll() is None
