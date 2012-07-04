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

import subprocess, os, time, sys, shutil, timeit

def touch(fname, times = None):
    with file(fname, 'a'):
        os.utime(fname, times)

class Timer(object):
    def __init__(self,name):
        self.__name = name

    def __enter__(self):
        self.__start = time.time()

    def __exit__(self, type, value, traceback):
        # Error handling here
        self.__finish = time.time()
        print self.__name, "took", self.duration_in_seconds(), "seconds"

    def duration_in_seconds(self):
        return self.__finish - self.__start

def run(name):
    #with Timer(name):
    print "==", name, "=="
    subprocess.Popen(os.path.join(".",name), stdout = devnull).wait()


if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

#lll_test = os.path.join(exe_path,"lll_test")

SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")

logdir = os.path.join(SAFIR_RUNTIME,"log")
doblogdir = os.path.join(logdir,"Dob-LowLevelLog")

if not os.path.exists(logdir):
    os.mkdir(logdir)
if os.path.exists(doblogdir):
    shutil.rmtree(doblogdir)
os.mkdir(doblogdir)

touch(os.path.join(doblogdir,"logging_on"))

devnull = open(os.devnull,"w")

run("new_unflushed")
run("new_flushed")
#run("cppstream_unflushed")
#run("cppstream_flushed")
run("old_unflushed")
run("old_flushed")
run("threaded_unflushed")
run("threaded_flushed")
    

sys.exit(0)
