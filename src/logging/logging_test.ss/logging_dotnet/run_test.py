#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://safir.sourceforge.net)
#
# Created by: Anders Widén <anders.widen@consoden.se>
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
import subprocess, os, time, sys, shutil
import syslog_server

#TODO remove this when we drop python 2.6 support
if "check_output" not in dir( subprocess ): # duck punch it in!
    def f(*popenargs, **kwargs):
        if 'stdout' in kwargs:
            raise ValueError('stdout argument not allowed, it will be overridden.')
        process = subprocess.Popen(stdout=subprocess.PIPE, *popenargs, **kwargs)
        output, unused_err = process.communicate()
        retcode = process.poll()
        if retcode:
            cmd = kwargs.get("args")
            if cmd is None:
                cmd = popenargs[0]
            raise subprocess.CalledProcessError(retcode, cmd)
        return output
    subprocess.check_output = f

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."
    
sender_path_base = os.path.join(exe_path,"log_sender_dotnet")
sender_csexe = sender_path_base+".csexe"
sender_exe = sender_path_base+".exe"
shutil.copy2(sender_csexe,sender_exe)

dependencies = ("Safir.Logging.dll",)

SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
for dep in dependencies:
    shutil.copy2(os.path.join(SAFIR_RUNTIME,"bin",dep),
                 ".")

log_server = syslog_server.SyslogServer()

o1 = subprocess.check_output(sender_exe, stderr=subprocess.STDOUT)
o2 = subprocess.check_output(sender_exe, stderr=subprocess.STDOUT)
o3 = subprocess.check_output(sender_exe, stderr=subprocess.STDOUT)

os.remove(sender_exe)
for dep in dependencies:
    os.remove(dep)

syslog_output = log_server.get_data(1)
stdout_output = (o1 + o2 + o3).decode("utf-8").replace("\r","")

def fail(message):
    print("Failed! Wrong number of ",message)
    print ("STDOUT OUTPUT:")
    safe_print(stdout_output)
    print ("SYSLOG OUTPUT:")
    safe_print(syslog_output)
    sys.exit(1)

if stdout_output.count(u"This is an emergency log. Bryn@s @r b@st!@") != 3 or syslog_output.count(u"This is an emergency log. Bryn\u00e4s \u00e4r b\u00e4st!\u2620") != 3:
    fail("Brynas ar bast")
                                                                                               
if stdout_output.count(u"This is an alert log") != 3 or syslog_output.count(u"This is an alert log") != 3:
    fail("Alert log")

if stdout_output.count(u"This is a critical log") != 3 or syslog_output.count(u"This is a critical log") != 3:
    fail("Critical log")

if stdout_output.count(u"This is an error log") != 3 or syslog_output.count(u"This is an error log") != 3:
    fail("Error log")

if stdout_output.count(u"This is a warning log") != 0 or syslog_output.count(u"This is a warning log") != 3:
    fail("Warning log")

if stdout_output.count(u"This is a notice log") != 0 or syslog_output.count(u"This is a notice log") != 3:
    fail("Notice log")

if stdout_output.count(u"This is an informational log") != 0 or syslog_output.count(u"This is an informational log") != 3:
    fail("Informational log")

if stdout_output.count(u"This is a debug log") != 0 or syslog_output.count(u"This is a debug log") != 3:
    fail("Debug log")                                                                                  
                                                                         
print("Found all expected output!")
sys.exit(0)


