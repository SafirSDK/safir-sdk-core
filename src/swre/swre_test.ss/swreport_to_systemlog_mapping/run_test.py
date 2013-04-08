#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://www.safirsdk.com)
#
# Created by: Anders Widén
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
import subprocess, os, time, sys, signal, re
import ConfigParser
import socket
import shutil
import copy

sys.path.append("../testutil")
from testenv import TestEnv, TestEnvStopper


if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

sender_pgm_name = "swreport_systemlog_sender" 
sender_path = os.path.join(exe_path, sender_pgm_name)

syslog_server_address = "127.0.0.1"
syslog_server_port = 6565

log_file = os.path.join(os.environ['SAFIR_RUNTIME'], "log", "logging.ini")

original_config = ConfigParser.RawConfigParser()
original_config.read(log_file)
    
config = ConfigParser.RawConfigParser()
config.read(log_file)

config.set('SYSTEM-LOG', 'send-to-syslog-server', 'true')
config.set('SYSTEM-LOG', 'syslog-server-address', syslog_server_address)
config.set('SYSTEM-LOG', 'syslog-server-port', str(syslog_server_port))

with open(log_file, 'wb') as configfile:
    config.write(configfile)

#Start listen to messages sent to the syslog server port
sock = socket.socket(socket.AF_INET, # Internet
                     socket.SOCK_DGRAM ) # UDP
sock.settimeout(2)
sock.bind( (syslog_server_address, syslog_server_port) )
    
#Run the program that sends swre logs
env = TestEnv()
with TestEnvStopper(env):
    subprocess.call((sender_path,"first instance"))

if not env.ReturnCodesOk():
    print("Some process exited with an unexpected value")
    sys.exit(1)

# Restore original config file
with open(log_file, 'wb') as configfile:
    original_config.write(configfile)

try:
    log_common_part = r"\D{3} [ |\d]\d \d{2}:\d{2}:\d{2} \S* " + sender_pgm_name + r"\[\d*\]: "
    for log_nbr in range(5):        
        data, addr = sock.recvfrom( 10 * 1024 ) # buffer size is 10 k
        
        if log_nbr == 0:
            pri = r"<10>"
            text = """========== Software Report Received ==========
Type => Fatal Error
Time => [0-9\-:T\.]*
Application connection => StandAlone;0;sender;[0-9#]*
Sequence number => 1
Report type sequence number => 1
Node => StandAlone
Error Code => FatalErrorCode
Location => swreport_sender
Text =>
Fatal error text from .* instance"""

        if log_nbr == 1:
            pri = r"<11>"
            text = """========== Software Report Received ==========
Type => Error
Time => [0-9\-:T\.]*
Application connection => StandAlone;0;sender;[0-9#]*
Sequence number => 2
Report type sequence number => 1
Node => StandAlone
Error Code => ErrorCode
Location => swreport_sender
Text =>
Error text from .* instance"""

        if log_nbr == 2:
            pri = r"<13>"
            text = """========== Software Report Received ==========
Type => Resource
Time => [0-9\-:T\.]*
Application connection => StandAlone;0;sender;[0-9#]*
Sequence number => 3
Report type sequence number => 1
Node => StandAlone
Resource Id => ResourceId
Resource Allocated => false
Text =>
Resource report text from .* instance"""

        if log_nbr == 3:
            pri = r"<8>"
            text = """========== Software Report Received ==========
Type => Programming Error
Time => [0-9\-:T\.]*
Application connection => StandAlone;0;sender;[0-9#]*
Sequence number => 4
Report type sequence number => 1
Node => StandAlone
Error Code => ProgrammingErrorCode
Location => swreport_sender
Text =>
Programming error text from .* instance"""

        if log_nbr == 4:
            pri = r"<15>"
            text = """========== Software Report Received ==========
Type => Program Info
Time => [0-9\-:T\.]*
Application connection => StandAlone;0;sender;[0-9#]*
Sequence number => 5
Report type sequence number => 1
Node => StandAlone
Text =>
Program info text from .* instance"""                  
   
        data = data.decode("utf-8")

        if re.match(pri + log_common_part + text, data) == None:
            print ("Unexpected syslog message: " + data)
            sys.exit(1)

except:
    print("Timeout")
    sys.exit(1)

sys.exit(0)


