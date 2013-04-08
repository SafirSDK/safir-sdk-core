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
import subprocess, os, time, sys
import ConfigParser
import socket
import shutil
import re

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

system_log_test_pgm = "SystemLog_test"  
system_log_test_path = os.path.join(exe_path, system_log_test_pgm)

syslog_server_address = "127.0.0.1"
syslog_server_port = 6565

# Temporary redirect SAFIR_RUNTIME
os.environ["SAFIR_RUNTIME"] = exe_path

# Create configuration file
tmp_log_file_dir = os.path.join(exe_path, "log")

if not os.path.exists(tmp_log_file_dir):
    os.mkdir(tmp_log_file_dir)
config = ConfigParser.RawConfigParser()
config.add_section('SYSTEM-LOG')
config.set('SYSTEM-LOG', 'native-logging', 'false')
config.set('SYSTEM-LOG', 'send-to-syslog-server', 'true')
config.set('SYSTEM-LOG', 'syslog-server-address', syslog_server_address)
config.set('SYSTEM-LOG', 'syslog-server-port', str(syslog_server_port))

with open(os.path.join(tmp_log_file_dir, "logging.ini"), 'wb') as configfile:
    config.write(configfile)

#Start listen to messages sent to the syslog server port
sock = socket.socket(socket.AF_INET, # Internet
                     socket.SOCK_DGRAM ) # UDP
sock.settimeout(2)
sock.bind( (syslog_server_address, syslog_server_port) )
    
#Run the program that sends system logs
proc = subprocess.Popen(system_log_test_path, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,universal_newlines=True)
stdout, stderr = proc.communicate()
print (stdout)

# Remove temporary config file
shutil.rmtree(tmp_log_file_dir, True)

if proc.returncode != 0:
    print("Failed when sending system logs!")
    sys.exit(1)

try:
    log_common_part = r"\D{3} [ |\d]\d \d{2}:\d{2}:\d{2} \S* " + system_log_test_pgm + r"\[\d*\]: "
    for test in range(1, 2):        
        data, addr = sock.recvfrom( 10 * 1024 ) # buffer size is 10 k
        print ("Received data: " + data) 
        if test == 1:
            pri = r"<10>"
            text = r"Sending a critical log"
        if test == 2:
            pri = r"<12>"
            text = r"Sending a warning log with \n newline and \t tab"

        p = re.compile(pri + log_common_part + text)
        data = data.decode("utf-8")
        if p.match(data) == None:
            print ("Unexpected syslog message: " + data)
            sys.exit(1)
        print(data)
except:
    print("Timeout")
    sys.exit(1)

sys.exit(0)
