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
try:
    import ConfigParser
except ImportError:
    import configparser as ConfigParser
import socket
import shutil
import re
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("test_conf_dir", help="Test configuration directory")
args = parser.parse_args()

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

system_log_test_pgm = "SystemLog_test"  
system_log_test_path = os.path.join(exe_path, system_log_test_pgm)

conf_dir = os.path.join(args.test_conf_dir, "syslog_and_native_logging")
conf_file = os.path.join(conf_dir, "logging.ini")

os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = conf_dir

config = ConfigParser.ConfigParser()
      
if not config.read(conf_file):
    print("Failed to read file " + conf_file)
    sys.exit(1)
    
syslog_server_address = config.get('SystemLog','syslog_server_address')
syslog_server_port = config.get('SystemLog','syslog_server_port')

#Start listen to messages sent to the syslog server port
sock = socket.socket(socket.AF_INET, # Internet
                     socket.SOCK_DGRAM ) # UDP
sock.settimeout(2)
sock.bind((syslog_server_address, int(syslog_server_port)))
    
#Run the program that sends system logs
proc = subprocess.Popen(system_log_test_path, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,universal_newlines=True)
stdout, stderr = proc.communicate()
#print (stdout)

if proc.returncode != 0:
    print("Failed when sending system logs!")
    sys.exit(1)

try:
    log_common_part = r"\D{3} [ |\d]\d \d{2}:\d{2}:\d{2} \S* " + system_log_test_pgm + r"\[\d*\]: "
    for test in range(9):
        data, addr = sock.recvfrom( 10 * 1024 ) # buffer size is 10 k
        print ("Received data:", data) 
        if test == 0:
            pri = r"<8>"
            text = r"This is an emergency log"
        elif test == 1:
            pri = r"<9>"
            text = r"This is an alert log"            
        elif test == 2:
            pri = r"<10>"
            text = r"This is a critical log with   newline and \t tab"
        elif test == 3:
            pri = r"<11>"
            text = r"This is an error log"
        elif test == 4:
            pri = r"<12>"
            text = r"This is a warning log with   newline and \t tab"
        elif test == 5:
            pri = r"<13>"
            text = r"This is a notice log"
        elif test == 6:
            pri = r"<14>"
            text = r"This is an informational log with   newline and \t tab"
        elif test == 7:
            pri = r"<15>"
            text = r"This is a debug log"            
        elif test == 8:
            pri = r"<11>"
            text = r"This is another error log"
            
        p = re.compile(pri + log_common_part + text)
        data = data.decode("utf-8")
        if p.match(data) == None:
            print ("Unexpected syslog message:", data)
            sys.exit(1)
            
except:
    print("Exception!")
    sys.exit(1)

print ("Success!")
sys.exit(0)
