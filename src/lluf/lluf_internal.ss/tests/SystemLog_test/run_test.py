#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013,2023 (http://safirsdkcore.com)
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
import subprocess
import os
import time
import sys

import configparser as ConfigParser
import socket
import re
import argparse

def parse_arguments():
    parser = argparse.ArgumentParser(description='unit test script')
    parser.add_argument("test_conf_dir", help="Test configuration directory")
    parser.add_argument("--test-exe", help="The test executable", required=True)
    parser.add_argument("--test-exe-name", help="The test executable name", required=True)
    return parser.parse_args()

args = parse_arguments()

conf_file = os.path.join(args.test_conf_dir, "logging.ini")

os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = args.test_conf_dir

config = ConfigParser.ConfigParser()

if not config.read(conf_file):
    print("Failed to read file " + conf_file)
    sys.exit(1)

# figure out what (if any) instance string that is expected
inst_str = r""
show_safir_instance = config.getboolean('SystemLog', 'show_safir_instance')
if show_safir_instance:
    inst_str = r"\(" + os.getenv("SAFIR_INSTANCE", "0") + r"\) "
truncate_syslog_to_bytes = config.getint('SystemLog', 'truncate_syslog_to_bytes', fallback=1024)
syslog_server_address = config.get('SystemLog', 'syslog_server_address')
syslog_server_port = config.get('SystemLog', 'syslog_server_port')

#Start listen to messages sent to the syslog server port
sock = socket.socket(
    socket.AF_INET,  # Internet
    socket.SOCK_DGRAM)  # UDP
sock.settimeout(2)
sock.bind((syslog_server_address, int(syslog_server_port)))

#Run the program that sends system logs
proc = subprocess.Popen(args.test_exe, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)
stdout, stderr = proc.communicate()

if proc.returncode != 0:
    print("Failed when sending system logs!")
    print(stdout)
    sys.exit(1)


log_common_part = r"\D{3} [ |\d]\d \d{2}:\d{2}:\d{2} \S* " + args.test_exe_name + r"\[\d*\]: "
for test in range(12):
    data, addr = sock.recvfrom(10 * 1024)  # buffer size is 10 k
    print("Received data:", data)
    if test == 0:
        pri = r"<9>"
        text = r"This is a log from a singleton constructor"
    elif test == 1:
        pri = r"<8>"
        text = r"This is an emergency log"
    elif test == 2:
        pri = r"<9>"
        text = r"This is an alert log"
    elif test == 3:
        pri = r"<10>"
        text = r"This is a critical log with   newline and \t tab"
    elif test == 4:
        pri = r"<11>"
        text = r"This is an error log"
    elif test == 5:
        pri = r"<12>"
        text = r"This is a warning log with   newline and \t tab"
    elif test == 6:
        pri = r"<13>"
        text = r"This is a notice log"
    elif test == 7:
        pri = r"<14>"
        text = r"This is an informational log with   newline and \t tab"
    elif test == 8:
        pri = r"<15>"
        text = r"This is a debug log"
    elif test == 9:
        pri = r"<11>"
        text = r"This is another error log"
    elif test == 10:
        pri = r"<15>"
        text = r"This is a suuuuuuuuuuper duuuuuuuper long log line a*"
        if truncate_syslog_to_bytes == 1024:
            text+= r"\.\.\.$"
        else:
            text+= r"b*c$"

    elif test == 11:
        pri = r"<9>"
        text = r"This is a log from a singleton destructor"

    p = re.compile(pri + log_common_part + inst_str + text)
    data = data.decode("utf-8")
    if p.match(data) == None:
        print("Unexpected syslog message:", data)
        sys.exit(1)

print("Success!")
sys.exit(0)
