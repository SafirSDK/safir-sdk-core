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

import subprocess, os, time, sys
import socket

UDP_IP="127.0.0.1"
UDP_PORT=31221

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."
    
pl_test = os.path.join(exe_path,"pl_test")


sock = socket.socket( socket.AF_INET, # Internet
                      socket.SOCK_DGRAM ) # UDP
sock.settimeout(1)
sock.bind( (UDP_IP,UDP_PORT) )

#Run the program that sends
proc = subprocess.Popen(pl_test, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
proc.communicate()

if proc.returncode != 0:
    print "Failed to send panic log!"
    sys.exit(1)

try:
    data, addr = sock.recvfrom( 10 * 1024 ) # buffer size is 10 k
except:
    print "Timeout"
    sys.exit(1)

if addr[0] != "127.0.0.1":
    print "Receive from unexpected address"
    sys.exit(1)

if data.find("PANIC LOG!") == -1:
    print "Received data did not contain expected text 'PANIC LOG!'"
    sys.exit(1)

if data.find("Testing PanicLogging") == -1:
    print "Received data did not contain expected text 'Testing PanicLogging'"
    sys.exit(1)

sys.exit(0)
