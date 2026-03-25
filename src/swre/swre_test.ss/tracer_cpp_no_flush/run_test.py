#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://safirsdkcore.com)
#
# Created by: Lars Hagström / lars.hagstrom@consoden.se
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
import sys
import argparse
import syslog_server
import time
from safe_print import *

parser = argparse.ArgumentParser("test script")
parser.add_argument("--binary", required=True)
parser.add_argument("--safir-show-config", required=True)

arguments = parser.parse_args()

syslog = syslog_server.SyslogServer(arguments.safir_show_config)

# Start sender process
sender = subprocess.Popen((arguments.binary, "enable"))

# Poll syslog for expected string
while True:
    data = syslog.get_data(0.1)
    safe_print(f"Got {len(data)} bytes of data from syslog")
    if "Application is not flushing its tracers" in data:
        sender.kill()
        sender.wait()
        safe_print("success")
        sys.exit(0)
