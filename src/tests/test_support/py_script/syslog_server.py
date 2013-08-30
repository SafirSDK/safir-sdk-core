#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://www.safirsdk.com)
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
import subprocess, os, time, sys
try:
    import ConfigParser
except ImportError:
    import configparser as ConfigParser
try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO
try:
    import SocketServer
except ImportError:
    import socketserver as SocketServer
    
import shutil
import re

print_to_std_out = False;
buf = str()

class _Handler(SocketServer.DatagramRequestHandler):
        
    def handle(self):
        global buf
        global print_to_std_out

        data = self.rfile.readline()
        
        if print_to_std_out:
            print (data)
            
        if buf:
           buf = buf + '\n'           
        buf = buf + data

        
class SyslogServer(SocketServer.UDPServer):
    
    def __init__(self):
        SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")

        #Run the program that writes the ini file configuration to standard output
        proc = subprocess.Popen(os.path.join(SAFIR_RUNTIME,"bin","safir_show_config"), stdout=subprocess.PIPE, stderr=subprocess.STDOUT,universal_newlines=True)
        stdout, stderr = proc.communicate()

        # ConfigParser wants a section header so add a dummy one.
        conf_str = '[root]\n' + stdout

        config = ConfigParser.ConfigParser()
        config.readfp(StringIO(conf_str))

        send_to_syslog_server = config.getboolean('SystemLog','send_to_syslog_server')
        
        if not send_to_syslog_server:
            raise Exception("Safir is not configured to send logs to a syslog_server!")
        
        syslog_server_address = config.get('SystemLog','syslog_server_address')
        syslog_server_port = config.get('SystemLog','syslog_server_port')

        SocketServer.UDPServer.__init__(self, (syslog_server_address, int(syslog_server_port)), _Handler)

        self.is_timed_out = False
        
    def handle_timeout(self):
        self.is_timed_out = True

    def get_data(self, timeout):
        global buf
        buf = str()
        self.timeout = timeout
        self.is_timed_out = False
        while not self.is_timed_out:
            self.handle_request()

        return buf
        
if __name__ == "__main__":
    server = SyslogServer()
    print_to_std_out = True
    server.serve_forever()
    




