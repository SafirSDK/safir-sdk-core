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
        
class SyslogServer(SocketServer.UDPServer):
    class __Handler(SocketServer.DatagramRequestHandler):
        
        def handle(self):
            data = self.request[0].decode("utf-8")
            if self.server.buf is None:
                print(data)
            else:
                if self.server.buf:
                    self.server.buf += "\n"
                self.server.buf += data
                
    def __init__(self):
        SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")

        #Run the program that writes the ini file configuration to standard output
        proc = subprocess.Popen(os.path.join(SAFIR_RUNTIME,"bin","safir_show_config"),
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,
                                universal_newlines=True)
        # ConfigParser wants a section header so add a dummy one.
        conf_str = '[root]\n' + proc.communicate()[0]

        config = ConfigParser.ConfigParser()
        config.readfp(StringIO(conf_str))

        send_to_syslog_server = config.getboolean('SystemLog','send_to_syslog_server')
        
        if not send_to_syslog_server:
            print (conf_str)
            raise Exception("Safir is not configured to send logs to a syslog_server!")
        
        self.syslog_server_address = config.get('SystemLog','syslog_server_address')
        self.syslog_server_port = config.getint('SystemLog','syslog_server_port')

        SocketServer.UDPServer.__init__(self,
                                        (self.syslog_server_address,
                                         self.syslog_server_port),
                                        SyslogServer.__Handler)

        #set up some variables that the handler can use
        self.is_timed_out = False
        self.buf = None

    def handle_timeout(self):
        self.is_timed_out = True

    def get_data(self, timeout):
        self.buf = str()
        self.timeout = timeout
        self.is_timed_out = False
        while not self.is_timed_out:
            self.handle_request()

        return self.buf
        
if __name__ == "__main__":
    try:
        server = SyslogServer()
        print ("Listening to", server.syslog_server_address, server.syslog_server_port)
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    




