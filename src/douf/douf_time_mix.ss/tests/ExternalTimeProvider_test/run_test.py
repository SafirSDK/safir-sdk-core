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

import subprocess, os, time, sys, xml.dom.minidom, shutil

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    libpath = os.environ.get("LD_LIBRARY_PATH")
    os.environ["LD_LIBRARY_PATH"] = libpath + ":" + os.getcwd()
    exe_path = "."


SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")

parameters_path = os.path.join(SAFIR_RUNTIME,
                               "data", 
                               "text", 
                               "dots", 
                               "classes", 
                               "definitions",
                               "safir_core", 
                               "config")

LibraryParameters_path = os.path.join(parameters_path, 
                               "Safir.Time.LibraryParameters.dou")

backup_path = os.path.join(".","param.bak")

def getText(nodelist):
    rc = []
    for node in nodelist:
        if node.nodeType == node.TEXT_NODE:
            rc.append(node.data)
    return ''.join(rc)

def UpdateConfig():
    dom = xml.dom.minidom.parse(LibraryParameters_path)
    for param in dom.getElementsByTagName("parameter"):
        name = getText(param.getElementsByTagName("name")[0].childNodes)
        value = param.getElementsByTagName("value")[0]
        if name == "LibraryName":
            value.childNodes[0].data = "external_time_provider_for_test"
        if name == "UtcTimeFunctionName":
            value.childNodes[0].data = "MyGetTimeUtc"
        if name == "LocalTimeOffsetFunctionName":
            value.childNodes[0].data = "MyGetLocalTimeOffset"
    with open(LibraryParameters_path,"w") as file:
       file.write(dom.toxml())


#backup config
shutil.copy2(LibraryParameters_path,backup_path)

UpdateConfig()

result = subprocess.call(os.path.join(exe_path,"ExternalTimeProvider_test"))

#restore config
shutil.copy2(backup_path,LibraryParameters_path)

if result != 0:
    print "Failure"
    sys.exit(1)

sys.exit(0)
