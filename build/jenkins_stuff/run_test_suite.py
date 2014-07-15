#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2014 (http://safir.sourceforge.net)
#
# Created by: Lars Hagstrom / lars.hagstrom@consoden.se
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
import os, glob, sys, subprocess


def uninstall(installpath):
    path = os.path.join(installpath, "Uninstall.exe")
    if not os.path.isfile(path):
        print ("Couldn't find uninstaller")
        return False
    result = subprocess.call((path, "/S"))
    print ("Uninstall result:", result)
    return True
    
print (os.listdir("."))

if sys.platform != "win32":
    print ("Only windows is supported so far")
    sys.exit(0)

installpath = os.path.join(os.environ["ProgramFiles"],"Safir SDK Core")
uninstall(installpath)

installer = glob.glob("SafirSDKCore*.exe")

if len(installer) != 1:
    print("Unexpected number of installers:", installer)
    sys.exit(1)

installer = installer[0]

print ("Will run installer", installer)

result = subprocess.call((installer, "/S"))

print ("Install result:", result)

returncode = 0

print(os.listdir(installpath))
if len(os.listdir(installpath)) < 2:
    returncode = 1

try:
    print("attempt 1")
    result = subprocess.call("safir_show_config")
    if result != 0:
        print ("safir_show_config failed")
        returncode = 1
except:
    pass

try:
    print("attempt 2")
    proc = subprocess.Popen(("safir_show_config",),creationflags = subprocess.CREATE_NEW_PROCESS_GROUP).communicate()
    if proc.returncode != 0:
        print ("safir_show_config failed")
        returncode = 1
    except:
        pass

try:
    print("attempt 3")
    proc = subprocess.Popen(("safir_show_config",),creationflags = subprocess.CREATE_NEW_CONSOLE).communicate()
    if proc.returncode != 0:
        print ("safir_show_config failed")
        returncode = 1
except:
    pass

if not uninstall(installpath):
    print("Uninstall failed")
    returncode = 1

sys.exit(returncode)
