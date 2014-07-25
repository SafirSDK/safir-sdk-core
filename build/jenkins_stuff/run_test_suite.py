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
import os, glob, sys, subprocess, time, re

def log(*args, **kwargs):
    print(*args, **kwargs)
    sys.stdout.flush()

class SetupError(Exception):
    pass

class WindowsInstaller(object):
    def __init__(self):
        self.installpath = os.path.join(os.environ["ProgramFiles"],"Safir SDK Core")
        self.uninstaller = os.path.join(self.installpath, "Uninstall.exe")

        installer = glob.glob("SafirSDKCore*.exe")
        if len(installer) != 1:
            raise SetupError("Unexpected number of installers: "+ str(installer))
        self.installer = installer[0]

                
    def uninstall(self, quiet = False):
        if not os.path.isfile(self.uninstaller):
            if quiet:
                return
            else:
                raise SetupError("No uninstaller found!")
        
        log ("Running uninstaller:", self.uninstaller)
        result = subprocess.call((self.uninstaller, "/S"))
        if result != 0:
            raise SetupError("Uninstaller failed (" + str(result) + ")!")
        
        #Wait for uninstaller to complete by polling for install directory...
        starttime = time.clock()
        while True:
            if not os.path.exists(self.installpath):
                break
            if time.clock() - starttime > 10*60: # 10 minutes
                break
            time.sleep(1.0)
            
        if os.path.isdir(self.installpath):
            raise SetupError("Installer dir still exists after uninstallation! Contents:\n" + str(os.listdir(self.installpath)))
        if os.path.exists(self.installpath):
            raise SetupError("Installer dir does not seem to be a directory!")
            
    def install(self):
        log ("Running installer:", self.installer)

        result = subprocess.call((self.installer, "/S"))

        if result != 0:
            raise SetupError("Installer failed (" + str(result) + ")!")

    def setup_debug_runtime(self):
        #we get out of here immediately if we're not running debug.
        if os.environ.get("Config") != "DebugOnly":
            return
        
        #Work out studio version and bitness from installer name
        match = re.search(r"SafirSDKCore-VS([0-9]*)-([0-9]*)bit-DebugOnly.exe", self.installer)
        vs_version = match.group(1)
        width = match.group(2)

        if width == "32":
            arch = "x86"
        elif width == "64":
            arch = "x64"

        debugcrt_path = os.path.join("c:",
                                     os.sep,
                                     "debug-runtime",
                                     "vs" + vs_version,
                                     arch)

        log("Adding", debugcrt_path, "to the PATH")

        if not os.path.isdir(debugcrt_path):
            raise SetupError("The debug runtime directory seems to be missing: " + debugcrt_path)

        os.environ["PATH"] += debugcrt_path
                                           
        
    def check_installation(self):
        if not os.path.isdir(self.installpath):
            raise SetupError("Installation directory does not exist!")
        listdir = os.listdir(self.installpath)
        if len(listdir) < 2:
            raise SetupError("Unexpected number of directories in installation directory: " + str(listdir))
        if all (dir in listdir for k in ("bin", "installer_utils", "include", "lib")):
            raise SetupError("Could not find some expected directory in installation directory: " + str(listdir))

        #Check that bin directory is in path
        pathed = os.path.join(self.installpath,"installer_utils","pathed")

        proc = subprocess.Popen((pathed,"/machine"),
                                stdout = subprocess.PIPE,
                                stderr = subprocess.STDOUT,
                                universal_newlines = True)

        output = proc.communicate()[0]
        binpath = os.path.join(self.installpath,"bin")
        if output.find(binpath) == -1:
            raise SetupError("bin directory does not appear to have been added to PATH:\n" + output)
        if os.environ["PATH"].find(binpath) != -1:
            raise SetupError("bin directory seems to have been added to PATH before installation!:\n" + os.environ["PATH"])
        os.environ["PATH"] += ";" + binpath


        self.setup_debug_runtime()
        
        proc = subprocess.Popen(("safir_show_config","--locations", "--typesystem", "--logging"),
                                stdout = subprocess.PIPE,
                                stderr = subprocess.STDOUT,
                                universal_newlines = True)
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run safir_show_config. returncode = " + str(proc.returncode) + "\nOutput:\n" + output)

def main():
    if sys.platform != "win32":
        log ("Only windows is supported so far")
        sys.exit(0)

    installer = WindowsInstaller()

    try:
        installer.uninstall(quiet = True)

        installer.install()
        installer.check_installation()
        
    except SetupError as e:
        log ("Error: " + str(e))
        return 1
    finally:
        try:
            installer.uninstall()
        except SetupError as e:
            log ("Error: " + str(e))
            return 1
    return 0

sys.exit(main())
