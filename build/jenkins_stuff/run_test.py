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
import os, glob, sys, subprocess, time, re, platform, argparse

#try to import a package that we need for the debian installer
#this will fail quietly on all other platforms, which is all right.
try:
    import apt
except:
    pass

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

    def can_uninstall(self):
        return os.path.isfile(self.uninstaller)

    def uninstall(self):
        if not self.can_uninstall():
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
            raise SetupError("Installer dir still exists after uninstallation! Contents:\n"
                             + str(os.listdir(self.installpath)))
        if os.path.exists(self.installpath):
            raise SetupError("Installer dir does not seem to be a directory!")
        return True

    def install(self, development, testsuite):
        log ("Running installer:", self.installer)

        cmd = [self.installer, "/S"]

        if not development:
            cmd.append("/NODEVELOPMENT")
        if testsuite:
            cmd.append("/TESTSUITE")

        result = subprocess.call(cmd)

        if result != 0:
            raise SetupError("Installer failed (" + str(result) + ")!")

    def __setup_debug_runtime(self):
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
                                     "debug-runtimes",
                                     "vs" + vs_version,
                                     arch)

        log("Adding", debugcrt_path, "to the PATH")

        if not os.path.isdir(debugcrt_path):
            raise SetupError("The debug runtime directory seems to be missing: " + debugcrt_path)

        os.environ["PATH"] += os.pathsep + debugcrt_path


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
            raise SetupError("bin directory seems to have been added to PATH before installation!:\n"
                             + os.environ["PATH"])
        os.environ["PATH"] += os.pathsep + binpath


        self.__setup_debug_runtime()

        log("Running safir_show_config to test that exes can be run")
        proc = subprocess.Popen(("safir_show_config","--locations", "--typesystem", "--logging"),
                                stdout = subprocess.PIPE,
                                stderr = subprocess.STDOUT,
                                universal_newlines = True)
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run safir_show_config. returncode = "
                             + str(proc.returncode) + "\nOutput:\n" + output)

        if os.path.isdir(os.path.join(self.installpath,"include","boost")):
            raise SetupError("Found unexpected directory 'include/boost'")
        if os.path.isdir(os.path.join(self.installpath,"include","Safir")):
            raise SetupError("Found unexpected directory 'include/Safir'")

class DebianInstaller(object):
    def __init__(self):
        self.packages = ("safir-sdk-core", "safir-sdk-core-dev", "safir-sdk-core-testsuite")

    def __is_installed(self, package_name, cache = None):
        if cache is None:
            cache = apt.cache.Cache()

        return cache.has_key(package_name) and \
          cache[package_name].is_installed

    def can_uninstall(self):
        cache = apt.cache.Cache()
        for pkg in self.packages:
            if self.__is_installed(pkg, cache):
                return True
        return False;

    def uninstall(self):
        if not self.can_uninstall():
            raise SetupError("Cannot uninstall! Packages are not installed!")

        log ("Uninstalling packages: ")

        cmd = ["sudo", "--non-interactive", "apt-get", "--yes", "purge"]
        for pkg in self.packages:
            if self.__is_installed(pkg):
                log (" ", pkg)
                cmd.append(pkg)


        proc = subprocess.Popen(cmd,
                                stdout = subprocess.PIPE,
                                stderr = subprocess.STDOUT)
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run apt-get purge. returncode = "
                             + str(proc.returncode) + "\nOutput:\n" + output)


    def install(self, development, testsuite):
        runtime = glob.glob("safir-sdk-core_*.deb")
        if len(runtime) != 1:
            raise SetupError("Unexpected number of runtime packages: "+ str(pkg))

        packages = [runtime[0],]

        if development:
            pkg = glob.glob("safir-sdk-core-dev_*.deb")
            if len(pkg) != 1:
                raise SetupError("Unexpected number of development packages: "+ str(pkg))
            packages.append(pkg[0])

        if testsuite:
            pkg = glob.glob("safir-sdk-core-testsuite_*.deb")
            if len(pkg) != 1:
                raise SetupError("Unexpected number of testsuite packages: "+ str(pkg))
            packages.append(pkg[0])

        log ("Installing packages", packages)

        proc = subprocess.Popen(["sudo", "dpkg", "--install"] + packages,
                                stdout = subprocess.PIPE,
                                stderr = subprocess.STDOUT)
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run dpkg --install. returncode = "
                             + str(proc.returncode) + "\nOutput:\n" + output)


    def check_installation(self):
        log("Running safir_show_config to test that exes can be run")
        proc = subprocess.Popen(("safir_show_config","--locations", "--typesystem", "--logging"),
                                stdout = subprocess.PIPE,
                                stderr = subprocess.STDOUT,
                                universal_newlines = True)
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run safir_show_config. returncode = "
                             + str(proc.returncode) + "\nOutput:\n" + output)

def run_test_suite():
    log("Launching test suite")
    if sys.platform == "win32":
        result = subprocess.call(("run_dose_tests.py" ,"--jenkins"), shell = True)
    else:
        result = subprocess.call(("run_dose_tests","--jenkins"))

    if result != 0:
        raise SetupError("Test suite failed. Returncode = " + str(result))

def parse_command_line():
    parser = argparse.ArgumentParser()
    parser.add_argument("--skip-install", action="store_true",
                        help="Skip the install step. Useful for running this script when "
                             + " you've already installed Safir SDK Core")

    parser.add_argument("--test", "-t",
                        choices=["standalone-tests","build-examples"],
                        help="Which test to perform")

    arguments = parser.parse_args()

    return arguments

def main():
    args = parse_command_line()

    if not args.skip_install:
        if sys.platform == "win32":
            installer = WindowsInstaller()
        elif sys.platform.startswith("linux") and \
            platform.linux_distribution()[0] in ("debian", "Ubuntu"):
            installer = DebianInstaller()
        else:
            log ("Platform", sys.platform, ",", platform.linux_distribution()," is not supported by this script")
            return 1

        if installer.can_uninstall():
            log("It looks like Safir SDK Core is already installed! Will uninstall and return a failure.")
            installer.uninstall()
            return 1

    try:
        if not args.skip_install:
            development = args.test == "build-examples"
            testsuite = args.test != "build-examples"

            installer.install(development,testsuite)
            installer.check_installation()

        if args.test == "standalone-tests":
            run_test_suite()
        elif args.test == "build-examples":
            log("Not implemented, yet")
            return 1

    except SetupError as e:
        log ("Error: " + str(e))
        return 1
    except Exception as e:
        log ("Caught exception: " + str(e))
        return 1
    finally:
        if not args.skip_install:
            try:
                installer.uninstall()
            except SetupError as e:
                log ("Error: " + str(e))
                return 1
    return 0

sys.exit(main())
