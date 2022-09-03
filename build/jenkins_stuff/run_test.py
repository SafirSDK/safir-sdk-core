#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2014-2015 (http://safirsdkcore.com)
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
import os
import glob
import sys
import subprocess
import re
import platform
import argparse

try:
    import apt
except:
    pass

#Make linux_distribution available from some suitable package, falling back to returning that we don't know.
try:
    from distro import linux_distribution
except:
    try:
        from platform import linux_distribution
    except:
        def linux_distribution():
            return ("unknown",)

def log(*args, **kwargs):
    """logging function that flushes"""
    print(*args, **kwargs)
    sys.stdout.flush()


class SetupError(Exception):
    pass


class WindowsInstaller():
    def __init__(self):

        installer = glob.glob("SafirSDKCore*.exe")
        if len(installer) != 1:
            raise SetupError("Unexpected number of installers: " + str(installer))
        self.installer = installer[0]
        self.uninstaller = None

        if self.installer.find("32bit") != -1 and os.environ.get("ProgramFiles(x86)") is not None:
            self.installpath = os.path.join(os.environ["ProgramFiles(x86)"], "Safir SDK Core")
        else:
            self.installpath = os.path.join(os.environ["ProgramFiles"], "Safir SDK Core")

        log("Found installer = ", self.installer)
        log("Using installpath = ", self.installpath)

    def can_uninstall(self):
        #we can't use self.installpath, since it may not be installed there if it was
        #left over from previous installation
        ip = os.path.join(os.environ["ProgramFiles"], "Safir SDK Core")
        uninstaller = os.path.join(ip, "Uninstall.exe")
        installed = os.path.isfile(uninstaller) and len(os.listdir(ip)) > 1

        pf86 = os.environ.get("ProgramFiles(x86)")

        if pf86 is None:
            if installed:
                self.uninstaller = uninstaller
        else:
            ip86 = os.path.join(pf86, "Safir SDK Core")
            uninstaller86 = os.path.join(ip86, "Uninstall.exe")
            installed86 = os.path.isfile(uninstaller86) and len(os.listdir(ip86)) > 1

            if installed86 and installed:
                raise SetupError("Multiple installs found!")
            elif installed:
                self.uninstaller = uninstaller
            elif installed86:
                self.uninstaller = uninstaller86

        return self.uninstaller is not None

    def uninstall(self):
        if not self.can_uninstall():
            raise SetupError("No uninstaller found!")

        log("Running uninstaller:", self.uninstaller)
        #The _? argument requires that we concatenate the command like this instead of using a tuple
        result = subprocess.call((self.uninstaller + " /S _?=" + self.installpath))
        if result != 0:
            raise SetupError("Uninstaller failed (" + str(result) + ")!")

        if os.path.isdir(os.path.join(self.installpath, "dou")):
            raise SetupError("Installer dir " + self.installpath + " still exists after uninstallation! Contents:\n" +
                             str(os.listdir(self.installpath)))
        uninstallerdir = os.path.dirname(self.uninstaller)
        if os.path.isdir(os.path.join(uninstallerdir, "dou")):
            raise SetupError("Uninstaller dir " + uninstallerdir + " still exists after uninstallation! Contents:\n" +
                             str(os.listdir(uninstallerdir)))

        return True

    def install(self, development, testsuite):
        log("Running installer:", self.installer)

        cmd = [self.installer, "/S"]

        if not development:
            cmd.append("/NODEVELOPMENT")
        if testsuite:
            cmd.append("/TESTSUITE")

        result = subprocess.call(cmd)

        if result != 0:
            raise SetupError("Installer failed (" + str(result) + ")!")

        self.development_installed = development

    def __setup_debug_runtime(self):
        #we get out of here immediately if we're not running debug.
        if os.environ.get("BUILD_TYPE") != "DebugOnly":
            return

        #build machines should have debug runtime on them
        if platform.node().find("-build") != -1:
            return

        #Work out studio version and bitness from installer name
        match = re.search(r"SafirSDKCore-.*-VS([0-9]*)-([0-9]*)bit-DebugOnly.exe", self.installer)
        vs_version = match.group(1)
        width = match.group(2)

        if width == "32":
            arch = "x86"
        elif width == "64":
            arch = "x64"

        debugcrt_path = os.path.join("c:", os.sep, "debug-runtimes", "vs" + vs_version, arch)

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
        if all(dir in listdir for k in ("bin", "installer_utils", "include", "lib")):
            raise SetupError("Could not find some expected directory in installation directory: " + str(listdir))

        #Check that bin directory is in path
        pathed = os.path.join(self.installpath, "installer_utils", "pathed")

        proc = subprocess.Popen((pathed, "/machine"),
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,
                                universal_newlines=True)

        output = proc.communicate()[0]
        binpath = os.path.join(self.installpath, "bin")
        if output.find(binpath) == -1:
            raise SetupError("bin directory does not appear to have been added to PATH:\n" + output.decode("utf-8"))
        if os.environ["PATH"].find(os.path.join("Safir SDK Core", "bin")) != -1:
            raise SetupError("bin directory seems to have been added to PATH before installation!:\n" +
                             os.environ["PATH"])

        os.environ["PATH"] += os.pathsep + binpath

        self.__setup_debug_runtime()

        log("Running safir_show_config to test that exes can be run")
        proc = subprocess.Popen(("safir_show_config", "--locations", "--typesystem", "--logging"),
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,
                                universal_newlines=True)
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run safir_show_config. returncode = " + str(proc.returncode) + "\nOutput:\n" +
                             output.decode("utf-8"))

        if not self.development_installed:
            if os.path.isdir(os.path.join(self.installpath, "include", "Safir", "Dob")):
                raise SetupError("Found unexpected directory 'include/Safir/Dob'")

class DebianInstaller():
    def __init__(self):
        self.packages = ("safir-sdk-core", "safir-sdk-core-tools", "safir-sdk-core-dev", "safir-sdk-core-testsuite")

    def __is_installed(self, package_name, cache=None):
        if cache is None:
            cache = apt.cache.Cache()

        return cache.has_key(package_name) and \
          cache[package_name].is_installed

    def can_uninstall(self):
        cache = apt.cache.Cache()
        for pkg in self.packages:
            if self.__is_installed(pkg, cache):
                return True
        return False

    def uninstall(self):
        if not self.can_uninstall():
            raise SetupError("Cannot uninstall! Packages are not installed!")

        log("Uninstalling packages: ")

        cmd = ["sudo", "--non-interactive", "apt-get", "--yes", "purge"]
        for pkg in self.packages:
            if self.__is_installed(pkg):
                log(" ", pkg)
                cmd.append(pkg)

        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run apt-get purge. returncode = " + str(proc.returncode) + "\nOutput:\n" +
                             output.decode("utf-8"))

    def install(self, development, testsuite):
        runtime = glob.glob("safir-sdk-core_*.deb")
        if len(runtime) != 1:
            raise SetupError("Unexpected number of runtime packages: " + str(runtime))

        tools = glob.glob("safir-sdk-core-tools*.deb")
        if len(tools) != 1:
            raise SetupError("Unexpected number of tools packages: " + str(tools))

        packages = [runtime[0], tools[0]]

        if development:
            pkg = glob.glob("safir-sdk-core-dev_*.deb")
            if len(pkg) != 1:
                raise SetupError("Unexpected number of development packages: " + str(pkg))
            packages.append(pkg[0])

        if testsuite:
            pkg = glob.glob("safir-sdk-core-testsuite_*.deb")
            if len(pkg) != 1:
                raise SetupError("Unexpected number of testsuite packages: " + str(pkg))
            packages.append(pkg[0])

        log("Installing packages", packages)

        proc = subprocess.Popen(["sudo", "dpkg", "--install"] + packages,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT)
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run dpkg --install. returncode = " + str(proc.returncode) + "\nOutput:\n" +
                             output.decode("utf-8"))

    def check_installation(self):
        log("Running safir_show_config to test that exes can be run")
        proc = subprocess.Popen(("safir_show_config", "--locations", "--typesystem", "--logging"),
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,
                                universal_newlines=True)
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run safir_show_config. returncode = " + str(proc.returncode) + "\nOutput:\n" +
                             output.decode("utf-8"))


def run_test_suite(kind):
    log("Launching test suite")
    arguments = [
        "--jenkins",
    ]
    if os.environ["JOB_NAME"].find("32on64") != -1:
        arguments += ("--no-java", )
    if kind == "multinode":
        arguments += ("--multinode", )
    if kind == "multicomputer":
        arguments += ("--multicomputer", )
    if sys.platform == "win32":
        result = subprocess.call([
            "run_dose_tests.py",
        ] + arguments, shell=True)
    else:
        result = subprocess.call([
            "run_dose_tests",
        ] + arguments)

    if result != 0:
        raise SetupError("Test suite failed. Returncode = " + str(result))


def run_test_slave():
    log("Launching Multinode test slave")
    arguments = ["--jenkins", "--slave"]
    result = subprocess.call([
        "run_dose_tests",
    ] + arguments)

    if result != 0:
        raise SetupError("Test suite failed. Returncode = " + str(result))


def run_database_tests():
    log("Running Dope tests")
    if os.environ["Driver"] == "ms-sql":
        hostname = "ms-sql-server\\SQLEXPRESS"
    else:
        hostname = "databases"
    args = [
        "--driver", os.environ["Driver"], "--hostname", hostname, "--database",
        os.environ.get("label").replace("-", "")
    ]
    if sys.platform == "win32":
        dope_result = subprocess.call([
            "run_dope_odbc_backend_test.py",
        ] + args, shell=True)
    else:
        dope_result = subprocess.call([
            "run_dope_odbc_backend_test",
        ] + args)

    log("Dope tests result:", dope_result)

    if dope_result != 0:
        raise Exception("Database tests failed")


def build_examples():
    olddir = os.getcwd()
    dirs = (("examples", None), ("src/dots/dots_dobmake.ss/tests/tree", None),
            ("src/dots/dots_dobmake.ss/tests/many_dous", None), ("src/dots/dots_dobmake.ss/tests/separate_dirs/dous_1",
                                                                 os.path.join(olddir, "inst")))
    #We don't test the dous_2 and dous_3 builds, since it is just too fiddly to get automated.

    for (builddir, installdir) in dirs:
        os.chdir(builddir)

        if sys.platform == "win32":
            cmd = [
                "dobmake-batch.py",
            ]
        else:
            cmd = [
                "dobmake-batch",
            ]

        cmd += ("--verbose", "--jenkins", "--skip-tests")

        if sys.platform == "win32":
            cmd += ("--use-studio", os.environ["BUILD_PLATFORM"])
            cmd += ("--arch", os.environ["BUILD_ARCH"])

        if installdir is not None:
            cmd += ("--install", installdir)

        log("Running command ", " ".join(cmd))
        result = subprocess.call(cmd, shell=sys.platform == "win32")
        if result != 0:
            raise SetupError("Build examples failed. Returncode = " + str(result))

        os.chdir(olddir)


def parse_command_line():
    parser = argparse.ArgumentParser()
    parser.add_argument("--skip-install",
                        action="store_true",
                        help="Skip the install step. Useful for running this script when " +
                        " you've already installed Safir SDK Core")

    parser.add_argument(
        "--test",
        "-t",
        choices=["standalone-tests", "multinode-tests", "multicomputer-tests", "build-examples", "database"],
        help="Which test to perform")

    parser.add_argument("--slave", action="store_true", help="Be a multinode test slave")

    arguments = parser.parse_args()

    return arguments


def main():
    args = parse_command_line()

    if not args.skip_install:
        if sys.platform == "win32":
            installer = WindowsInstaller()
        elif sys.platform.startswith("linux") and \
             linux_distribution()[0] in  ("Debian GNU/Linux", "Ubuntu"):
            installer = DebianInstaller()
        else:
            log("Platform", sys.platform, ",", linux_distribution(), " is not supported by this script")
            return 1

        if installer.can_uninstall():
            log("It looks like Safir SDK Core is already installed! Will uninstall and return a failure.")
            installer.uninstall()
            return 1

    try:
        if not args.skip_install:
            development = args.test == "build-examples"
            testsuite = args.test != "build-examples"

            installer.install(development, testsuite)
            installer.check_installation()

        if args.test == "standalone-tests":
            run_test_suite(kind="standalone")
        if args.test == "multinode-tests":
            run_test_suite(kind="multinode")
        if args.test == "multicomputer-tests":
            run_test_suite(kind="multicomputer")
        elif args.test == "build-examples":
            build_examples()
        elif args.test == "database":
            run_database_tests()
        elif args.slave:
            run_test_slave()

    except SetupError as e:
        log("Error: " + str(e))
        return 1
    except Exception as e:
        log("Caught exception: " + str(e))
        return 1
    finally:
        if not args.skip_install:
            try:
                installer.uninstall()
            except SetupError as e:
                log("Error: " + str(e))
                return 1
    return 0


sys.exit(main())
