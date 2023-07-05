#!/usr/bin/env python3
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
import signal
import tempfile
import urllib.request
import time
import socket
import datetime

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
    print(datetime.datetime.now().isoformat(), ":", *args, **kwargs)
    sys.stdout.flush()

def nice_call(*popenargs, timeout=None, **kwargs):
    """
    Like subprocess.call(), but give the child process time to
    clean up and communicate if a KeyboardInterrupt is raised.
    """
    with subprocess.Popen(*popenargs, **kwargs) as p:
        try:
            return p.wait(timeout=timeout)
        except KeyboardInterrupt:
            if not timeout:
                timeout = 0.5
            # Wait again, now that the child has received SIGINT, too.
            p.wait(timeout=timeout)
            raise
        except:
            p.kill()
            p.wait()
            raise

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
        result = nice_call((self.uninstaller + " /S _?=" + self.installpath))
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

        result = nice_call(cmd)

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
                                encoding="utf-8")

        output = proc.communicate()[0]
        binpath = os.path.join(self.installpath, "bin")
        if output.find(binpath) == -1:
            raise SetupError("bin directory does not appear to have been added to PATH:\n" + output)
        if os.environ["PATH"].find(os.path.join("Safir SDK Core", "bin")) != -1:
            raise SetupError("bin directory seems to have been added to PATH before installation!:\n" +
                             os.environ["PATH"])

        os.environ["PATH"] += os.pathsep + binpath

        self.__setup_debug_runtime()

        log("Running safir_show_config to test that exes can be run")
        proc = subprocess.Popen(("safir_show_config", "--locations", "--typesystem", "--logging"),
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,
                                encoding="utf-8")
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run safir_show_config. returncode = " + str(proc.returncode) + "\nOutput:\n" +
                             output)

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

        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, encoding="utf-8")
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run apt-get purge. returncode = " + str(proc.returncode) + "\nOutput:\n" +
                             output)

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
                                stderr=subprocess.STDOUT,
                                encoding="utf-8")
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run dpkg --install. returncode = " + str(proc.returncode) + "\nOutput:\n" +
                             output)

    def check_installation(self):
        log("Running safir_show_config to test that exes can be run")
        proc = subprocess.Popen(("safir_show_config", "--locations", "--typesystem", "--logging"),
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,
                                encoding="utf-8")
        output = proc.communicate()[0]
        if proc.returncode != 0:
            raise SetupError("Failed to run safir_show_config. returncode = " + str(proc.returncode) + "\nOutput:\n" +
                             output)

class JenkinsInterface:
    def __init__(self):
        self.server = os.environ.get("JENKINS_URL_OVERRIDE")
        if self.server is None:
            self.server = os.environ.get("JENKINS_URL")
            if self.server is None:
                log("No JENKINS_URL found")
                sys.exit(1)
        self.user = os.environ.get("JENKINS_USER")
        if self.user is None:
            log("No JENKINS_USER found, defaulting to 'jenkins'")
            self.user = "jenkins"
        log("Using jenkins server", self.server)
        log("Using jenkins user", self.user)

        self.log_level = os.environ.get("JENKINS_CLI_LOGGING")
        if self.log_level is None:
            self.log_level = "OFF"

        cliurl = self.server + "/jnlpJars/jenkins-cli.jar"
        self.tempdir = tempfile.TemporaryDirectory()
        #log "will download jenkins-cli.jar using url", cliurl
        self.clijar = os.path.join(self.tempdir.name, "jenkins-cli.jar")
        urllib.request.urlretrieve(cliurl, self.clijar)

    def __run_command(self, cmd, inp=None, name=None):
        args = list()

        args += ("java", "-jar", self.clijar, "-s", self.server, "-logger", self.log_level, "-ssh", "-user", self.user)

        if type(cmd) is str:
            args.append(cmd)
        else:
            args += cmd

        if name is None:
            log(f"Running command {' '.join(args)}")
        else:
            log(f"Running {name}")


        proc = subprocess.Popen(args,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,
                                encoding = "utf-8",
                                stdin=None if inp is None else subprocess.PIPE)
        res = proc.communicate(inp)
        if proc.returncode == 0:
            return res[0]
        else:
            log(res[0])
            raise Exception(f"Failed to run jenkins command '{' '.join(args)}'")

    def __run_groovy(self, name, script):
        return self.__run_command(("groovy", "=", name), inp=script)

    def help(self):
        return self.__run_command("help")

    def who_am_i(self):
        return self.__run_command("who-am-i")

    def list_jobs(self):
        output = self.__run_groovy("list_jobs",
                                   "import hudson.model.*\n" +
                                   "for(item in Hudson.instance.items) {println(item.name)}")
        jobs = output.splitlines()
        return jobs

    def __to_bool(self, output, on_error):
        output = output.strip()
        if output not in ("true", "false"):
            log("Unexpected boolean value: '{0}' was not found in {1}. Treating as {2}".format(
                output, ("true", "false"), on_error))
            return on_error
        return output == "true"

    def is_building(self, job, on_error):
        script = """
                 import hudson.model.*
                 item = Hudson.instance.getItemByFullName("JOB_NAME")
                 println(item.isBuilding())
                 """
        script = script.replace("JOB_NAME", job)
        output = self.__run_groovy("is_building", script)
        return self.__to_bool(output, on_error)

    def is_restarting(self):
        script = """
                 import hudson.model.*
                 println(Hudson.instance.isQuietingDown())
                 """
        output = self.__run_groovy("is_restarting", script)
        return self.__to_bool(output, True)

    def wait_for_job(self, job, duration=None):
        if duration is None:
            while self.is_building(job, True):
                time.sleep(1.0)
        else:
            future = time.time() + duration
            while time.time() < future and self.is_building(job, True):
                time.sleep(1.0)

    def build(self, job, parameters = None):
        command = ("build", job, "-w")
        if parameters is not None:
            for key,value in parameters.items():
                command += ("-p", f"{key}={value}")
        self.__run_command(command)
        while not self.is_building(job, False):
            time.sleep(1.0)

    def cancel_job(self, job):
        script = """
                 import hudson.model.*
                 item = Hudson.instance.getItemByFullName("JOB_NAME")
                 executor = item.getLastBuild().getExecutor()
                 if (executor != null)
                 {
                   executor.interrupt()
                 }
                 """
        script = script.replace("JOB_NAME", job)
        self.__run_groovy("cancel_job", script)

    def get_console_output(self, job):
        script = """
                 import hudson.model.*
                 item = Hudson.instance.getItemByFullName("JOB_NAME")
                 build = item.getLastBuild()
                 text = build.getLogText()
                 println ("text length " + text.length())
                 println(build.getLog())
                 println ("end log")
                 """
        script = script.replace("JOB_NAME", job)
        log(self.__run_groovy("get_console_output",script))

class JenkinsController:
    def __init__(self, slave_role):
        self.interface = JenkinsInterface()
        self.slave_role = slave_role
        self.job_name = "multicomputer-test-slaves/multicomputer-test-" + slave_role
        auth = self.interface.who_am_i()
        if auth.find("authenticated") == -1:
            log("Failed to authenticate using ssh keys, please check that keys are set up correctly")
            log(auth)
            sys.exit(1)

    def is_restarting(self):
        if self.interface.is_restarting():
            log(" !! Jenkins is restarting")
            return True
        else:
            return False

    def start_slave(self):
        log(" * Starting slave")
        #wait for any previous run to complete
        self.interface.wait_for_job(self.job_name)

        # Get the build number and the job name for the slave to copy artifacts from
        # from Jenkins environment variables
        parameters = {"SOURCE_PROJECT" : os.environ.get("JOB_NAME"),
                      "SOURCE_BUILD_NUMBER" : os.environ.get("BUILD_NUMBER"),
                      "SLAVE_ROLE": self.slave_role}
        self.interface.build(self.job_name, parameters)

    def __stop_slave(self):
        try:
            self.interface.wait_for_job(self.job_name, 60)
            self.interface.cancel_job(self.job_name)
            self.interface.wait_for_job(self.job_name)
        except Exception as exc:
            log("Failed to stop slave!", exc)

    def close(self):
        self.__stop_slave()

def run_test_suite(kind):
    log("Launching test suite")
    arguments = [
        "--jenkins",
    ]
    try:
        server_1 = None
        client_0 = None
        client_1 = None

        if os.environ["JOB_NAME"].find("32on64") != -1:
            arguments += ("--no-java", )
        if kind == "multinode":
            arguments += ("--multinode", )
        if kind == "multicomputer":
            server_1 = JenkinsController("server-1")
            client_0 = JenkinsController("client-0")
            client_1 = JenkinsController("client-1")
            if server_1.is_restarting():
                log("Jenkins is restarting, exiting quickly...")
                return
            server_1.start_slave()
            client_0.start_slave()
            client_1.start_slave()
            arguments += ("--multicomputer", "--stop-slaves")

        log(f"Launching test suite with arguments {arguments}")
        if sys.platform == "win32":
            result = nice_call([
                "run_dose_tests.py",
            ] + arguments, shell=True)
        else:
            result = nice_call([
                "run_dose_tests",
            ] + arguments)
    finally:
        if server_1 is not None:
            server_1.close()
        if client_0 is not None:
            client_0.close()
        if client_1 is not None:
            client_1.close()

    if result != 0:
        raise SetupError("Test suite failed. Returncode = " + str(result))


def run_test_slave(slave_type):
    command = ["run_dose_tests", "--jenkins", "--slave", slave_type]
    log(f"Launching Multinode test slave using command {' '.join(command)}")
    result = nice_call(command)

    if result != 0:
        raise SetupError(f"Test suite failed ({slave_type}). Returncode = {str(result)}")


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
        dope_result = nice_call([
            "run_dope_odbc_backend_test.py",
        ] + args, shell=True)
    else:
        dope_result = nice_call([
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
        result = nice_call(cmd, shell=sys.platform == "win32")
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

    parser.add_argument("--slave",
                        choices=["server-1", "client-0", "client-1"],
                        help="Be a multicomputer test slave of the specified type")

    arguments = parser.parse_args()

    return arguments


def main():
    signal.signal(signal.SIGINT, signal.default_int_handler)
    signal.signal(signal.SIGTERM, signal.default_int_handler)
    args = parse_command_line()
    log(f"Running on '{socket.gethostname()}'")
    result = 0
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
            run_test_slave(args.slave)

    except SetupError as e:
        log("Error: " + str(e))
        result = 1
    except KeyboardInterrupt:
        log("Got a signal to stop!")
        result = 1
    except Exception as e:
        log("Caught exception: " + str(e))
        result = 1
    finally:
        if not args.skip_install:
            try:
                installer.uninstall()
            except SetupError as e:
                log("Error: " + str(e))
                result = 1
    return result


sys.exit(main())
