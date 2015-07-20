#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2009-2014 (http://safir.sourceforge.net)
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
import os, glob, sys, subprocess, platform, xml.dom.minidom, re, time, shutil, argparse
import locale, codecs
from xml.sax.saxutils import escape

#a few constants
known_configs = set(["Release", "Debug", "MinSizeRel", "RelWithDebInfo"])

#our own exception for "die" fcn
class FatalError(Exception):
    pass

def die(message):
    raise FatalError(message)

def is_64_bit():
    """Detecting this is a lot more complex than it should be.
    See http://stackoverflow.com/questions/2764356/python-get-windows-os-version-and-architecture
    and http://bytes.com/topic/python/answers/509764-detecting-64bit-vs-32bit-linux
    This will work reasonably well on our supported systems:"""
    if sys.platform.startswith("linux"):
        return platform.architecture()[0] == "64bit"
    else:
        PROCESSOR_ARCHITECTURE = os.environ.get("PROCESSOR_ARCHITECTURE")
        PROCESSOR_ARCHITEW6432 = os.environ.get("PROCESSOR_ARCHITEW6432")
        return PROCESSOR_ARCHITECTURE == "AMD64" or PROCESSOR_ARCHITEW6432 == "AMD64"

def cmake():
    """Get the name of the cmake executable. Currently only detects the cmake28/cmake difference on
    centos/rhel 6"""
    if not hasattr(cmake, "cmake_executable"):
        try:
            subprocess.Popen(("cmake28", "--version"), stdout = subprocess.PIPE).communicate()
            cmake.cmake_executable = "cmake28"
        except:
            cmake.cmake_executable = "cmake"
    return cmake.cmake_executable

def ctest():
    """Get the name of the ctest executable. Currently only detects the ctest28/ctest difference on
    centos/rhel 6"""
    if not hasattr(ctest, "ctest_executable"):
        try:
            subprocess.Popen(("ctest28", "--version"), stdout = subprocess.PIPE).communicate()
            ctest.ctest_executable = "ctest28"
        except:
            ctest.ctest_executable = "ctest"
    return ctest.ctest_executable

def mkdir(newdir):
    """works the way a good mkdir should :)
        - already exists, silently complete
        - regular file in the way, raise an exception
        - parent directory(ies) does not exist, make them as well
    """
    if os.path.isdir(newdir):
        pass
    elif os.path.isfile(newdir):
        raise OSError("a file with the same name as the desired " \
                      "dir, '%s', already exists." % newdir)
    else:
        head, tail = os.path.split(newdir)
        if head and not os.path.isdir(head):
            mkdir(head)
        if tail:
            os.mkdir(newdir)


def remove(path):
    if not os.path.exists(path):
        return
    if os.path.isfile(path):
        try:
            os.remove(path)
            return
        except Exception as e:
            die ("Failed to remove file " + path + ". Got exception " + str(e))

    for name in os.listdir(path):
        if os.path.isdir(os.path.join(path, name)):
            remove(os.path.join(path, name))
        else:
            try:
                os.remove(os.path.join(path, name))
            except Exception as e:
                die ("Failed to remove file " + os.path.join(path, name) + ". Got exception " + str(e))

    try:
        os.rmdir(path)
    except Exception as e:
        die ("Failed to remove directory " + path + ". Got exception " + str(e))


def num_cpus():
    """Detects the number of CPUs on a system. Cribbed from pp."""
    # Linux, Unix and MacOS:
    if hasattr(os, "sysconf"):
        if "SC_NPROCESSORS_ONLN" in os.sysconf_names:
            # Linux & Unix:
            ncpus = os.sysconf("SC_NPROCESSORS_ONLN")
            if isinstance(ncpus, int) and ncpus > 0:
                return ncpus
        else: # OSX:
            return int(os.popen2("sysctl -n hw.ncpu")[1].read())
    # Windows:
    if "NUMBER_OF_PROCESSORS" in os.environ:
        ncpus = int(os.environ["NUMBER_OF_PROCESSORS"])
        if ncpus > 0:
            return ncpus
    return 1 # Default

def physical_memory():
    if sys.platform.startswith("linux"):
        with open("/proc/meminfo") as a_file:
            meminfo = a_file.read()
        match = re.search(r"MemTotal:\s*([0-9]*) kB", meminfo)
        return int(match.group(1))/1024
    else:
        return None

def num_jobs():
    #We need to limit ourselves a little bit in how
    #many parallel jobs we perform. Each job may use
    #up to 400Mb of memory.
    try:
        num_jobs = num_cpus() + 1

        mem_per_job = 400
        memory = physical_memory()
        if memory is not None and memory / num_jobs < mem_per_job:
            num_jobs = max(1, int(memory / mem_per_job))
    except:
        num_jobs = 2
    return num_jobs

def read_version():
    parts = {}
    with open("VERSION.txt", 'r') as version_file:
        for line in version_file:
            line = line.strip()
            if len(line) == 0 or line.startswith("#"):
                continue
            key, value = line.split("=")
            parts[key] = value
    return ((parts["MAJOR"],parts["MINOR"],parts["PATCH"],parts["SUFFIX"]),
            parts["MAJOR"] + "." + parts["MINOR"] + "." + parts["PATCH"] + parts["SUFFIX"])

class DummyLogger(object):
    def log(self, data, tag = None):
        sys.stdout.write(data + "\n")
        sys.stdout.flush()

    def close(self):
        pass

    def log_output(self, process):
        raise Exception("DummyLogger doesnt support process output logging. " +
                        "You should investigate why the real logger is not instantiated by now...")

class Logger(object):
    LogLevel = ("Brief", "Verbose")
    Tags = set(["header", "brief","normal","detail", "command_description", "command","output"])

    def __init__(self,level):
        if level not in Logger.LogLevel:
            die("Bad log level")
        self.__log_level = level
        self.__last_tag = None

        self.__buildlog = codecs.open("buildlog.html", mode = "w", encoding = "utf-8")
        self.__buildlog.write("<html><head>"
                              "<script language=\"JavaScript\">"
                              "function readCookie(name){"
                              "  return(document.cookie.match('(^|; )'+name+'=([^;]*)')||0)[2]"
                              "}"
                              "</script>"
                              "<title>Safir SDK Core Build Log</title>"
                              "<meta http-equiv=\"refresh\" content=\"10\" ></head>\n")
        self.__buildlog.write("<body onScroll=\"document.cookie='ypos=' + window.pageYOffset\" onLoad=\"window.scrollTo(0,readCookie('ypos'))\">\n")
        self.__buildlog.write("<h1>Safir SDK Core Build Log</h1>")
        self.__buildlog.write("<b>Command line:</b> " + " ".join(sys.argv) + "<br/>")
        self.__buildlog.write("<b>Working directory:</b> " + os.getcwd() + "<br/>")
        self.__buildlog.write("<b>Start time (local time)</b>: " + time.asctime() + "<br/>")
        self.__buildlog.write("<h2>Starting build</h2>\n")

    def close(self):
        self.__buildlog.write("\n<p/>End time (local time): " + time.asctime())
        self.__buildlog.write("\n</body>\n")
        self.__buildlog.close()

    def __print(self,data):
        if sys.version_info[0] < 3:
            data = data.encode("utf-8")
        sys.stdout.write(data)
        sys.stdout.write("\n")
        sys.stdout.flush()

    def __log_stdout(self, data, tag):
        if tag not in Logger.Tags:
            die("unknown logging tag")
        #data = data.encode("utf-8")
        if self.__log_level == "Brief":
            if tag == "header" or tag == "normal" or tag == "brief":
                self.__print(data)
        elif self.__log_level == "Verbose":
            if tag == "brief":
                pass
            elif tag == "header":
                self.__print("\n==== " + data + " ====")
            elif tag == "command_description":
                self.__print("+ " + data + ": ")
            elif tag == "command":
                self.__print("'" + data + "'")
            else:
                self.__print(data)


    def __log_file(self, data, tag):
        log = self.__buildlog

        if self.__last_tag == "output" and self.__last_tag != tag:
            log.write("</pre>\n")

        if tag == "header":
            log.write("<h3>" + data + "</h3>\n")
        elif tag == "brief":
            pass
        elif tag == "normal":
            log.write(data + "<br/>\n")
        elif tag == "detail":
            log.write(data + "<br/>\n")
        if tag == "command_description":
            log.write("<h4>" + data + "</h4>\n")
        elif tag == "command":
            log.write("<pre style=\"color: green\">" + data + "</pre>\n")
        elif tag == "output":
            if self.__last_tag != tag:
                log.write("<pre>" + data)
            else:
                log.write("\n" + data)
        log.flush()
        self.__last_tag = tag

    def log(self, data, tag = "normal"):
        if data is None: return

        self.__log_stdout(data,tag)
        self.__log_file(data,tag)


    def log_output(self, process):
        output = list()
        while True:
            line = process.stdout.readline()
            if not line:
                break
            #CMake does some strange thing with a carriage return alone on a line, which we get rid of like this.
            line = line.decode("utf8").rstrip("\r")
            if len(line) != 0:
                line = line.rstrip()
                self.log(line,"output")
                output += (line,)
        process.wait()
        if process.returncode != 0:
            self.log("Failure, return code is " + str(process.returncode))
        self.log("","output")
        return "\n".join(output)

def suppress(help_string):
    """
    Use this function to wrap help strings for arguments that should not be visible
    in the help when this script is run as dobmake-batch.
    """
    try:
        if os.path.basename(__file__).startswith("dobmake-batch"):
            return argparse.SUPPRESS
    except:
        pass
    return help_string

def add_win32_options(parser):
    parser.add_argument("--use-studio",
                        help="The visual studio to use for building, can be '2010', '2012' or '2013'")
    if is_64_bit():
        parser.add_argument("--32-bit",
                            action="store_true",
                            dest="build_32_bit",
                            default=False,
                            help="Build a 32 bit system even though this machine is 64 bit.")
    parser.add_argument("--configs",
                        default=("Debug", "RelWithDebInfo"),
                        nargs='*',
                        choices=known_configs,
                        help="The configurations to build. Debug and RelWithDebInfo is the default.")

def add_linux_options(parser):
    parser.add_argument("--config",
                        dest="configs",
                        nargs=1,
                        default=("RelWithDebInfo",),
                        choices=known_configs,
                        help="The configuration to build. RelWithDebInfo is the default.")



def parse_command_line():
    parser = argparse.ArgumentParser()
    action = parser.add_mutually_exclusive_group()

    action.add_argument("--package",
                        action="store_true",
                        default=False,
                        help=suppress("Build everything and package the results for the current platform."))

    action.add_argument("--install",
                        metavar="PATH",
                        help="Build the source in the current directory and install it to "
                             "PATH. If PATH is set to 'None' the install step will be run "
                             "without setting CMAKE_INSTALL_PREFIX, useful if your "
                             "CMakeLists.txt has absolute paths in the INSTALL directives.")

    parser.add_argument("--skip-tests",
                        action = "store_true",
                        help=suppress("Skip running the unit tests"))

    parser.add_argument("--jenkins",
                        action="store_true",
                        default=False,
                        help=suppress("Increase verbosity and obey build matrix variables."))

    parser.add_argument("--verbose", "-v",
                        action="count",
                        default=0,
                        help="Print more stuff about what is going on. Use twice to get very verbose output.")

    if sys.platform.startswith("linux"):
        add_linux_options(parser)
    else:
        add_win32_options(parser)

    arguments = parser.parse_args()

    if arguments.jenkins:
        arguments.verbose += 1
    if arguments.verbose >= 2:
        os.environ["VERBOSE"] = "1"

    global logger
    logger = Logger("Brief" if arguments.verbose == 0 else "Verbose")

    return arguments

class BuilderBase(object):
    def __init__(self, arguments):
        self.num_jobs = num_jobs()

        self.total_tests = 0
        self.failed_tests = 0

        self.arguments = arguments
        self.__handle_command_line_arguments()

    def __handle_command_line_arguments(self):
        self.configs = self.arguments.configs

        self.debug_only = False
        if self.arguments.jenkins:
            if os.environ.get("Config") == "DebugOnly":
                logger.log("Using Config 'DebugOnly', building everything in Debug only.")
                self.configs = ("Debug",)
                self.debug_only = True

        self.stagedir = os.path.join(os.getcwd(),"stage") if self.arguments.package else None

        self.install_prefix = None #derived classes can override if arguments.package is true
        if self.arguments.install and self.arguments.install != "None":
            self.install_prefix = self.arguments.install

    def build(self):
        for config in self.configs:
            olddir = os.getcwd()
            mkdir(config)
            os.chdir(config)

            self.__build_internal(os.pardir,
                                  config)
            os.chdir(olddir)

        if self.arguments.package:
            self.__package()

    def __package(self):
        try:
            import stage_dependencies
            stage_dependencies.stage_dependencies(logger, self.stagedir)
        except stage_dependencies.StagingError as e:
            raise FatalError("Error while copying dependencies to staging area: " + str(e))

        logger.log("Building installation package", "header")
        self.stage_package()

    def __configure(self, srcdir, config):

        command = (cmake(),
                   "-G", self.cmake_generator,
                   "-D", "CMAKE_BUILD_TYPE:string=" + config)

        if self.install_prefix is not None:
            command += ("-D", "CMAKE_INSTALL_PREFIX=" + self.install_prefix)

        command += (srcdir,)
        self._run_command(command,
                           "Configure for " + config + " build")

    def __build_internal(self, srcdir, config):
        logger.log(" - in config " + config, "brief")

        self.__configure(srcdir, config)

        command = (cmake(),
                   "--build", ".",
                   "--") + self.generator_specific_build_cmds()


        self._run_command(command,
                           "Build " + config)
        if not self.arguments.skip_tests:
            logger.log("   + testing", "brief")
            self.test()
            translate_results_to_junit(config)

        if self.arguments.package:
            logger.log("   + installing to staging area", "brief")
            self.__stage_install()
        elif self.arguments.install:
            self.__install()

    def __stage_install(self):
        for component in ("Runtime", "Development", "Test"):
            command = (cmake(),
                    "-DCOMPONENT="+ component,
                    "-P", "cmake_install.cmake")
            env = os.environ.copy()
            env["DESTDIR"] = os.path.join(self.stagedir,component)
            self._run_command(command,
                               "Staged install " + component, env = env)

    def __install(self):
        command = (cmake(),
                   "-P", "cmake_install.cmake")
        self._run_command(command,
                          "Installing to " + self.arguments.install)
    def stage_package(self):
        logger.log(" ! Packaging not implemented in this builder !","brief")

    def test(self):
        """run ctest in current directory"""
        if not os.path.isfile("DartConfiguration.tcl"):
            dummyfile = open("DartConfiguration.tcl","w")
            dummyfile.close()

        output = self._run_command((ctest(),
                                    "-V",
                                    "-T", "Test",
                                    "--no-compress-output"),
                                    "Test", allow_fail = True)
        self.interpret_test_output(output)


    def _run_command(self, cmd, description, allow_fail = False, env = None):
        """Run a command"""

        logger.log(description, "command_description")
        logger.log(" ".join(cmd), "command")

        process = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env = env)
        output = logger.log_output(process)
        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + " ".join(cmd) + "' in " + os.getcwd())
            else:
                logger.log("This command failed, but failure of this particular command " +
                           "is non-fatal to the build process, so I'm continuing")

        return output

    def interpret_test_output(self,output):
        logger.log("Checking test output")
        match = re.search(r"tests passed, ([0-9]+) tests failed out of ([0-9]+)",output)
        if not match:
            if output.find("No tests were found") == -1:
                logger.log("Failed to parse test output!")
            return
        failed = int(match.group(1))
        tests = int(match.group(2))
        self.total_tests += tests
        self.failed_tests += failed

class VisualStudioBuilder(BuilderBase):
    def __init__(self, arguments):
        super(VisualStudioBuilder, self).__init__(arguments)

        self.install_target = "Install"

        # Use Ninja for building if available, it is much faster
        try:
            subprocess.Popen(("ninja", "--version"), stdout = subprocess.PIPE).communicate()
            self.cmake_generator = "Ninja"
            self.have_ninja = True
        except:
            self.cmake_generator = "NMake Makefiles"
            self.have_ninja = False

        self.__handle_command_line_arguments()

        self.__setup_build_environment()
    @staticmethod
    def can_use():
        return sys.platform == "win32"

    def __handle_command_line_arguments(self):
        self.use_studio = self.arguments.use_studio

        if not is_64_bit() or self.arguments.build_32_bit:
            self.target_architecture = "x86"
        else:
            self.target_architecture = "x86-64"


    def generator_specific_build_cmds(self):
        if self.have_ninja:
            return () #empty tuple
        else:
            return ("/nologo",)

    def __find_vcvarsall(self):
        install_dirs = {"VS120COMNTOOLS" : "2013",
                        "VS110COMNTOOLS" : "2012",
                        "VS100COMNTOOLS" : "2010"}

        # If use_studio is specified we change the list to only contain that vs.
        if self.use_studio is not None:
            found = False
            for dir, ver in install_dirs.items():
                if self.use_studio == ver:
                    install_dirs = {dir : ver}
                    found = True
                    break
            if not found:
                die("The version of visual studio that you asked for is not supported.")

        if len(install_dirs) < 1:
            die("Internal error in __find_vcvarsall(...)")

        for install_dir, version in install_dirs.items():
            env = os.environ.get(install_dir)
            if env is not None:
                self.used_studio = version
                break
        if env is None:
            die ("Failed to find Visual Studio install dir, "
                 + "checked the following environment variables: " + str(install_dirs))
        result = os.path.join(env,os.pardir,os.pardir,"VC","vcvarsall.bat")
        if not os.path.isfile(result):
            die("No such file: " + result)
        return result

    def __run_vcvarsall(self, vcvarsall, arch):
        cmd = '"%s" %s & set' % (vcvarsall, arch)
        logger.log("Running '" + cmd + "' to extract environment")
        proc = subprocess.Popen(cmd,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,
                                universal_newlines = True)
        output = proc.communicate()[0]
        if proc.returncode != 0:
            die ("Failed to fetch environment variables out of vcvarsall.bat: " + output)
        return output

    def __setup_build_environment(self):
        """
        Find vcvarsall.bat and load the relevant environment variables from it.  This function
        is inspired (but not copied, for licensing reasons) by the one in python
        distutils2 msvc9compiler.py
        """

        vcvarsall = self.__find_vcvarsall()

        #use uppercase only in this variable!
        required_variables = set(["LIB", "LIBPATH", "PATH", "INCLUDE", "VSINSTALLDIR"])
        optional_variables = set(["PLATFORM",])
        wanted_variables = required_variables | optional_variables #union

        logger.log("Loading Visual Studio Environment","header")
        output = self.__run_vcvarsall(vcvarsall, "x86" if self.target_architecture == "x86" else "amd64")

        #retry with cross compilation toolset if we're on amd64 and vcvarsall says the toolset is missing
        if self.target_architecture == "x86-64" and output.find("configuration might not be installed") != -1:
            logger.log("Native toolset appears to be missing, trying cross compilation")
            output = self.__run_vcvarsall(vcvarsall, "x86_amd64")

        found_variables = set()

        for line in output.split("\n"):
            if '=' not in line:
                continue
            line = line.strip()
            name, value = line.split('=', 1)
            name = name.upper()
            if name in wanted_variables:
                if value.endswith(os.pathsep):
                    value = value[:-1]
                if os.environ.get(name) is None:
                    logger.log("Will set '" + name + "' to '" + value + "'", "detail")
                else:
                    logger.log("Will change '" + name + "' from '" + os.environ.get(name)
                               + "' to '" + value + "'", "detail")
                os.environ[name] = value
                found_variables.add(name)

        if len(required_variables - found_variables) != 0:
            die("Failed to find all expected variables in vcvarsall.bat")

    def stage_package(self):
        version_tuple, version_string = read_version()
        command = ("makensis",
                   "/DARCH=" + self.target_architecture,
                   "/DSTUDIO=" + self.used_studio,
                   "/DVERSION=" + version_string)

        if self.debug_only:
            command += ("/DDEBUGONLY",)

        command += (os.path.join("build","packaging","windows","installer.nsi"),)

        self._run_command(command, "Packaging ")

class UnixGccBuilder(BuilderBase):
    def __init__(self, arguments):
        super(UnixGccBuilder, self).__init__(arguments)

        #ada builds (with gnatmake) will look at environment variable that is
        #defined on windows to determine parallellism. Define it on linux too.
        os.environ["NUMBER_OF_PROCESSORS"] = str(self.num_jobs)

        self.install_target = "install"
        self.cmake_generator = "Unix Makefiles"

    @staticmethod
    def can_use():
        return sys.platform.startswith("linux")

    def generator_specific_build_cmds(self):
        return ( "-j", str(self.num_jobs))

#this builder has nothing in common with the other builders, really.
class DebianPackager(object):
    def __init__(self, arguments):
        self.num_jobs = num_jobs()

        #this builder doesnt support exposing test results.
        self.total_tests = -1
        self.failed_tests = -1

    @staticmethod
    def can_use():
        return sys.platform.startswith("linux") and \
            platform.linux_distribution()[0] in ("debian", "Ubuntu")

    def __run(self, cmd, description):
        """Run a command"""

        logger.log(description, "command_description")
        logger.log(" ".join(cmd), "command")

        process = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        output = logger.log_output(process)
        if process.returncode != 0:
            die("Failed to run '" + " ".join(cmd) + "' in " + os.getcwd())

        return output

    def build(self):
        version_tuple, version_string = read_version()
        remove("tmp")
        mkdir("tmp")
        self.__run(("/usr/bin/git", "archive", "HEAD",
                    "--prefix", "safir-sdk-core_" + version_string + "/",
                    "-o", "tmp/safir-sdk-core_" + version_string + ".orig.tar"),
                   "creating tar archive")
        self.__run(("/bin/bzip2", "tmp/safir-sdk-core_" + version_string + ".orig.tar"), "compressing archive")
        os.chdir("tmp")
        self.__run(("/bin/tar", "xvfj", "safir-sdk-core_" + version_string + ".orig.tar.bz2"), "extracting archive")
        os.chdir("safir-sdk-core_" + version_string)
        shutil.copytree(os.path.join("build", "packaging", "debian"), "debian")
        self.__run(("debuild",
                    "--set-envvar", "DEB_BUILD_OPTIONS=parallel="+ str(self.num_jobs),
                    "--prepend-path",
                    "/usr/lib/ccache/",
                    "-us", "-uc"),
                    "building packages")
        os.chdir(glob.glob("obj-*")[0])
        translate_results_to_junit("debhelper")

def getText(nodelist):
    rc = []
    for node in nodelist:
        if node.nodeType == node.TEXT_NODE:
            rc.append(node.data)
    return ''.join(rc)

def translate_results_to_junit(suite_name):
    with open(os.path.join("Testing","TAG"), 'r') as TAGfile:
        dirname = TAGfile.readline().strip()
    with open(suite_name + ".junit.xml","w") as junitfile:
        junitfile.write("<?xml version=\"1.0\"?>\n<testsuite>\n")

        dom = xml.dom.minidom.parse(os.path.join("Testing",dirname,"Test.xml"))

        testing = dom.getElementsByTagName("Testing")[0]
        for child in testing.childNodes:
            if child.nodeType == xml.dom.Node.ELEMENT_NODE:
                if child.tagName == "Test":
                    testName = getText(child.getElementsByTagName("Name")[0].childNodes)
                    testTarget = os.path.split(getText(child.getElementsByTagName("Path")[0].childNodes))[-1]
                    testStatus = child.getAttribute("Status")
                    for meas in child.getElementsByTagName("NamedMeasurement"):
                        if meas.getAttribute("name") == "Exit Code":
                            exitCode = getText(meas.getElementsByTagName("Value")[0].childNodes)
                        if meas.getAttribute("name") == "Exit Value":
                            exitValue = getText(meas.getElementsByTagName("Value")[0].childNodes)
                        if meas.getAttribute("name") == "Execution Time":
                            executionTime = float(getText(meas.getElementsByTagName("Value")[0].childNodes))

                    meas = child.getElementsByTagName("Measurement")[0]

                    junitfile.write("  <testcase name=\"" + testName + "\" classname=\"" +
                                    suite_name + "\" time=\"" + str(executionTime) + "\">\n")
                    output = escape(getText(meas.getElementsByTagName("Value")[0].childNodes))
                    if testStatus == "passed":
                        """success"""
                        junitfile.write("<system-out>" +
                                        output +
                                        "\n</system-out>\n")
                    else:
                        """failure"""

                        junitfile.write("<error message=\"" + exitCode + "(" + exitValue +  ")\">" +
                                        output +
                                        "\n</error>\n")
                    junitfile.write("  </testcase>\n")
        junitfile.write("</testsuite>")


def get_builder(arguments):
    if VisualStudioBuilder.can_use():
        return VisualStudioBuilder(arguments)
    elif arguments.package and DebianPackager.can_use():
        return DebianPackager(arguments)
    elif UnixGccBuilder.can_use():
        return UnixGccBuilder(arguments)
    else:
        die("Failed to work out what builder to use!")

def main():
    arguments = parse_command_line()
    builder = get_builder(arguments)

    builder.build()

    return (builder.total_tests, builder.failed_tests)

#### actual code starts here ####

# create a dummy logger that we use until we have the real thing
logger = DummyLogger()

#reduce process priority (currently only done on unix platforms)
if hasattr(os,"nice"):
    try:
        if os.nice(0) == 0:
            result = os.nice(10)
    except Exception as e:
        logger.log("Failed to set process niceness: " + str(e))

try:
    (tests, failed) = main()
    logger.log("Result", "header")
    logger.log("Build completed successfully!")
    if tests == -1:
        pass
    elif tests == 0:
        logger.log("No tests were performed")
    elif failed == 0:
        logger.log("All tests ran successfully!")
    else:
        logger.log(str(failed) + " tests failed out of " + str(tests) + ".","brief")
    result = 0
except FatalError as e:
    logger.log("Result", "header")
    logger.log("Build script failed:")
    logger.log(str(e), "output")
    logger.log(str(e), "brief")
    result = 1

logger.close()
sys.exit(result)
