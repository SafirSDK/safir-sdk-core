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
        self.__buildlog.write("<html><head><title>Safir SDK Core Build Log</title></head>\n")
        self.__buildlog.write("<body>\n")
        self.__buildlog.write("<h1>Safir SDK Core Build Log</h1>")
        self.__buildlog.write("<b>Command line:</b> " + " ".join(sys.argv) + "<br/>")
        self.__buildlog.write("<b>Start time (local time)</b>: " + time.asctime() + "<br/>")
        self.__buildlog.write("<h2>Starting build</h2>\n")

    def close(self):
        self.__buildlog.write("\n<p/>End time (local time): " + time.asctime())
        self.__buildlog.write("\n</body>\n")
        self.__buildlog.close()

    def __log_stdout(self, data, tag):
        if tag not in Logger.Tags:
            die("unknown logging tag")

        if self.__log_level == "Brief":
            if tag == "header" or tag == "normal" or tag == "brief":
                print(data)
        elif self.__log_level == "Verbose":
            if tag == "brief":
                pass
            elif tag == "header":
                print("\n==== " + data + " ====")
            elif tag == "command_description":
                print("+ " + data + ": ")
            elif tag == "command":
                print("'" + data + "'")
            else:
                print(data)
        sys.stdout.flush()

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

def check_environment():
    pass
    #TODO check cmake?! and other needed stuff


def parse_command_line(builder):
    parser = argparse.ArgumentParser()
    parser.add_argument("--jenkins",
                        action="store_true",
                        default=False,
                        help="Set up and use environment variables for a Jenkins automated build." +
                             "TODO: what else does it do?")

    parser.add_argument("--stage",
                        action="store_true",
                        default=False,
                        help="Install everything to a staging area")

    parser.add_argument("--skip-tests",
                        action = "store_true",
                        help="Skip running the unit tests")

    parser.add_argument("--verbose", "-v",
                        action="count",
                        default=0,
                        help="Print more stuff about what is going on. Use twice to get very verbose output.")

    builder.setup_command_line_arguments(parser)
    arguments = parser.parse_args()

    if arguments.jenkins:
        arguments.verbose += 1
    if arguments.verbose >= 2:
        os.environ["VERBOSE"] = "1"

    global logger
    logger = Logger("Brief" if arguments.verbose == 0 else "Verbose")

    if arguments.jenkins:
        arguments.stage = True
        builder.setenv_jenkins()

    msg = builder.handle_command_line_arguments(arguments)
    if msg is not None:
        print("Failed to parse command line:", msg)
        parser.print_help()
        sys.exit(1)

class BuilderBase(object):
    def __init__(self):
        #We need to limit ourselves a little bit in how
        #many parallel jobs we perform. Each job may use
        #up to 400Mb of memory.
        try:
            self.num_jobs = num_cpus() + 1

            mem_per_job = 400
            memory = physical_memory()
            if memory is not None and memory / self.num_jobs < mem_per_job:
                self.num_jobs = max(1, int(memory / mem_per_job))
        except:
            self.num_jobs = 2

        self.total_tests = 0
        self.failed_tests = 0

        self.install_prefix = None #derived classes can override

    def setup_build_environment(self):
        pass

    def setup_command_line_arguments(self,parser):
        #Child is expected to set up an argument "configs"
        self.setup_command_line_arguments_internal(parser)

    def handle_command_line_arguments(self,arguments):
        self.configs = arguments.configs

        self.debug_only = False
        if arguments.jenkins:
            if os.environ.get("Config") == "DebugOnly":
                logger.log("Using Config 'DebugOnly', building everything in Debug only.")
                self.configs = ("Debug",)
                self.debug_only = True
                
        self.skip_tests = arguments.skip_tests

        self.stage = os.path.join(os.getcwd(),"stage") if arguments.stage else None

        return self.handle_command_line_arguments_internal(arguments)

    def setenv_jenkins_internal(self):
        raise Exception("Not implemented! This is an abstract method")

    def setenv_jenkins(self):
        WORKSPACE = os.environ.get("WORKSPACE")
        if not WORKSPACE:
            die("Environment variable WORKSPACE is not set, is this really a Jenkins build?!")
        #ADA_PROJECT_PATH = (os.environ.get("ADA_PROJECT_PATH") + os.pathsep) if os.environ.get("ADA_PROJECT_PATH") else ""
        #os.environ["ADA_PROJECT_PATH"] = ADA_PROJECT_PATH + os.path.join(os.environ.get("SAFIR_SDK"),"ada")
        #java path gets set by jenkins

        #set database from label environment variable
        #label = os.environ.get("label")
        #if label is not None:
        #    os.environ["DATABASE_NAME"] = label.replace("-","")

        #Call the platform specific setenv
        self.setenv_jenkins_internal()

    def build(self, directory):
        for config in self.configs:
            olddir = os.getcwd()
            mkdir(config)
            os.chdir(config)

            self.__build_internal(directory,
                                  os.pardir if olddir else ".",
                                  config)
            os.chdir(olddir)

        if self.stage:
            logger.log("Building installation package", "brief")
            self.stage_package()

    def __configure(self, directory, srcdir, config):

        command = (cmake(),
                   "-G", self.cmake_generator,
                   "-D", "CMAKE_BUILD_TYPE:string=" + config)

        if self.install_prefix is not None:
            command += ("-D", "CMAKE_INSTALL_PREFIX=" + self.install_prefix)

        command += (srcdir,)
        self._run_command(command,
                           "Configure for " + config + " build", directory)

    def __build_internal(self, directory, srcdir, config):
        logger.log(" - in config " + config, "brief")

        self.__configure(directory, srcdir, config)

        command = (cmake(),
                   "--build", ".",
                   "--") + self.generator_specific_build_cmds()

        
        self._run_command(command,
                           "Build " + config, directory)
        if not self.skip_tests:
            logger.log("   + testing", "brief")
            self.test(directory)
            translate_results_to_junit(config)

        if self.stage:
            logger.log("   + installing to staging area", "brief")
            self.stage_install(directory)

    def stage_install(self, directory):
        for component in ("Runtime", "Development", "Test"):
            command = (cmake(),
                    "-DCOMPONENT="+ component,
                    "-P", "cmake_install.cmake")
            env = os.environ.copy()
            env["DESTDIR"] = os.path.join(self.stage,component)
            self._run_command(command,
                               "Staged install " + component, directory, env = env)
    def stage_package(self):
        logger.log(" ! Packaging not implemented in this builder !","brief")

    def test(self, directory):
        """run ctest in a directory"""
        if not os.path.isfile("DartConfiguration.tcl"):
            dummyfile = open("DartConfiguration.tcl","w")
            dummyfile.close()

        output = self._run_command((ctest(),
                                     "-T", "Test",
                                     "--output-on-failure",
                                     "--no-compress-output"),
                                    "Test", directory, allow_fail = True)
        self.interpret_test_output(output)


    def _run_command(self, cmd, description, what, allow_fail = False, env = None):
        """Run a command"""

        logger.log(description + " " + what, "command_description")
        logger.log(" ".join(cmd), "command")

        process = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env = env)
        output = logger.log_output(process)
        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + " ".join(cmd) + "' for " + what)
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
        #if failed == 0:
        #    logger.log(" - All tests succeeded.","brief")
        #else:
        #    logger.log("!! " + str(failed) + " tests failed out of " + str(tests) + ".","brief")


class VisualStudioBuilder(BuilderBase):
    def __init__(self):
        super(VisualStudioBuilder, self).__init__()

        self.install_target = "Install"

        # Use Jom (google for qt jom) to build if it is available
        try:
            subprocess.Popen(("jom", "/version"), stdout = subprocess.PIPE).communicate()
            self.cmake_generator = "NMake Makefiles JOM"
            self.have_jom = True
        except:
            self.cmake_generator = "NMake Makefiles"
            self.have_jom = False

    @staticmethod
    def can_use():
        return sys.platform == "win32"

    def setup_command_line_arguments_internal(self,parser):
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



    def handle_command_line_arguments_internal(self,arguments):
        self.use_studio = arguments.use_studio

        if not is_64_bit() or arguments.build_32_bit:
            self.target_architecture = "x86"
        else:
            self.target_architecture = "x86-64"


    def generator_specific_build_cmds(self):
        if self.have_jom:
            return ("/nologo", "/j", str(self.num_jobs))
        else:
            return ("/nologo",)

    def filter_configs(self, configs):
        return configs

    def setenv_jenkins_internal(self):
        #os.environ["PATH"] = os.environ.get("PATH") + os.pathsep + os.path.join(os.environ.get("SAFIR_RUNTIME"),"bin")

        #set up K: drive:
        #logger.log("Setting up K: drive using subst.exe","header")
        #bindir = os.path.join(os.environ.get("SAFIR_RUNTIME"),"bin")
        #if not os.path.isdir(bindir):
        #    mkdir(bindir)
        #ret = subprocess.call(("subst","/d", "k:"))
        #logger.log("'subst /d k:' exited with return code " + str(ret),"command")
        #subprocess.call(("subst","k:",bindir))
        #logger.log("'subst k:" + bindir + "' exited with return code " + str(ret),"output")
        pass

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
            die ("Failed to find Visual Studio install dir, checked the following environment variables: " + str(install_dirs))
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

    def setup_build_environment(self):
        """Find vcvarsall.bat and load the relevant environment variables from it.
        This function is inspired (but not copied, for licensing reasons) by the one in python distutils2 msvc9compiler.py"""
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
                    logger.log("Will change '" + name + "' from '" + os.environ.get(name) + "' to '" + value + "'", "detail")
                os.environ[name] = value
                found_variables.add(name)

        if len(required_variables - found_variables) != 0:
            die("Failed to find all expected variables in vcvarsall.bat")

    def stage_package(self):
        command = ("makensis",
                   "/DARCH=" + self.target_architecture,
                   "/DSTUDIO=" + self.used_studio)

        if self.debug_only:
            command += ("/DDEBUGONLY",)

        command += (os.path.join("build","packaging","windows","installer.nsi"),)
        
        self._run_command(command, "Packaging ", "TODO")

class UnixGccBuilder(BuilderBase):
    def __init__(self):
        super(UnixGccBuilder, self).__init__()

        #ada builds (with gnatmake) will look at environment variable that is
        #defined on windows to determine parallellism. Define it on linux too.
        os.environ["NUMBER_OF_PROCESSORS"] = str(self.num_jobs)

        self.install_target = "install"
        self.cmake_generator = "Unix Makefiles"
        self.install_prefix = "/usr"

    @staticmethod
    def can_use():
        return sys.platform.startswith("linux")

    def setup_command_line_arguments_internal(self,parser):
        parser.add_argument("--config",
                            dest="configs",
                            nargs=1,
                            default=("RelWithDebInfo",),
                            choices=known_configs,
                            help="The configuration to build. RelWithDebInfo is the default.")



    def handle_command_line_arguments_internal(self,arguments):
        pass


    def generator_specific_build_cmds(self):
        return ( "-j", str(self.num_jobs))

    def setenv_jenkins_internal(self):
        #LD_LIBRARY_PATH = (os.environ.get("LD_LIBRARY_PATH") + ":") if os.environ.get("LD_LIBRARY_PATH") else ""
        #os.environ["LD_LIBRARY_PATH"] = LD_LIBRARY_PATH + os.path.join(os.environ.get("SAFIR_RUNTIME"),"lib")
        pass

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
                                    suite_name + "\" time=\"" + str(executionTime) + "\"")
                    if testStatus == "passed":
                        """success"""
                        junitfile.write("/>\n")
                    else:
                        """failure"""
                        output = escape(getText(meas.getElementsByTagName("Value")[0].childNodes))
                        junitfile.write(">\n    <error message=\"" + exitCode + "(" + exitValue +  ")\">" +
                                        output +
                                        "\n</error>\n  </testcase>\n")
        junitfile.write("</testsuite>")


def get_builder():
    if VisualStudioBuilder.can_use():
        return VisualStudioBuilder()
    elif UnixGccBuilder.can_use():
        return UnixGccBuilder()
    else:
        die("Failed to work out what builder to use!")

def main():
    builder = get_builder()
    parse_command_line(builder)
    check_environment()
    builder.setup_build_environment()

    builder.build(".")

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
    if tests == 0:
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
