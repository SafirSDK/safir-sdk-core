#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2009 (http://www.safirsdk.com)
#
# Created by: Lars Hagstrom / stlrha
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
import os, glob, sys, subprocess, platform, xml.dom.minidom, re, time, shutil

from xml.sax.saxutils import escape


#Load some environment variables that are needed throughout as globals
SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
SAFIR_SDK = os.environ.get("SAFIR_SDK")

#a few constants
known_configs = set(["Release", "Debug", "MinSizeRel", "RelWithDebInfo"])

#define some global variables
skip_list = None
clean = False
force_config = None
force_extra_config = None
if sys.platform == "win32": #target_architecture is only set on windows platfoms!
    target_architecture = None
ada_support = False
java_support = False
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

def copy_dob_files(source_dir, target_dir):
    """Copy dou and dom files from the source directory to the given subdirectory in the dots_generated directory"""
    dots_generated_dir = os.path.join(SAFIR_SDK, "dots", "dots_generated")
    abs_target_dir = os.path.join(dots_generated_dir, target_dir)

    logger.log("Copying dob files from " + source_dir + " to " + abs_target_dir,"output")
    
    if not os.path.isdir(dots_generated_dir):
        mkdir(dots_generated_dir)

    pattern = re.compile("[a-zA-Z0-9\.\-]*\.do[um]$")
    pattern2 = re.compile("[a-zA-Z0-9\.]*-java\.namespace\.txt$")
    for filename in os.listdir(source_dir):
        if pattern.match(filename) or pattern2.match(filename):
            
            if not os.path.isdir(abs_target_dir):
                mkdir(abs_target_dir)
                
            shutil.copy2(os.path.join(source_dir, filename), abs_target_dir)

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
    if not sys.platform.startswith("linux"):
        die ("physical_memory() is only implemented on linux")
    with open("/proc/meminfo") as a_file:
        meminfo = a_file.read()
    match = re.search(r"MemTotal:\s*([0-9]*) kB", meminfo)
    return int(match.group(1))/1024

class DummyLogger(object):
    def log(self, data, tag = None):
        sys.stdout.write(data + "\n")
        sys.stdout.flush()
        
    def logOutput(self, process):
        raise Exception("DummyLogger doesnt support process output logging. " + 
                        "You should investigate why the real logger is not instantiated by now...")

class Logger(object):
    LogLevel = ("Brief", "Verbose")
    Tags = set(["header","brief","normal","detail","command","output"])

    def __init__(self,level):
        #make stdout unbuffered
        if level not in Logger.LogLevel:
            die("Bad log level")
        self.__logLevel = level

    def log(self, data, tag = "normal"):
        if data is None: return
        
        if tag not in Logger.Tags:
            die("unknown logging tag")

        if self.__logLevel == "Brief":
            if tag == "header" or tag == "normal" or tag == "brief":
                sys.stdout.write(data + "\n")
        elif self.__logLevel == "Verbose":
            if tag == "brief":
                pass
            elif tag == "header":
                sys.stdout.write("\n==== " + data + " ====\n")
            elif tag == "command":
                sys.stdout.write("+ " + data + "\n")
            else:
                sys.stdout.write(data + "\n")
        sys.stdout.flush()

    def logOutput(self, process):
        output = list()
        while True:
            line = process.stdout.readline()
            if not line:
                break
            line = line.rstrip("\n\r")
            self.log(line,"output")
            output += (line,)
        process.wait()
        if process.returncode != 0:
            self.log("Failure, return code is " + str(process.returncode))
        self.log("","output")
        return "\n".join(output)

def check_environment():
    global SAFIR_RUNTIME
    global SAFIR_SDK

    if SAFIR_RUNTIME == None or SAFIR_SDK == None:
        die("You need to have both SAFIR_RUNTIME and SAFIR_SDK set")

    #Make sure slashes are the right direction, etc.
    SAFIR_RUNTIME = os.path.normpath(SAFIR_RUNTIME)
    SAFIR_SDK = os.path.normpath(SAFIR_SDK)

    #TODO check cmake?! and other needed stuff


def parse_command_line(builder):
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("--command-file", "-f",action="store",type="string",dest="command_file",
                      help="The command to execute")
    parser.add_option("--no-ada-support", action="store_false",dest="ada_support",default=True,
                      help="Disable Ada support")
    parser.add_option("--no-java-support", action="store_false",dest="java_support",default=True,
                      help="Disable Java support")
    parser.add_option("--skip-list",action="store",type="string",dest="skip_list",
                      help="A space-separated list of regular expressions of lines in the command file to skip")
    parser.add_option("--clean", action="store_true",dest="clean",default=False,
                      help="Run 'clean' before building each subsystem.")
    parser.add_option("--force-config", action="store",type="string",dest="force_config",
                      help="Build for the given config irrespective of what the command file says about config")
    parser.add_option("--force-extra-config", action="store",type="string",dest="force_extra_config",
                      help="Build for the given extra config irrespective of what the command " + 
                      "file says about extra_config")    
    parser.add_option("--jenkins", action="store_true",dest="jenkins",default=False,
                      help="Set up and use environment variables for a Jenkins automated build." + 
                      "Currently sets up SAFIR_RUNTIME, SAFIR_SDK, PATH, LD_LIBRARY_PATH and ADA_PROJECT_PATH " +
                      "when needed, and honours the 'Config' matrix axis. Also implies --verbose.")
    parser.add_option("--verbose", "-v", action="count",dest="verbose",default=0,
                      help="Print more stuff about what is going on. Use twice to get very verbose output.")

    if is_64_bit() and sys.platform == "win32":
        parser.add_option("--32-bit",action="store_true",dest="build_32_bit",default=False,
                          help="Build a 32 bit system even though this machine is 64 bit.")

    builder.setup_command_line_options(parser)
    (options,args) = parser.parse_args()

    if options.jenkins:
        options.verbose += 1
    if options.verbose >= 2:
        os.environ["VERBOSE"] = "1"

    global logger
    logger = Logger("Brief" if options.verbose == 0 else "Verbose")

    if sys.platform == "win32":
        if not is_64_bit():
            options.build_32_bit = True

        if not options.build_32_bit:
            logger.log("Will not build Ada interfaces, since Ada is not currently supported for 64bit platforms")
            options.ada_support = False

        global target_architecture
        target_architecture = "x86" if options.build_32_bit else "x86-64"

    global ada_support
    ada_support = options.ada_support
    global java_support
    java_support = options.java_support
    
    global skip_list
    if (options.skip_list == None):
        skip_list = list()
    else:
        skip_list = options.skip_list.split()
        
    global clean
    clean = options.clean

    if options.command_file is None:
        die("You need to specify the command file to use")
        
    if not os.path.isfile(options.command_file):
        die("The specified command file could not be found")
    global command_file
    command_file = open(options.command_file,'r')

    global force_config
    if options.force_config is not None:
        force_config = options.force_config

    global force_extra_config
    if options.force_extra_config is not None:
        force_extra_config = options.force_extra_config        

    if options.jenkins:
        builder.setenv()
        global SAFIR_RUNTIME
        global SAFIR_SDK
        #reload env
        SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
        SAFIR_SDK = os.environ.get("SAFIR_SDK")

        config = os.environ.get("Config")
        if config is not None:
            if force_config is not None or force_extra_config is not None:
                die("Cannot combine force_config or force_extra_config with $Config!")
            if config == "Release":
                logger.log("Using Config 'Release', building as specified in the command file.")
            elif config == "DebugOnly":
                logger.log("Using Config 'DebugOnly', ignoring command file configs, and building " + 
                           "everything in Debug only.")
                force_config = "Debug"
                force_extra_config = "None"
            else:
                die("Unexpected 'Config' value: " + config + ", can handle 'Release' and 'DebugOnly'")
    builder.handle_command_line_options(options)


def find_sln():
    sln_files = glob.glob("*.sln")
    if (len(sln_files) != 1):
        die("There is not exactly one sln file in " + os.getcwd() +
            ", either cmake failed to generate one or there is another one coming from somewhere else!")
    return sln_files[0]

class BuilderBase(object):
    def __init__(self):
        self.total_tests = 0
        self.failed_tests = 0

    def interpret_test_output(self,output):
        match = re.search(r"tests passed, ([0-9]+) tests failed out of ([0-9]+)",output)
        if len(match.groups()) != 2:
            logger.log("Failed to parse test output!")
            return
        failed = int(match.group(1))
        tests = int(match.group(2))
        self.total_tests += tests
        self.failed_tests += failed
        if failed == 0:
            logger.log(" - All tests succeeded.","brief")
        else:
            logger.log("!! " + str(failed) + " tests failed out of " + str(tests) + ".","brief")
            
class VisualStudioBuilder(BuilderBase):
    def __init__(self):    
        self.tmpdir = os.environ.get("TEMP")
        if self.tmpdir is None:
            self.tmpdir = os.environ.get("TMP")
            if self.tmpdir is None:
                die("Failed to find a temp directory!")
        if not os.path.isdir(self.tmpdir):
            die("I can't seem to use the temp directory " + self.tmpdir)  

    def set_studio_version(self, studio):
        super(VisualStudioBuilder, self).__init__()
        VS80 = os.environ.get("VS80COMNTOOLS")
        VS90 = os.environ.get("VS90COMNTOOLS")
        VS100 = os.environ.get("VS100COMNTOOLS")

        VSCount = 0

        if VS80 is not None:
            VSCount = VSCount + 1
        if VS90 is not None:
            VSCount = VSCount + 1
        if VS100 is not None:
            VSCount = VSCount + 1

        self.studio = None
        self.generator = None
        self.studio_install_dir = None
        self.vcvarsall_arg = None
        
        if studio is not None:
            #check that studio given on command line is installed
            if studio == "2005":
                if VS80 is None:
                    die("Visual Studio 2005 seems not to be installed!")
                else:
                    self.studio = VS80
            elif studio == "2008":
                if VS90 is None:
                    die("Visual Studio 2008 seems not to be installed!")
                else:
                    self.studio = VS90                    
            elif studio == "2010":
                if VS100 is None:
                    die("Visual Studio 2010 seems not to be installed!")
                else:
                    self.studio = VS100                    
            else:
                die("Studio verson is not supported!")
        else:
            #no studio given on command line, check that there is only one version installed
            if VSCount > 1:
                die("I found several Visual Studio installations, will need command line arg!")
            elif VS80 is not None:
                self.studio = VS80
            elif VS90 is not None:
                self.studio = VS90
            elif VS100 is not None:
                self.studio = VS100
            else:
                die("No Visual Studio version seems to be installed!")


        if self.studio == VS80:
            if target_architecture == "x86":
                self.generator = "Visual Studio 8 2005"
            elif target_architecture == "x86-64":
                self.generator = "Visual Studio 8 2005 Win64"
            else:
                die("Target architecture " + target_architecture + " is not supported for Visual Studio 8 2005 !")
        elif self.studio == VS90:
            if target_architecture == "x86":
                self.generator = "Visual Studio 9 2008"
            elif target_architecture == "x86-64":
                self.generator = "Visual Studio 9 2008 Win64"
            else:
                die("Target architecture " + target_architecture + " is not supported for Visual Studio 9 2008 !")
        elif self.studio == VS100:
            if target_architecture == "x86":
                self.generator = "Visual Studio 10"
            elif target_architecture == "x86-64":
                self.generator = "Visual Studio 10 Win64"
            else:
                die("Target architecture " + target_architecture + " is not supported for Visual Studio 10 !")
        else:
            die("Could not find a supported compiler to use!")
            
    @staticmethod
    def can_use():
        VS80 = os.environ.get("VS80COMNTOOLS")
        VS90 = os.environ.get("VS90COMNTOOLS")
        VS100 = os.environ.get("VS100COMNTOOLS")
        return VS80 is not None or VS90 is not None or VS100 is not None
            
    def setup_command_line_options(self,parser):
        parser.add_option("--use-studio",action="store",type="string",dest="use_studio",
                          help="The visual studio to use for building, can be '2005', '2008' or '2010'")

    def handle_command_line_options(self,options):

        self.set_studio_version(options.use_studio)

        self.studio_install_dir = os.path.join(self.studio,"..", "..")

        #work out what compiler tools to use
        if target_architecture == "x86":
            self.vcvarsall_arg = "x86"
        elif target_architecture == "x86-64":
            self.vcvarsall_arg = "amd64"
        else:
            die("Unknown target architecture " + target_architecture)    
            
    def setenv(self):
        WORKSPACE = os.environ.get("WORKSPACE")
        if not WORKSPACE:
            die("Environment variable WORKSPACE is not set, is this really a Jenkins build?!")
        os.environ["SAFIR_RUNTIME"] = os.path.join(WORKSPACE,"safir","runtime")
        os.environ["SAFIR_SDK"] = os.path.join(WORKSPACE,"safir","sdk")
        os.environ["PATH"] = os.environ.get("PATH") + ";" + os.path.join(os.environ.get("SAFIR_RUNTIME"),"bin")
        ADA_PROJECT_PATH = (os.environ.get("ADA_PROJECT_PATH") + ";") if os.environ.get("ADA_PROJECT_PATH") else ""
        os.environ["ADA_PROJECT_PATH"] = ADA_PROJECT_PATH + os.path.join(os.environ.get("SAFIR_SDK"),"ada")
        #java path gets set by jenkins

        #set up K: drive:
        logger.log("Setting up K: drive using subst.exe","header")
        bindir = os.path.join(os.environ.get("SAFIR_RUNTIME"),"bin")
        if not os.path.isdir(bindir):
            mkdir(bindir)
        ret = subprocess.call(("subst","/d", "k:"))
        logger.log("'subst /d k:' exited with return code " + str(ret),"command")
        subprocess.call(("subst","k:",bindir))
        logger.log("'subst k:" + bindir + "' exited with return code " + str(ret),"output")
        
        #set database from label environment variable
        label = os.environ.get("label")
        if label is not None:
            os.environ["DATABASE_NAME"] = label.replace("-","")


    def build(self, directory, configs, install):
        if self.__can_use_studio_build(directory):
            self.__studio_build(directory,configs,install)
        else:
            self.__nmake_build(directory,configs,install)

    def test(self, directory):
        """run ctest in a directory"""
        if not os.path.isfile("DartConfiguration.tcl"):
            dummyfile = open("DartConfiguration.tcl","w")
            dummyfile.close()

        output = self.__run_command((ctest() + " -T Test --output-on-failure"),
                                    "Test", directory, allow_fail = True)
        self.interpret_test_output(output)
            

    def dobmake(self):
        """run the dobmake command"""
        ada = ""
        if not ada_support:
            ada = " --no-ada "
        java = ""
        if  not java_support:
            java = " --no-java "
            
        batpath = os.path.join(self.tmpdir,"build2.bat")
        
        with open(batpath,"w") as bat:
            bat.write("@echo off\n" +
                      "call \"" + os.path.join(self.studio_install_dir,"VC","vcvarsall.bat") + 
                      "\" "  + self.vcvarsall_arg + "\n" +
                      "\"" + os.path.join(SAFIR_RUNTIME,"bin","dobmake.py") + "\" -b --html-output --rebuild" + 
                      ada + java + " --target " + target_architecture) #batch mode (no gui)
            if force_config == "Debug" and force_extra_config == "None":
                bat.write (" --no-cpp-release --default-config Debug")
            bat.write("\n")
        process = subprocess.Popen(batpath,stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)
        
        logger.logOutput(process)

        if process.returncode != 0:
            die("Failed to run dobmake")

    def __can_use_studio_build(self,directory):
        with open("CMakeLists.txt","r") as cmakelists:
            contents = cmakelists.read().lower()

        bad_keywords = ("add_subdirectory",
                        "add_custom_target",
                        "add_custom_command",
                        "add_cs_library",
                        "dotnet",
                        "java",
                        "gprmake",
                        "enable_testing")
        logger.log("Checking if I can use Visual Studio to speed the build up.")
        for bad in bad_keywords:
            if contents.find(bad) != -1:
                logger.log(" - No, CMakeLists.txt contains " + bad)
                return False

        good_keywords = ("add_executable",
                         "add_library")

        for good in good_keywords:
            if contents.find(good) != -1:
                logger.log(" - Yes, CMakeLists.txt contains " + good)
                return True
        logger.log(" - No, failed to find either good or bad keywords!")
        return False

    def __studio_build(self,directory,configs,install):
        if clean:
            remove(self.generator)
            
        mkdir(self.generator)
        olddir = os.getcwd()
        os.chdir(self.generator)
    
        self.__run_command((cmake() +
                            " -G \"" + self.generator + "\" " +
                            ".."),
                           "Configure", directory)

        solution = find_sln()
        
        for config in configs:
            logger.log(" - in config " + config, "brief")
            self.__run_command(cmake() + " --build . --config " + config + ("--clean-first" if clean else ""),
                               "Build " + config, directory)

            if install:
                self.__run_command(cmake() + " --build . --config " + config + " --target Install",
                                   "Install " + config, directory)

        os.chdir(olddir)

    def __nmake_build(self,directory,configs,install):
        """build a directory using nmake"""
        for config in configs:
            logger.log(" - in config " + config, "brief")
            self.__run_command(cmake() + " -D CMAKE_BUILD_TYPE:string=" + config + " " +
                               "-D SAFIR_ADA_SUPPORT:boolean=" + str(ada_support) + " " +
                               "-D SAFIR_JAVA_SUPPORT:boolean=" + str(java_support) + " " +
                               "-G \"NMake Makefiles\" .",
                               "Configure " + config, directory)
            if clean:
                self.__run_command("nmake /NOLOGO clean",
                                   "Clean " + config, directory, allow_fail=True)
            self.__run_command("nmake /NOLOGO",
                               "Build " + config, directory)
            if install:
                self.__run_command("nmake /NOLOGO install",
                                   "Install " + config, directory)

    def __run_command(self, cmd, description, what, allow_fail = False):
        """Run a command"""
        batpath = os.path.join(self.tmpdir,"build.bat")
        bat = open(batpath,"w")
        bat.write("@echo off\n" +
                  "call \"" + os.path.join(self.studio_install_dir,"VC","vcvarsall.bat") + 
                  "\" " + self.vcvarsall_arg + "\n" +
                  cmd)
        bat.close()

        logger.log(description + " " + what + ": '" + cmd + "'", "command")
        process = subprocess.Popen(batpath,stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)
        output = logger.logOutput(process)

        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + cmd + "' for " + what)
            else:
                logger.log("This command failed, but failure of this particular command is " + 
                           "non-fatal to the build process, so I'm continuing\n")
        return output

class UnixGccBuilder(BuilderBase):
    def __init__(self):
        super(UnixGccBuilder, self).__init__()
        #We need to limit ourselves a little bit in how
        #many parallel jobs we perform. Each job may use
        #up to 400Mb of memory.
        try:
            memory = physical_memory()
            self.num_jobs = num_cpus() + 1
        
            if memory / self.num_jobs < 400:
                self.num_jobs = max(1,memory / 400)
        except:
            self.num_jobs = 2
        #ada builds (with gnatmake) will look at environment variable that is
        #defined on windows to determine parallellism. Define it on linux too.
        os.environ["NUMBER_OF_PROCESSORS"] = str(self.num_jobs)

    @staticmethod
    def can_use():
        return sys.platform.startswith("linux")

    def setup_command_line_options(self,parser):
        pass

    def handle_command_line_options(self,options):
        pass

    def setenv(self):
        WORKSPACE = os.environ.get("WORKSPACE")
        if not WORKSPACE:
            die("Environment variable WORKSPACE is not set, is this really a Jenkins build?!")
        os.environ["SAFIR_RUNTIME"] = os.path.join(WORKSPACE,"safir","runtime")
        os.environ["SAFIR_SDK"] = os.path.join(WORKSPACE,"safir","sdk")
        LD_LIBRARY_PATH = (os.environ.get("LD_LIBRARY_PATH") + ":") if os.environ.get("LD_LIBRARY_PATH") else ""
        os.environ["LD_LIBRARY_PATH"] = LD_LIBRARY_PATH + os.path.join(os.environ.get("SAFIR_RUNTIME"),"lib")
        ADA_PROJECT_PATH = (os.environ.get("ADA_PROJECT_PATH") + ":") if os.environ.get("ADA_PROJECT_PATH") else ""
        os.environ["ADA_PROJECT_PATH"] = ADA_PROJECT_PATH + os.path.join(os.environ.get("SAFIR_SDK"),"ada")
        #java path gets set by jenkins

        #set database from label environment variable
        label = os.environ.get("label")
        if label is not None:
            os.environ["DATABASE_NAME"] = label.replace("-","")

    def build(self, directory, configs, install):
        """build a directory using make"""
        config = configs[0]
        logger.log(" - in config " + config, "brief")
        self.__run_command((cmake(),
                            "-D", "CMAKE_BUILD_TYPE:string=" + config,
                            "-D", "SAFIR_ADA_SUPPORT:boolean=" + str(ada_support),
                            "-D", "SAFIR_JAVA_SUPPORT:boolean=" + str(java_support),
                            "."),
                           "Configure for " + config + " build", directory)
        if clean:
            self.__run_command(("make", "clean"),
                               "Clean " + config, directory, allow_fail=True)
        self.__run_command(("make","-j", str(self.num_jobs)),
                           "Build " + config, directory)
        if install:
            self.__run_command(("make", "install","-j", str(self.num_jobs)),
                               "Install " + config, directory)

    def test(self, directory):
        """run ctest in a directory"""
        if not os.path.isfile("DartConfiguration.tcl"):
            dummyfile = open("DartConfiguration.tcl","w")
            dummyfile.close()

        output = self.__run_command((ctest(),
                                     "-T", "Test", "--output-on-failure"),
                                    "Test", directory, allow_fail = True)
        self.interpret_test_output(output)

    def dobmake(self):
        """run the dobmake command"""
        cmd = (os.path.join(SAFIR_RUNTIME,"bin","dobmake.py"), "-b", "--rebuild") #batch mode (no gui)
        if force_config == "Debug" and force_extra_config == "None":
            cmd += ("--no-cpp-release","--default-config","Debug")
        if not ada_support:
            cmd += ("--no-ada",)
        if not java_support:
            cmd += ("--no-java",)
        
        process = subprocess.Popen(cmd,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT,
                                   universal_newlines=True)
        logger.logOutput(process)
        if process.returncode != 0:
            die("Failed to run dobmake")

    def __run_command(self, cmd, description, what, allow_fail = False):
        """Run a command"""
        logger.log(description + " " + what + ": '" + " ".join(cmd) + "'", "command")
        process = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)
        output = logger.logOutput(process)
        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + " ".join(cmd) + "' for " + what)
            else:
                logger.log("This command failed, but failure of this particular command " + 
                           "is non-fatal to the build process, so I'm continuing")

        return output

def in_skip_list(line):
    "Check the argument against all regexps in the skip-list"
    import re
    for expr in skip_list:
        p=re.compile(expr)
        if p.search(line):
            return True
    return False


def build_dir(directory, configs, builder, install = True):
    if not os.path.isdir(directory):
        die("Failed to enter " + directory + ", since it does not exist (or is not a directory)")
    olddir = os.getcwd()
    os.chdir(directory)
    try:
        if not os.path.isfile("CMakeLists.txt"):
            die ("Couldn't find a CMakeLists.txt in " + directory + ".")
        builder.build(directory, configs, install)
    finally:
        os.chdir(olddir)

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


def run_test_suite(directory, suite_name, builder):
    olddir = os.getcwd()
    os.chdir(directory)
    try:
        builder.test(directory)
        translate_results_to_junit(suite_name)
    finally:
        os.chdir(olddir)

def dobmake(builder):
    logger.log("Running dobmake","header")
    builder.dobmake()

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

    olddir = os.getcwd()

    split_line = None

    for line in command_file:
        if line[0] == '#' or line.strip() == "":
            continue

        last_line = split_line
        split_line = line.split()
        
        if in_skip_list(line.strip()):
            logger.log("- Skipping " + str(split_line) + ", matches skip-list","header")
            continue
                
        command = split_line[0]

        start_time = time.time()

        if command == "build_dir":
            if len(split_line) > 1:
                directory = split_line[1]

            configs = list()
            if len(split_line) > 2:
                if (force_config != None):
                    configs.append(force_config)
                else:
                    configs.append(split_line[2])
            
            if len(split_line) > 3:
                if force_extra_config is None:
                    configs.append(split_line[3])
                elif force_extra_config != "None":
                    configs.append(force_extra_config)
                
            if not set(configs) <= known_configs:
                die ("Unknown build kind '" + str(configs) + "' for " + directory)
            if len(configs) < 1:
                die ("Need at least one config for " + directory)
            logger.log("Building " + directory, "header")
            build_dir(directory, configs, builder)
        elif command == "copy_dob_files":
            if len(split_line) < 3:
                die ("Need both a source and target directory")
            if len(split_line) > 3:
                die ("To many parameters for " + command)
            if last_line is not None and last_line[0] != "copy_dob_files":
                logger.log("Copying dob files","header")
            copy_dob_files(os.path.normpath(split_line[1]), os.path.normpath(split_line[2]))  
        elif command == "run_test_suite":
            if len(split_line) < 3:
                die ("Need both a source directory and a test suite name")
            if len(split_line) > 3:
                die ("To many parameters for " + command)
            directory = split_line[1]
            suite_name = split_line[2]
            logger.log("Building and running test suite " + suite_name + " in " + directory, "header")
            cf = "Release" if force_config is None else force_config
            build_dir(directory, (cf,), builder, False) # build, but do not install
            run_test_suite(directory, suite_name, builder)
        elif command == "dobmake":
            dobmake(builder)
        else:
            die("Got unknown command '" + command + "'")

        logger.log("Build step took " + str(int(time.time() - start_time)) + " seconds", "detail")

    os.chdir(olddir)

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
    if failed == 0:
        logger.log("All tests ran successfully!")
    else:
        logger.log(str(failed) + " tests failed out of " + str(tests) + ".","brief")
except FatalError as e:
    logger.log("Result", "header")
    logger.log("Build script failed: " + str(e))
    sys.exit(1)

sys.exit(0)
