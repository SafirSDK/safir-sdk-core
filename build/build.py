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

import os, glob, sys, subprocess, threading, xml.dom.minidom, re

#Load some environment variables that are needed throughout as globals
SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
SAFIR_SDK = os.environ.get("SAFIR_SDK")

#a few constants
known_configs = set(["Release", "Debug", "MinSizeRel", "RelWithDebInfo"])

#define some global variables
skip_list= None
clean = False
force_config = None
force_extra_config = None

class FatalError(Exception):pass

def die(message):
    raise FatalError(message)

def copy_dob_files(source_dir, target_dir):
    """Copy dou and dom files from the source directory to the given subdirectory in the dots_generated directory"""
    import os
    import re
    import shutil
    dots_generated_dir = os.path.join(SAFIR_SDK,"dots","dots_generated")
    abs_target_dir = os.path.join(dots_generated_dir, target_dir)

    logger.log("Copying dob files from " + source_dir + " to " + abs_target_dir)
    
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
        except Exception, e:
            die ("Failed to remove file " + path + ". Got exception " + e)
            
    for name in os.listdir(path):
        if os.path.isdir(os.path.join(path,name)):
            remove(os.path.join(path,name))
        else:
            try:
                os.remove(os.path.join(path,name))
            except Exception, e:
                die ("Failed to remove file " + os.path.join(path,name) + ". Got exception " + e)

    try:
        os.rmdir(path)
    except Exception, e:
        die ("Failed to remove directory " + path + ". Got exception " + e)


def num_cpus():
    """Detects the number of CPUs on a system. Cribbed from pp."""
    # Linux, Unix and MacOS:
    if hasattr(os, "sysconf"):
        if os.sysconf_names.has_key("SC_NPROCESSORS_ONLN"):
            # Linux & Unix:
            ncpus = os.sysconf("SC_NPROCESSORS_ONLN")
            if isinstance(ncpus, int) and ncpus > 0:
                return ncpus
        else: # OSX:
            return int(os.popen2("sysctl -n hw.ncpu")[1].read())
    # Windows:
    if os.environ.has_key("NUMBER_OF_PROCESSORS"):
        ncpus = int(os.environ["NUMBER_OF_PROCESSORS"]);
        if ncpus > 0:
            return ncpus
    return 1 # Default

def physical_memory():
    if not sys.platform.startswith("linux"):
        die ("physical_memory() is only implemented on linux")
    with open("/proc/meminfo") as file:
        meminfo = file.read()
    match = re.search(r"MemTotal:\s*([0-9]*) kB", meminfo)
    return int(match.group(1))/1024

class Logger(object):
    LogLevel = set(["None", "Brief", "Verbose", "Full"])
    LogType = set(["Plain", "HTML"])

    def __init__(self):
        #make stdout unbuffered
        sys.stdout = os.fdopen(sys.stdout.fileno(), 'w', 0)

        self.logdata = list()
        self.lock = threading.Lock()
        self.lastTag = None
        self.stdoutLog = ("Brief", "Plain")
        self.fileLog = ("Full", "HTML")
        self.file = None

    def close(self):
        pass

    def set_stdoutLog(self, log):
        if log[0] not in Logger.LogLevel:
            die("Bad log level")
        if log[1] not in Logger.LogType:
            die("Bad log type")
        self.stdoutLog = log

    def set_fileLog(self, log):
        if log[0] not in Logger.LogLevel:
            die("Bad log level")
        if log[1] not in Logger.LogType:
            die("Bad log type")
        self.fileLog = log

    def __emitStartTag(self, stream, tag):
        if tag == "header":
            stream.write("<h3>\n")
        if tag == "title":
            stream.write("<title>\n")
        if tag == "command":
            stream.write("<pre style=\"color: green\">\n")
        if tag == "pre":
            stream.write("<pre>\n")
        if tag == "error":
            stream.write("<font color=\"red\">\n")
        

    def __emitEndTag(self, stream, tag):
        if tag == "header":
            stream.write("</h3>\n")
        if tag == "title":
            stream.write("</title>\n")
        if tag == "command":
            stream.write("</pre>\n")
        if tag == "pre":
            stream.write("</pre>\n")
        if tag == "error":
            stream.write("</font>\n")

    def __openLogFile(self):
        try:
            os.remove("buildlog.html")
        except OSError:
            pass
        try:
            os.remove("buildlog.txt")
        except OSError:
            pass

        if self.fileLog[1] == "HTML":
            logpath = "buildlog.html"
        else:
            logpath = "buildlog.txt"

        self.logfile = open(logpath,'w', 0)
#        buildlog.write("<html><title>Build Log</title><body>")

    def log(self, data, tag = "normal"):
        if data is None: return
        
        if tag not in ("header","normal","command","output"):
            die("unknown logging tag")
        
        if tag == "header":
            sys.stdout.write("\n==== " + data + " ====\n")
        elif tag == "command":
            sys.stdout.write("+ " + data + "\n")
        else:
            sys.stdout.write(data + "\n")

 
    def __write(self, data, tag = "normal", level = 0):
        if data is None: return

        if self.file is None:
            self.__openLogFile()

        if tag != self.lastTag:
            self.__emitEndTag(self.lastTag)                
            self.__emitStartTag(sys.stdout, tag)
            self.lastTag = tag
        sys.stdout.write(data)

    def writeHeader(self, data):
        self.__write(data,"header")

    def writeTitle(self, data):
        self.__write(data,"title")

    def writeCommand(self, data):
        self.__write(data,"command")

    def writeError(self,data):
        self.__write(data,"error")

    class __PollThread(threading.Thread):
        def __init__(self, process, logger):
            threading.Thread.__init__(self)
            self.process = process
            self.logger = logger

        def run(self):
            while True:
                data = self.process.stdout.readline().rstrip()
                if self.process.poll() is not None:
                    return
                if len(data) == 0:
                    continue
                self.logger.log(data, "output")


    def logOutput(self, process):
        thread = Logger.__PollThread(process,self)
        thread.start()
        thread.join()
        process.wait()
        if process.returncode != 0:
            self.log("Failure, return code is " + str(process.returncode))
        self.log("")

    def close(self):
        pass

def check_environment():
    global SAFIR_RUNTIME
    global SAFIR_SDK

    print "Platform =", sys.platform
    
    if SAFIR_RUNTIME == None or SAFIR_SDK == None:
        die("You need to have both SAFIR_RUNTIME and SAFIR_SDK set");

    #Make sure slashes are the right direction, etc.
    SAFIR_RUNTIME = os.path.normpath(SAFIR_RUNTIME)
    SAFIR_SDK = os.path.normpath(SAFIR_SDK)

    print "Using SAFIR_RUNTIME =", SAFIR_RUNTIME
    print "Using SAFIR_SDK =", SAFIR_SDK
    #TODO check cmake?! and other needed stuff




def parse_command_line(builder):
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("--command-file", "-f",action="store",type="string",dest="command_file",
                      help="The command to execute")
    parser.add_option("--skip-list",action="store",type="string",dest="skip_list",
                      help="A space-separated list of regular expressions of lines in the command file to skip")
    parser.add_option("--clean", action="store_true",dest="clean",default=False,
                      help="Run 'clean' before building each subsystem.")
    parser.add_option("--force-config", action="store",type="string",dest="force_config",
                      help="Build for the given config irrespective of what the command file says about config")
    parser.add_option("--force-extra-config", action="store",type="string",dest="force_extra_config",
                      help="Build for the given extra config irrespective of what the command file says about extra_config")    
    parser.add_option("--jenkins", action="store_true",dest="jenkins",default=False,
                      help="Set up and use environment variables for a Jenkins automated build.\n" + 
                      "Currently sets up SAFIR_RUNTIME, SAFIR_SDK, PATH, LD_LIBRARY_PATH and ADA_PROJECT_PATH \n" +
                      "when needed, and honours the 'Config' matrix axis")
    parser.add_option("--verbose", "-v", action="store_true",dest="verbose",default=False,
                      help="Print more stuff about what is going on")
    parser.add_option("--full", action="store_true",dest="full_log",default=False,
                      help="Print everything to stdout, and no log to file.")


    builder.setup_command_line_options(parser)
    (options,args) = parser.parse_args()

    global skip_list;
    if (options.skip_list == None):
        skip_list = list();
    else:
        skip_list = options.skip_list.split();
        
    global clean
    clean = options.clean

#    if options.verbose:
#        logger.set_stdoutLog(("Verbose","Plain"))

#    if options.full_log:
#        logger.set_stdoutLog(("Full","Plain"))
#        logger.set_fileLog(("None","HTML"))


    if options.command_file is None:
        die("You need to specify the command file to use")
        
    if not os.path.isfile(options.command_file):
        die("The specified command file could not be found")
    global command_file
    command_file = open(options.command_file,'r')

    global force_config;
    if options.force_config is not None:
        force_config = options.force_config

    global force_extra_config;
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
                print "Using Config 'Release', building as specified in the command file."
            elif config == "DebugOnly":
                print "Using Config 'DebugOnly', ignoring command file configs, and building everything in Debug only."
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
    
            
class VisualStudioBuilder(object):
    def __init__(self):
        VS80 = os.environ.get("VS80COMNTOOLS")
        VS90 = os.environ.get("VS90COMNTOOLS")
        VS100 = os.environ.get("VS100COMNTOOLS")

        VSCount = 0;

        if VS80 is not None:
            VSCount = VSCount + 1;
        if VS90 is not None:
            VSCount = VSCount + 1;
        if VS100 is not None:
            VSCount = VSCount + 1;

        self.studio = None
        self.generator = None
        
        if VSCount > 1:
            die("I found both several Visual Studio installations, will need command line arg!")
        elif VS80 is not None:
            self.studio = VS80
            self.generator = "Visual Studio 8 2005"
        elif VS90 is not None:
            self.studio = VS90
            self.generator = "Visual Studio 9 2008"
        elif VS100 is not None:
            self.studio = VS100
            self.generator = "Visual Studio 10"
        else:
            die("Could not find a supported compiler to use!")

        self.tmpdir = os.environ.get("TEMP")
        if self.tmpdir is None:
            self.tmpdir = os.environ.get("TMP")
            if self.tmpdir is None:
                die("Failed to find a temp directory!")
        if not os.path.isdir(self.tmpdir):
            die("I can't seem to use the temp directory " + self.tmpdir)

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
        if self.studio is None and options.use_studio is None:
            die("Please specify which visual studio you want to use for building.\n" +
                "Use the --use-studio command line switch, can be '2005', '2008' or '2010'")
        elif options.use_studio is not None:
            if options.use_studio == "2005":
                self.studio = os.environ.get("VS80COMNTOOLS")
                self.generator = "Visual Studio 8 2005"
            elif options.use_studio == "2008":
                self.studio = os.environ.get("VS90COMNTOOLS")
                self.generator = "Visual Studio 9 2008"
            elif options.use_studio == "2010":
                self.studio = os.environ.get("VS100COMNTOOLS")
                self.generator = "Visual Studio 10"
            else:
                die ("Unknown visual studio " + options.use_studio)

    def setenv(self):
        WORKSPACE = os.environ.get("WORKSPACE")
        if not WORKSPACE:
            die("Environment variable WORKSPACE is not set, is this really a Jenkins build?!")
        os.environ["SAFIR_RUNTIME"] = os.path.join(WORKSPACE,"safir","runtime")
        os.environ["SAFIR_SDK"] = os.path.join(WORKSPACE,"safir","sdk")
        os.environ["PATH"] = os.environ.get("PATH") + ";" + os.path.join(os.environ.get("SAFIR_RUNTIME"),"bin")
        ADA_PROJECT_PATH = (os.environ.get("ADA_PROJECT_PATH") + ";") if os.environ.get("ADA_PROJECT_PATH") else ""
        os.environ["ADA_PROJECT_PATH"] = ADA_PROJECT_PATH + os.path.join(os.environ.get("SAFIR_SDK"),"ada")
        os.environ["XML_ADA_VER"] = "4.2"
        #java path gets set by jenkins

        #set up K: drive:
        bindir = os.path.join(os.environ.get("SAFIR_RUNTIME"),"bin")
        if not os.path.isdir(bindir):
            mkdir(bindir)
        ret = subprocess.call(("subst","/d", "k:"))
        print "'subst /d k:' exited with return code", ret
        subprocess.call(("subst","k:",bindir))
        print "'subst k:", bindir + "' exited with return code", ret

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

        self.__run_command(("ctest -T Test --output-on-failure"),
                           "Test", directory, allow_fail = True)
            

    def dobmake(self):
        """run the dobmake command"""
        batpath = os.path.join(self.tmpdir,"build2.bat")
        
        with open(batpath,"w") as bat:
            bat.write("@echo off\n" +
                      "call \"" + os.path.join(self.studio,"vsvars32.bat") + "\"\n" +
                      "\"" + os.path.join(SAFIR_RUNTIME,"bin","dobmake.py") + "\" -b --rebuild") #batch mode (no gui)
            if force_config == "Debug" and force_extra_config == "None":
                bat.write (" --no-cpp-release --default-config Debug")
            bat.write("\n")
        process = subprocess.Popen(batpath,stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        
        logger.logOutput(process)
        #result = process.communicate()
        #buildlog.write(result[0].replace("\r\n","\n"))
        #logger.log(result[0].replace("\r\n","\n"))
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
        olddir = os.getcwd();
        os.chdir(self.generator)
    
        self.__run_command(("cmake " +
                            "-G \"" + self.generator + "\" " +
                            ".."),
                           "Configure", directory)

        solution = find_sln()
        
        for config in configs:
            self.__run_command("cmake --build . --config " + config + ("--clean-first" if clean else ""),
                               "Build " + config, directory)

            if install:
                self.__run_command("cmake --build . --config " + config + " --target Install",
                                   "Install " + config, directory)

        os.chdir(olddir)

    def __nmake_build(self,directory,configs,install):
        """build a directory using nmake"""
        for config in configs:                
            self.__run_command("cmake -D CMAKE_BUILD_TYPE:string=" + config + " " +
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
                  "call \"" + os.path.join(self.studio,"vsvars32.bat") + "\"\n" +
                  cmd)
        bat.close()
        #buildlog.write("<h4>" + description + " '" + what + "'</h4><pre style=\"color: green\">" + cmd + "</pre>\n")
        logger.log(description + " " + what + ": '" + cmd + "'", "command")
        process = subprocess.Popen(batpath,stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        logger.logOutput(process)
        #result = process.communicate()
        #buildlog.write("<pre>")
        #buildlog.write(result[0].replace("\r\n","\n"))
        #logger.log(result[0].replace("\r\n","\n"))
        #buildlog.write("</pre>")
        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + cmd + "' for " + what)
            else:
                #buildlog.write("This command failed, but failure of this particular command is non-fatal to the build process, so I'm continuing\n")
                logger.log("This command failed, but failure of this particular command is non-fatal to the build process, so I'm continuing\n")


class UnixGccBuilder(object):
    def __init__(self):
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
        #ada builds (with gnatmake) will look at environment to determine parallellism
        os.environ["NUMBER_OF_PROCESSORS"] = str(self.num_jobs)

    @staticmethod
    def can_use():
        import sys
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

    def build(self, directory, configs, install):
        """build a directory using make"""
        config = configs[0]
        self.__run_command(("cmake",
                            "-D", "CMAKE_BUILD_TYPE:string=" + config,
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

        self.__run_command(("ctest",
                            "-T", "Test", "--output-on-failure"),
                           "Test", directory, allow_fail = True)

    def dobmake(self):
        """run the dobmake command"""
        cmd = (os.path.join(SAFIR_RUNTIME,"bin","dobmake.py"), "-b", "--rebuild") #batch mode (no gui)
        if force_config == "Debug" and force_extra_config == "None":
            cmd += ("--no-cpp-release","--default-config","Debug")
        process = subprocess.Popen(cmd,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT)
        logger.logOutput(process)
        if process.returncode != 0:
            die("Failed to run dobmake")

    def __run_command(self, cmd, description, what, allow_fail = False):
        """Run a command"""
        logger.log(description + " " + what + ": '" + " ".join(cmd) + "'", "command")
        process = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        logger.logOutput(process)
        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + " ".join(cmd) + "' for " + what)
            else:
                logger.log("This command failed, but failure of this particular command is non-fatal to the build process, so I'm continuing")


def in_skip_list(line):
    "Check the argument against all regexps in the skip-list"
    import re
    for expr in skip_list:
        p=re.compile(expr)
        if p.search(line):
            return True
    return False;


def build_dir(directory, configs, builder, install = True):
    if not os.path.isdir(directory):
        die("Failed to enter " + directory + ", since it does not exist (or is not a directory)")
    olddir = os.getcwd();
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

                    junitfile.write("  <testcase name=\"" + testName + "\" classname=\"" + suite_name + "\" time=\"" + str(executionTime) + "\"")
                    if testStatus == "passed":
                        """success"""
                        junitfile.write("/>\n")
                    else:
                        """failure"""
                        output = getText(meas.getElementsByTagName("Value")[0].childNodes)
                        #replace characters that can't be in xml. in a rather inefficient way...
                        output = output.\
                            replace('&',"&amp;").\
                            replace('<',"&lt;").\
                            replace('>',"&gt;").\
                            replace('"',"&quot;").\
                            replace('\'',"&apos;")
                        junitfile.write(">\n    <error message=\"" + exitCode + "(" + exitValue +  ")\">" + 
                                        output +
                                        "\n</error>\n  </testcase>\n")
        junitfile.write("</testsuite>")


def run_test_suite(directory, suite_name, builder):
    olddir = os.getcwd();
    os.chdir(directory)
    try:
        builder.test(directory)
        translate_results_to_junit(suite_name)
    finally:
        os.chdir(olddir)

def dobmake(builder):
    logger.log("Running dobmake","header");
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

    global buildlog
    
    olddir = os.getcwd();

    split_line = None

    for line in command_file:
        if line[0] == '#' or line.strip() == "":
            continue

        last_line = split_line
        split_line = line.split()
        
        if in_skip_list(line.strip()):
            logger.log("- Skipping " + str(split_line) + ", matches skip-list","header")
            sys.stdout.flush()
            continue
                
        command = split_line[0]

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
            build_dir(directory,(cf,),builder,False) # build, but do not install
            run_test_suite(directory, suite_name, builder)
        elif command == "dobmake":
            dobmake(builder)
        else:
            die("Got unknown command '" + command + "'")
        
        sys.stdout.flush()
        sys.stderr.flush()

    os.chdir(olddir)
    
    logger.log("Result", "header")
    logger.log("Build completed successfully!")


#actual code starts here
logger = Logger()

try:
    main()
except FatalError, e:
    logger.log("Result", "header")
    logger.log("Build script failed: " + str(e))
    logger.close()
    sys.exit(1)

logger.close()
sys.exit(0)
