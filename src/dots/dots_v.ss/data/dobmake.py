#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
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
from __future__ import print_function
import os, glob, subprocess, threading, sys, stat, shutil, time, traceback, re, platform
from optparse import OptionParser

try:
    # 3.x name
    from configparser import SafeConfigParser
except ImportError:
    # 2.x name
    from ConfigParser import SafeConfigParser


try:
    # 3.x name
    import tkinter
    import tkinter.font as tkfont
except ImportError:
    # 2.x name
    import Tkinter as tkinter
    import tkFont as tkfont

def log(data):
    print(data)
    sys.stdout.flush()


def is_64_bit():
    #Detecting this is a lot more complex than it should be.
    #See http://stackoverflow.com/questions/2764356/python-get-windows-os-version-and-architecture
    #and http://bytes.com/topic/python/answers/509764-detecting-64bit-vs-32bit-linux
    #This will work reasonably well on our supported systems:
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
            subprocess.Popen(("cmake28", "--version"),stdout = subprocess.PIPE).communicate()
            cmake.cmake_executable = "cmake28"
        except:
            cmake.cmake_executable = "cmake"
    return cmake.cmake_executable

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
            ncpus = int(os.environ["NUMBER_OF_PROCESSORS"]);
            if ncpus > 0:
                return ncpus
    return 1 # Default


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

class DobmakeError(Exception):
    def __init__(self,msg):
        Exception.__init__(self,msg)

def die(msg):
    logger.writeError(msg + "\n")
    raise DobmakeError("\n" + msg)

def physical_memory():
    if sys.platform.startswith("linux"):
        with open("/proc/meminfo") as a_file:
            meminfo = a_file.read()
        match = re.search(r"MemTotal:\s*([0-9]*) kB", meminfo)
        return int(match.group(1))/1024
    else:
        return None

def chmod(path):
    flags = stat.S_IWRITE | stat.S_IREAD
    if os.path.isdir(path):
        flags = flags| stat.S_IEXEC
    try:
        os.chmod(path,flags)
    except Exception:
        pass

def remove(path):
    if not os.path.exists(path):
        return
    if os.path.isfile(path):
        try:
            chmod(path)
            os.remove(path)
            return
        except Exception as e:
            die ("Failed to remove file " + path + ". Got exception " + str(e))
            
    for name in os.listdir(path):
        chmod(os.path.join(path,name))
        if os.path.isdir(os.path.join(path,name)):
            remove(os.path.join(path,name))
        else:
            try:
                os.remove(os.path.join(path,name))
            except Exception as e:
                die ("Failed to remove file " + os.path.join(path,name) + ". Got exception " + str(e))

    try:
        chmod(path)
        os.rmdir(path)

    except Exception as e:
        die ("Failed to remove directory " + path + ". Got exception " + str(e))

def save_installed_files_manifest():
    installed_files = open(os.path.join(SAFIR_SDK,"dots","dots_generated","installed_files.txt"), "a")
    manifest = open("install_manifest.txt", "r")

    shutil.copyfileobj(manifest, installed_files)
    
    manifest.close()
    installed_files.close()

def uninstall():
    logger.writeHeader("Uninstalling...\n")
        
    path = os.path.join(SAFIR_SDK,"dots","dots_generated","installed_files.txt")
    if not os.path.exists(path):
        logger.write("Can't find " + path + ", files are probably already uninstalled.\n")        
        return
    files = open(path)
    for file in files:
        remove(file[0:-1])

        # Remove the directory if it is empty
        try:
            os.rmdir(os.path.dirname(file))
        except OSError:
            # not empty, continue
            pass
            
    files.close()

    remove(path)
    
    logger.write("Uninstalling done.\n")
    
def get_config_file():
    return os.path.join(SAFIR_SDK,"dots","dots_generated","dobmake.ini")

def replace_non_ascii(s):
    if type(s) is str:
        return "".join([c if ord(c) < 128 else '#' for c in s])
    else:
        return "".join([chr(c) if c < 128 else '#' for c in s])

class Logger(object):
    def __init__(self):
        self.logdata = list()
        self.delta = list()
        self.lock = threading.Lock()
        self.lastTag = None

    def emitStartTag(self,tag):
        pass

    def emitEndTag(self,tag):
        pass

    def write(self, data, tag = "normal"):
        if data is None: return
        if no_gui:
            if tag != self.lastTag:
                self.emitEndTag(self.lastTag)                
                self.emitStartTag(tag)
                self.lastTag = tag
            sys.stdout.write(data)
            sys.stdout.flush()

        else:
            self.lock.acquire()
            try:
                self.logdata.append((data,tag))
                self.delta.append((data,tag))
            finally:
                self.lock.release()


    def writeHeader(self, data):
        self.write(data,"header")

    def writeTitle(self, data):
        self.write(data,"title")

    def writeCommand(self, data):
        self.write(data,"command")

    def writeError(self,data):
        self.write(data,"error")


    class PollThread(threading.Thread):
        def __init__(self, process, logger, strip_cr):
            threading.Thread.__init__(self)
            self.process = process
            self.logger = logger
            self.strip_cr = strip_cr and (sys.platform == "win32")

        def run(self):
            while True:
                data = self.process.stdout.readline()
                if not data:
                    break
                replace_non_ascii(data)
                if self.strip_cr:
                    data = data.replace('\r','')
                self.logger.write(data,"pre")

    def logOutput(self, process, strip_cr = False):
        thread = Logger.PollThread(process, self, strip_cr)
        thread.start()
        thread.join()
        process.wait()

    def getDelta(self):
        self.lock.acquire()
        try:
            data = self.delta
            self.delta = list()
        finally:
            self.lock.release()
        return data

    def close(self):
        log("logger.close")

buildType = "build"
default_config="RelWithDebInfo"
build_java = True
build_ada = True
build_cpp_release = True
build_cpp_debug = True
no_gui = False
if sys.platform == "win32": #target_architecture is only set on windows platfoms!
    target_architecture = None

def parse_command_line():
    parser = OptionParser()
    parser.add_option("--clean", action="store_true",dest="clean",default=False,
                      help="Remove all intermediary build files")
    parser.add_option("--rebuild", action="store_true",dest="rebuild",default=False,
                      help="Remove all old build files and installed files before starting the build")
    parser.add_option("--uninstall", action="store_true",dest="uninstall",default=False,
                      help="Remove all installed files")
    parser.add_option("--no-ada", action="store_true",dest="no_ada",default=False,
                      help="Dont attempt to build Ada code")
    parser.add_option("--no-java", action="store_true",dest="no_java",default=False,
                      help="Dont attempt to build Java code")
    parser.add_option("--no-cpp-debug", action="store_true",dest="no_cpp_debug",default=False,
                      help="Dont attempt to build cpp debug code")      
    parser.add_option("--no-cpp-release", action="store_true",dest="no_cpp_release",default=False,
                      help="Dont attempt to build cpp release code")      
    parser.add_option("--default-config", action="store",type="string",dest="default_config",default="RelWithDebInfo",
                      help="Configuration for the other languages. RelWithDebInfo is default.")
    parser.add_option("--no-gui", "--batch", "-b", action="store_true",dest="stdoutlog",default=False,
                      help="Run in batch (non-gui) mode")

    if is_64_bit() and sys.platform == "win32":
        parser.add_option("--target", "--arch", action="store",dest="arch",choices=("x86","x86-64"),
                          help="The target architecture to build for. x86 or x86-64 are valid args. "
                          "This value will be stored in dobmake.ini, so it only needs to specified "
                          "if the stored value needs to be changed, or if no value is stored.")

    (options,args) = parser.parse_args()

    if options.clean and options.rebuild:
        log("Specifying both --clean and --rebuild is redundant, ignoring --clean")

    if options.uninstall and options.rebuild:
        log("Specifying both --uninstall and --rebuild is redundant, ignoring --uninstall")         

    global buildType

    if options.clean:
        buildType="clean"
    elif options.uninstall:
        buildType="uninstall"
        
    if options.clean and options.uninstall:
        buildType="clean_and_uninstall"
    
    if options.rebuild:
        buildType="rebuild"

    global default_config
    default_config = options.default_config

    if sys.platform == "win32":
        #builds on 32-bit windows always get x86 set.
        if not is_64_bit():
            options.arch = "x86"
        
        global target_architecture
        target_architecture = options.arch #can be None if not set on cmd line

    global build_java
    global build_ada
    
    if options.no_ada:
        build_ada = False
    if options.no_java:
        build_java = False

    global build_cpp_debug
    global build_cpp_release

    if options.no_cpp_debug:
        build_cpp_debug = False
    if options.no_cpp_release:
        build_cpp_release = False

    global no_gui
    no_gui = options.stdoutlog

def invoke_in_dir(function, directory):
    olddir = os.getcwd()
    os.chdir(directory)
    try:
        function()
    finally:
        os.chdir(olddir)


def run_dots_depends():
    logger.writeHeader("Checking dependencies in dou files\n")
    logger.writeCommand("Running dots_depends...\n")
    process = subprocess.Popen(os.path.join(SAFIR_RUNTIME,"bin","dots_depends"),
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.STDOUT,
                               universal_newlines=True)    
    logger.logOutput(process)
    if process.returncode != 0:
        die("dots_depends failed!")
        

class BuilderBase(object):
    def __init__(self):
        #We need to limit ourselves a little bit in how
        #many parallel jobs we perform. Each job may use
        #up to ~100Mb of memory.
        try:
            self.num_jobs = num_cpus() + 1
        
            mem_per_job = 100
            memory = physical_memory()
            if memory is not None and memory / self.num_jobs < mem_per_job:
                self.num_jobs = max(1,memory / mem_per_job)
        except:
            self.num_jobs = 2

    def __run_cmake(self, cmd, description, what, allow_fail = False):
        """Run a command"""
        logger.writeHeader(description + " '" + what + "'\n")
        command = [cmake(),]
        command += cmd
        logger.writeCommand(" ".join(cmd) + "\n")
        process = subprocess.Popen(command,
                                   stdout=subprocess.PIPE, 
                                   stderr=subprocess.STDOUT)
        #universal_newlines does not work with cmake on windows, so we
        #ask logger to do stripping for us.
        logger.logOutput(process, strip_cr = True)
        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + " ".join(cmd) + "' for " + what)
            else:
                logger.write("This command failed, but failure of this particular command is non-fatal to the build process, so I'm continuing\n")    

    def __generate_code(self):
        what = "Process DOU files - Generate code"

        rebuild = "TRUE" if buildType == "rebuild" else "FALSE"

        self.__run_cmake(("-G", self.cmake_generator,
                          "-D", "REBUILD=" + rebuild,
                          "."),
                         "Configure", what)
        command = ["--build", ".", 
                   "--target", self.install_target]

        command += self.target_specific_build_cmds()
        
        self.__run_cmake(command,
                         "Build and install", what)

        save_installed_files_manifest()

    def __build(self, config, cpp, dotnet, java, ada):
        what = "dots_generated"        
        rebuild = "TRUE" if buildType == "rebuild" else "FALSE"

        self.__run_cmake(("-G", self.cmake_generator,
                          "-D", "CMAKE_BUILD_TYPE:string=" + config,
                          "-D", "NO_CXX:string="+ str(not cpp), 
                          "-D", "NO_DOTNET:string="+ str(not dotnet), 
                          "-D", "NO_JAVA:string=" + str(not java),
                          "-D", "NO_ADA:string="+ str(not ada), 
                          "-D", "REBUILD=" + rebuild,
                          os.pardir),
                         "Configure", what)
            
        command = ["--build", ".", 
                   "--target", self.install_target]
        
        command += self.target_specific_build_cmds()
        
        self.__run_cmake(command,
                         "Build and install " + default_config, what)
        
        save_installed_files_manifest()
        


    def build(self):
        invoke_in_dir(lambda: self.__generate_code(), "gen")

        run_dots_depends()

        default_builds = ["dotnet",]
        other_builds = []
        if build_java:
            default_builds.append("java")
        if build_ada:
            default_builds.append("ada")
        #if default_config is a release build
        if default_config.find("Rel") != -1:
            other_config = "Debug"
            if build_cpp_release:
                default_builds.append("cpp")
            if build_cpp_debug:
                other_builds.append("cpp")
        else:
            other_config = "RelWithDebInfo"
            if build_cpp_debug:
                default_builds.append("cpp")
            if build_cpp_release:
                other_builds.append("cpp")

        if len(default_builds) != 0:
            mkdir(default_config)
            invoke_in_dir(lambda: self.__build(default_config,
                                               "cpp" in default_builds,
                                               "dotnet" in default_builds,
                                               "java" in default_builds,
                                               "ada" in default_builds),
                          default_config)

        if len (other_builds) != 0 and sys.platform == "win32":
            mkdir(other_config)
            invoke_in_dir(lambda: self.__build(other_config,
                                               "cpp" in other_builds,
                                               "dotnet" in other_builds,
                                               "java" in other_builds,
                                               "ada" in other_builds),
                          other_config)

        
    def clean(self):
        logger.write("Cleaning!\n")
                       
        remove("others")                  
        remove("build")
        remove("Debug")
        remove("Release")
        remove("RelWithDebInfo")
        remove("MinSizeRel")
        remove("cpp")
        remove("java")
        remove("ada")
        remove("dotnet")
        remove("tags")
        remove("dll_imports.cpp")

        # empty file, must exist
        tmp_file = open("cmake_depend.txt", "w")
        tmp_file.close()

        for name in os.listdir("gen"):
            if name != "CMakeLists.txt":
                remove(os.path.join("gen",name))
    
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
        
        self.__read_config()

        if os.environ.get("VSINSTALLDIR") is None:
            #looks like we need to load vcvarsall.bat to get environment variables
            self.__setup_build_environment()

    def __read_config(self):
        cfg = SafeConfigParser()
        cfgpath = get_config_file()

        add_section = False
        if not os.path.exists(cfgpath):
            log("Could not find " + cfgpath + ", trying to create one")
            add_section = True
        else:
            cfg.read(cfgpath)
            try:
                cfg.get("main","VSPATH")
                cfg.get("main","target_architecture")
            except:
                 add_section = True
        
        if add_section:
            DIR = os.environ.get("VSINSTALLDIR")
            if DIR is None:
                die("Could not work out which studio you are using, make sure you run dobmake.py in a Visual Studio command prompt.")
            if target_architecture is None:
                die("No target_architecture set. Please specify on command line, using the --target switch.")
            if not cfg.has_section("main"):
                cfg.add_section("main")
            cfg.set("main","VSPATH",os.path.join(DIR,"Common7","Tools"))
            cfg.set("main","target_architecture",target_architecture)
            with open(cfgpath,"w") as configfile:
                cfg.write(configfile)           
        elif target_architecture is not None and cfg.get("main","target_architecture") != target_architecture:
            #if it is set on the command line we want to write that value
            cfg.set("main","target_architecture",target_architecture)
            with open(cfgpath,"w") as configfile:
                cfg.write(configfile)           
        
        self.studio = os.path.normcase(os.path.normpath(cfg.get("main","VSPATH")))
        self.arch = cfg.get("main","target_architecture")

        if not os.path.isdir(self.studio) or not os.path.isfile(os.path.join(self.studio,os.pardir,os.pardir,"VC","vcvarsall.bat")):
            die("Something seems to have happened to your dobmake.ini or Visual Studio installations!"+
                "\nVSPATH (in dots_generated/dobmake.ini) does not seem to point to a valid path." +
                "\nTry to delete dots_generated/dobmake.ini and run dobmake.py in a Visual Studio command prompt.")

    def __setup_build_environment(self):
        """Find vcvarsall.bat and load the relevant environment variables from it.
        This function is inspired (but not copied, for licensing reasons) by the one in python distutils2 msvc9compiler.py"""
        vcvarsall = os.path.join(self.studio,os.pardir,os.pardir,"VC","vcvarsall.bat")

        #use uppercase only in this variable!
        required_variables = set(["LIB", "LIBPATH", "PATH", "INCLUDE"])
        optional_variables = set(["PLATFORM",])
        wanted_variables = required_variables | optional_variables #union

        arch = "x86" if self.arch == "x86" else "amd64"
        cmd = '"%s" %s & set' % (vcvarsall, arch)
        proc = subprocess.Popen(cmd,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,
                                universal_newlines = True)
        output = proc.communicate()[0]
        if proc.returncode != 0:
            die ("Failed to fetch environment variables out of vcvarsall.bat: " + output)
        
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
                os.environ[name] = value
                found_variables.add(name)
                
        if len(required_variables - found_variables) != 0:
            die("Failed to find all expected variables in vcvarsall.bat! Missing = " + str(required_variables - found_variables))


    @staticmethod
    def can_use():
        return sys.platform == "win32"

    def target_specific_build_cmds(self):
        if self.have_jom:
            return (("--", "/nologo", "/j", str(self.num_jobs)))
        else:
            return (("--", "/nologo"))

class UnixGccBuilder(BuilderBase):
    def __init__(self):
        super(UnixGccBuilder, self).__init__()

        self.install_target = "install"
        self.cmake_generator = "Unix Makefiles"


    @staticmethod
    def can_use():
        return sys.platform.startswith("linux")

    def target_specific_build_cmds(self):
        return (("--", "-j", str(self.num_jobs)))

def run_dots_configuration_check(check):
    os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = os.path.join(SAFIR_SDK,"dots","dots_generated","dobmake_config", check)

    process = subprocess.Popen(os.path.join(SAFIR_RUNTIME,"bin","dots_configuration_check"),
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.STDOUT,
                               universal_newlines=True)    
    logger.logOutput(process)

    os.environ.pop("SAFIR_TEST_CONFIG_OVERRIDE")
    if process.returncode != 0:
        die("dots_configuration_check failed! There is something wrong with your dou and dom files")
        
def check_config():
    logger.writeHeader("Checking dou and dom files\n")
    logger.writeCommand("dou and dom files located under $(SAFIR_RUNTIME)/data/text/dots/classes/ are checked.\n")
    run_dots_configuration_check("postcheck")

def check_config_dots_generated():
    logger.writeHeader("Checking dou and dom files\n")
    logger.writeCommand("dou and dom files located under $(SAFIR_SDK)/dots/dots_generated/ are checked.\n")
    
    run_dots_configuration_check("precheck")
        
def load_gui():
    global MainDialog
    
    class MainDialog(tkinter.Frame):
        def __init__(self,parent, builder):
            tkinter.Frame.__init__(self,parent)
            self.cfgpath = None
            self.cfg = None
            self.parent = parent
            self.builder = builder
            self.grid(row=0,column=0,sticky=tkinter.W + tkinter.E + tkinter.N + tkinter.S)

            self.parent.grid_rowconfigure(0, weight=1)
            self.parent.grid_columnconfigure(0, weight=1)

            self.buildType=tkinter.StringVar()
            self.buildType.set(buildType)
            self.buildAda = tkinter.IntVar()
            self.buildJava = tkinter.IntVar()
            self.buildReleaseCpp = tkinter.IntVar()
            self.buildDebugCpp = tkinter.IntVar()
            self.getGuiConfiguration()
            self.createWidgets()
            if sys.platform == "win32":
                self.cppDebugBoxChanged()
            self.buildTypeChanged()
            self.commandList = list()
            self.commandThread = None
            self.after(10,self.updateLog)
            self.start_time = time.time()


        def writeGuiConfiguration(self):
            with open(self.cfgpath,"w") as configfile:
                self.cfg.write(configfile)
                
        def getGuiConfiguration(self):
            self.cfgpath = get_config_file()
            self.cfg = SafeConfigParser()

            add_section = False
            if not os.path.exists(self.cfgpath):
                add_section = True
            else:
                self.cfg.read(self.cfgpath)
                if not self.cfg.has_section("gui-settings"):
                    add_section = True
            if add_section:
                self.cfg.add_section("gui-settings")
                self.cfg.set("gui-settings", "build_ada", "1")
                self.cfg.set("gui-settings", "build_java", "1")
                self.cfg.set("gui-settings", "build_release_cpp", "1")
                self.cfg.set("gui-settings", "build_debug_cpp", "1")
                
            self.buildAda.set(self.cfg.getint("gui-settings", "build_ada"))
            self.buildJava.set(self.cfg.getint("gui-settings", "build_java"))
            self.buildReleaseCpp.set(self.cfg.getint("gui-settings", "build_release_cpp"))
            self.buildDebugCpp.set(self.cfg.getint("gui-settings", "build_debug_cpp"))
            
            self.writeGuiConfiguration()           
        

        def createWidgets(self):
            self.grid_rowconfigure(5, weight=1)
            self.grid_columnconfigure(4, weight=1)

            tkinter.Label(self, 
                          text="Build directory:").grid(row=0,column=0,sticky=tkinter.W + tkinter.N)

            tkinter.Label(self, 
                          text=os.path.join(SAFIR_SDK,"dots","dots_generated"), 
                          relief=tkinter.SUNKEN).grid(row=0,column=1,columnspan=5,sticky=tkinter.W + tkinter.N)


            tkinter.Label(self, 
                          text="Task:").grid(row=1,column=0,sticky=tkinter.W + tkinter.N)

            tkinter.Radiobutton(self,
                                text="Build",
                                variable=self.buildType,
                                value="build",
                                command=self.buildTypeChanged).grid(row=1,column=1, sticky=tkinter.W + tkinter.N)

            tkinter.Radiobutton(self,
                                text="Rebuild",
                                variable=self.buildType,
                                value="rebuild",
                                command=self.buildTypeChanged).grid(row=1,column=2, sticky=tkinter.W + tkinter.N)

            tkinter.Radiobutton(self,
                                text="Clean",
                                variable=self.buildType,
                                value="clean",
                                command=self.buildTypeChanged).grid(row=1,column=3, sticky=tkinter.W + tkinter.N)
            tkinter.Label(self, 
                          text="Languages:").grid(row=2,column=0,sticky=tkinter.W + tkinter.N)
            
            cppBox = tkinter.Checkbutton(self,
                                         text="C++",
                                         state="disabled")
            cppBox.grid(row=2,column=1,sticky=tkinter.W + tkinter.N)
            cppBox.select()
            
            csBox = tkinter.Checkbutton(self,
                                        text="C#",
                                        state="disabled")
            csBox.grid(row=2,column=2,sticky=tkinter.W + tkinter.N)
            csBox.select()
            
            
            self.adaBox = tkinter.Checkbutton(self,
                                              text="Ada",
                                              variable = self.buildAda,
                                              command=self.adaBoxChanged)
            self.adaBox.grid(row=2,column=3,sticky=tkinter.W + tkinter.N)
            
            self.javaBox = tkinter.Checkbutton(self,
                                               text="Java",
                                               variable = self.buildJava,
                                               command=self.javaBoxChanged)
            self.javaBox.grid(row=2,column=4,sticky=tkinter.W + tkinter.N)
            
            if sys.platform == "win32":
                tkinter.Label(self,
                              text="C++ build type:").grid(row=3,column=0,sticky=tkinter.W + tkinter.N)

                cppReleaseBuildBox = tkinter.Checkbutton(self,
                                                         text="Release",
                                                         state="disabled")
                cppReleaseBuildBox.grid(row=3,column=1,sticky=tkinter.W + tkinter.N)
                cppReleaseBuildBox.select()
            
                self.cppDebugBuildBox = tkinter.Checkbutton(self,
                                                            text="Debug",
                                                            variable = self.buildDebugCpp,
                                                            command=self.cppDebugBoxChanged)
                self.cppDebugBuildBox.grid(row=3,column=2,sticky=tkinter.W + tkinter.N)             
            
            tkinter.Label(self, text="Build output").grid(row=4,column=0,sticky=tkinter.W + tkinter.N)
          
            scrollbar = tkinter.Scrollbar(self)            
            self.output = tkinter.Text(self, yscrollcommand=scrollbar.set)
            
            scrollbar.grid(row=5,column=10,columnspan=1,sticky=tkinter.W + tkinter.E + tkinter.N + tkinter.S)

            scrollbar.config(command=self.output.yview)
            self.output.config(yscrollcommand=scrollbar.set)

            self.output.grid(row=5,column=0,columnspan=10,sticky=tkinter.W + tkinter.E + tkinter.N + tkinter.S)
            self.output.tag_config("pre",foreground="black")
            self.output.tag_config("title",font = tkfont.Font(family="Times",size=-24,weight="bold"))
            self.output.tag_config("command",foreground="blue", underline=True)
            self.output.tag_config("error",foreground="red")
            self.output.tag_config("header",font = tkfont.Font(family="Times",size=-18))
            
            self.runButton = tkinter.Button(self, text="Run", command=self.run)
            self.runButton.grid(row=6,column=4,sticky=tkinter.E )
            tkinter.Button(self, text="Clear",command=self.clear).grid(row=6,column=5,pady=10,padx=10,sticky=tkinter.E)
            self.cancelButton = tkinter.Button(self, text="Quit",command=self.quit)
            self.cancelButton.grid(row=6,column=6,sticky=tkinter.E)


        def clear(self):
            self.output.delete(1.0,tkinter.END)

        def adaBoxChanged(self):
            global build_ada
            build_ada = (self.buildAda.get() == 1)
            self.cfg.set("gui-settings", "build_ada", str(self.buildAda.get()))
            self.writeGuiConfiguration()

        def javaBoxChanged(self):
            global build_java
            build_java = (self.buildJava.get() == 1)
            self.cfg.set("gui-settings", "build_java", str(self.buildAda.get()))
            self.writeGuiConfiguration()           
            
        def cppDebugBoxChanged(self):
            global build_cpp_debug
            build_cpp_debug = (self.buildDebugCpp.get() == 1)
            self.cfg.set("gui-settings", "build_debug_cpp", str(self.buildDebugCpp.get()))
            self.writeGuiConfiguration()

        def buildTypeChanged(self):
            global buildType
            buildType = self.buildType.get()
            if self.buildType.get() == "clean":
                self.javaBox.select()
                self.javaBox.config(state ="disabled")
                
                self.adaBox.select()
                self.adaBox.config(state ="disabled")
            else:
                self.javaBox.config(state ="normal")
                self.adaBox.config(state ="normal")
            self.adaBoxChanged()
            self.javaBoxChanged()

        def updateLog(self):
            self.after(100,self.updateLog)
            delta = logger.getDelta()
            for chunk in delta:
                self.output.insert(tkinter.END,chunk[0],chunk[1])
                self.output.see(tkinter.END)

        def run(self):
            self.runButton.config(state="disabled")
            self.cancelButton.config(state="disabled")
            self.start_time = time.time()
            if buildType == "clean" or buildType == "rebuild":
                self.commandList.append(self.builder.clean)

            if buildType == "uninstall" or buildType == "rebuild":
                self.commandList.append(uninstall)

            if buildType == "build" or buildType == "rebuild":
                self.commandList.append(check_config_dots_generated)
                self.commandList.append(self.builder.build)
                self.commandList.append(check_config)
            self.runCommandList()

        def pollCommandCompletion(self):
            if self.commandThread.isAlive():
                self.after(10,self.pollCommandCompletion)
            else:
                self.commandThread.join()
                self.commandList.pop(0)

                if self.commandThread.result != None:
                    if self.commandThread.traceback is not None:
                        log(self.commandThread.traceback)
                    self.runButton.config(state="normal")
                    self.cancelButton.config(state="normal")
                    self.commandList = list()

                self.runCommandList()

        def runCommandList(self):
            if len(self.commandList) != 0:
                self.commandThread = MainDialog.CommandThread(self.commandList[0]);
                self.commandThread.start()
                self.after(10,self.pollCommandCompletion)
            else:
                self.runButton.config(state="normal")
                self.cancelButton.config(state="normal")
                logger.write("dobmake took " + time.strftime("%H:%M:%S", time.gmtime(time.time() - self.start_time)) + "\n")

        class CommandThread(threading.Thread):
            def __init__(self, command):
                threading.Thread.__init__(self)
                self.command = command
                self.result = None
                self.traceback = None

            def run(self):
                try:
                    self.result = self.command()
                except Exception as e:
                    self.result = e
                    self.traceback = traceback.format_exc()

def main():
    global SAFIR_RUNTIME
    global SAFIR_SDK
    SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
    SAFIR_SDK = os.environ.get("SAFIR_SDK")
    if SAFIR_RUNTIME == None or SAFIR_SDK == None:
        log("You need to have both SAFIR_RUNTIME and SAFIR_SDK set")
        sys.exit(1)
    
    start_time = time.time()
    parse_command_line()

    global logger

    logger = Logger()

    if VisualStudioBuilder.can_use():
        builder = VisualStudioBuilder()
    elif UnixGccBuilder.can_use():
        builder = UnixGccBuilder()
    else:
        log("Failed to work out what builder to use!")
        return 1

    olddir = os.getcwd();
    dir = os.path.join(SAFIR_SDK,"dots","dots_generated")
    os.chdir(dir)

    if no_gui:
        log("Running in batch mode")
        
        if buildType == "clean" or buildType == "clean_and_uninstall" or buildType == "rebuild":
            builder.clean()
        
        if buildType == "uninstall" or buildType == "clean_and_uninstall" or buildType == "rebuild":
            uninstall();
            
        if buildType == "build" or buildType == "rebuild":
            check_config_dots_generated()
            builder.build()
            check_config()
        

        log ("Success! (dobmake took " + time.strftime("%H:%M:%S", time.gmtime(time.time() - start_time)) + ")")
    else:
        load_gui()
        application = tkinter.Tk()
        dlg = MainDialog(application,builder)
        application.title("Dobmake")
        application.mainloop()
    
    os.chdir(olddir)

    return 0



if __name__ == "__main__":
    sys.exit(main())
