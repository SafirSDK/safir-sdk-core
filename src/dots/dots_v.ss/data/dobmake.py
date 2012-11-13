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
from __future__ import print_function
import os, glob, subprocess, threading, sys, stat, shutil, time, traceback, re
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
    buildlog.writeError(msg + "\n")
    raise DobmakeError("\n" + msg)

def physical_memory():
    if not sys.platform.startswith("linux"):
        die ("physical_memory() is only implemented on linux")
    with open("/proc/meminfo") as file:
        meminfo = file.read()
    match = re.search(r"MemTotal:\s*([0-9]*) kB", meminfo)
    return int(match.group(1))/1024


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
    buildlog.writeHeader("Uninstalling...\n")
        
    path = os.path.join(SAFIR_SDK,"dots","dots_generated","installed_files.txt")
    if not os.path.exists(path):
        buildlog.write("Can't find " + path + ", files are probably already uninstalled.\n")        
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
    
    buildlog.write("Uninstalling done.\n")
    
def get_config_file():
    return os.path.join(SAFIR_SDK,"dots","dots_generated","dobmake.ini")

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
        if not no_gui:
            self.write(time.strftime("%H:%M:%S", time.localtime()) + "\n","command")
        self.write(data,"command")

    def writeError(self,data):
        self.write(data,"error")


    class PollThread(threading.Thread):
        def __init__(self, process, logger):
            threading.Thread.__init__(self)
            self.process = process
            self.logger = logger

        def run(self):
            newlines = 0
            while True:
                data = self.process.stdout.readline()
                if not data:
                    break
                self.logger.write(data,"pre")


    def logOutput(self, process):
        thread = Logger.PollThread(process,self)
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
default_config="Release"
build_java = True
build_ada = True
build_cpp_release = True
build_cpp_debug = True
no_gui = False
target_architecture = None

def parse_command_line():
    parser = OptionParser()
    parser.add_option("--clean", action="store_true",dest="clean",default=False,
                      help="Remove all intermediary build files")
    parser.add_option("--rebuild", action="store_true",dest="rebuild",default=False,
                      help="Remove all old build files and installed files before starting the build")
    parser.add_option("--uninstall", action="store_true",dest="uninstall",default=False,
                      help="Remove all installed files")
    parser.add_option("--target",action="store",type="string",dest="target_architecture",default="x86",
                          help="Target architecture, x86 or x86-64")
    parser.add_option("--no-ada", action="store_true",dest="no_ada",default=False,
                      help="Dont attempt to build Ada code")
    parser.add_option("--no-java", action="store_true",dest="no_java",default=False,
                      help="Dont attempt to build Java code")
    parser.add_option("--no-cpp-debug", action="store_true",dest="no_cpp_debug",default=False,
                      help="Dont attempt to build cpp debug code")      
    parser.add_option("--no-cpp-release", action="store_true",dest="no_cpp_release",default=False,
                      help="Dont attempt to build cpp release code")      
    parser.add_option("--default-config", action="store",type="string",dest="default_config",default="Release",
                      help="Configuration for the other languages. Release is default.")
    parser.add_option("--no-gui", "--batch", "-b", action="store_true",dest="stdoutlog",default=False,
                      help="Run in batch (non-gui) mode")
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

    if options.target_architecture == "x86-64":
        if not is_64_bit():
            die("Target x86-64 can't be set since this is not a 64 bit OS")
    elif options.target_architecture != "x86":
        die("Unknown target architecture " + options.target_architecture)
    global target_architecture
    target_architecture = options.target_architecture

    global vcvarsall_arg
    if target_architecture == "x86":
        vcvarsall_arg = "x86"
    elif target_architecture == "x86-64":
        vcvarsall_arg = "x86_amd64"
    else:
        die("Unknown target architecture " + target_architecture)

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


def run_dots_depends():
    buildlog.writeHeader("Checking dependencies in dou files\n")
    buildlog.writeCommand("Running dots_depends...\n")
    process = subprocess.Popen(os.path.join(SAFIR_RUNTIME,"bin","dots_depends"),
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.STDOUT,
                               universal_newlines=True)    
    buildlog.logOutput(process)
    if process.returncode != 0:
        die("dots_depends failed!")
        
    

class VisualStudioBuilder(object):
    def __init__(self):
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
            except:
                 add_section = True
        
        if add_section:
            DIR = os.environ.get("VSINSTALLDIR")
            if DIR is None:
                die("Could not work out which studio you are using, make sure you run dobmake.py in a Visual Studio command prompt.")
            cfg.add_section("main")
            cfg.set("main","VSPATH",os.path.join(DIR,"Common7","Tools"))
            with open(cfgpath,"w") as configfile:
                cfg.write(configfile)           
        
        self.studio = os.path.normcase(os.path.normpath(cfg.get("main","VSPATH")))

        self.studio_install_dir = os.path.join(self.studio,"..", "..")

        if not os.path.isdir(self.studio) or not os.path.isfile(os.path.join(self.studio_install_dir,"VC","vcvarsall.bat")):
            die("Something seems to have happened to your dobmake.ini or Visual Studio installations!"+
                "\nVSPATH (in dots_generated/dobmake.ini) does not seem to point to a valid path." +
                "\nTry to delete dots_generated/dobmake.ini and run dobmake.py in a Visual Studio command prompt.")
        def getenv_and_normalize(variable):
            env = os.environ.get(variable)
            if env is not None:
                return os.path.normcase(os.path.normpath(env))
            return None
        
        VS80 = getenv_and_normalize("VS80COMNTOOLS")
        VS90 = getenv_and_normalize("VS90COMNTOOLS")
        VS100 = getenv_and_normalize("VS100COMNTOOLS")
        
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
            die("VSPATH (in dots_generated/dobmake.ini) is set to something I dont recognize\n" +
                "It should be either the value of %VS80COMNTOOLS%, %VS90COMNTOOLS% or %VS100COMNTOOLS%" +
                "\nTry to delete dots_generated/dobmake.ini and run dobmake.py in a Visual Studio command prompt.")

        #work out where to put temp files
        self.tmpdir = os.environ.get("TEMP")
        if self.tmpdir is None:
            self.tmpdir = os.environ.get("TMP")
            if self.tmpdir is None:
                die("Failed to find a temp directory!")
        if not os.path.isdir(self.tmpdir):
            die("I can't seem to use the temp directory " + self.tmpdir)
            
        #work out what compiler tools to use
        if target_architecture == "x86":
            self.vcvarsall_arg = "x86"
        elif target_architecture == "x86-64":
            self.vcvarsall_arg = "amd64"
        else:
            die("Unknown target architecture " + target_architecture)
            
    @staticmethod
    def can_use():
        VS80 = os.environ.get("VS80COMNTOOLS")
        VS90 = os.environ.get("VS90COMNTOOLS")
        VS100 = os.environ.get("VS100COMNTOOLS")
        return VS80 is not None or VS90 is not None or VS100 is not None

    def run_command(self, cmd, description, what, allow_fail = False):
        """Run a command"""            
        batpath = os.path.join(self.tmpdir,"build.bat")
        bat = open(batpath,"w")
        bat.write("@echo off\n" +
                  "call \"" + os.path.join(self.studio_install_dir,"VC","vcvarsall.bat") +  "\" " + self.vcvarsall_arg + "\n" +
                  cmd)
        bat.close()
        buildlog.writeHeader(description + " '" + what + "'\n")
        buildlog.writeCommand(cmd + "\n")
        process = subprocess.Popen(batpath,stdout=subprocess.PIPE, stderr=subprocess.STDOUT,universal_newlines=True)
        
        buildlog.logOutput(process)

        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + cmd + "' for " + what)
            else:
                buildlog.write("This command failed, but failure of this particular command is non-fatal to the build process, so I'm continuing\n")

    def run_command2(self, cmd, description, what, allow_fail = False):
        """Run a command"""
        
        buildlog.writeHeader(description + " '" + what + "'\n")
        buildlog.writeCommand(cmd + "\n")
        process = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)

        buildlog.logOutput(process)

        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + cmd + "' for " + what)
            else:
                buildlog.write("This command failed, but failure of this particular command is non-fatal to the build process, so I'm continuing\n")    


    def find_sln(self):
        sln_files = glob.glob("*.sln")
        if (len(sln_files) != 1):
            die("There is not exactly one sln file in " + os.getcwd() +
                ", either cmake failed to generate one or there is another one coming from somewhere else!")
        return sln_files[0]

    def build_cpp(self):
        what = "CPP - dots_generated"
        buildlog.writeHeader("Building C++ using "+self.generator + "\n")
        mkdir(self.generator)
        olddir = os.getcwd();
        os.chdir(self.generator)
        Rebuild = "FALSE"
        if buildType == "rebuild":
            Rebuild = "TRUE"
        try:
            self.run_command2(cmake() + " -G \""+ self.generator + "\" "+
                              "-D NO_JAVA:string=TRUE " + 
                              "-D NO_DOTNET:string=TRUE " + 
                              "-D NO_ADA:string=TRUE " + 
                              "-D REBUILD=" + Rebuild + " " +
                              "..",
                              "Configure", what)

            solution = self.find_sln()

            cppconfig = []
            if build_cpp_debug:
                cppconfig += ["Debug"]
            if build_cpp_release:
                cppconfig += ["Release"]
            for config in cppconfig:
                self.run_command(cmake() + " --build . --config " + config,
                                 "Build CPP " + config,what)
                self.run_command(cmake() + " --build . --config " + config +" --target Install",
                                 "Install CPP " + config,what)
                save_installed_files_manifest()
            
        finally:
            os.chdir(olddir)
            
    def build_others(self):
        what = "DOTNET JAVA ADA - dots_generated"
        buildlog.writeHeader("Building others using "+self.generator+"\n")
        olddir = os.getcwd();
        mkdir("others")
        os.chdir("others")
        Rebuild = "FALSE"
        if buildType == "rebuild":
            Rebuild = "TRUE"
        try:
            self.run_command(cmake() + " -G \""+ "NMake Makefiles" + "\" "+
                             "-D NO_CXX:string=TRUE " +
                             "-D NO_DOTNET:string=FALSE " +
                             "-D NO_ADA:string=" + str(not build_ada) + " " + 
                             "-D NO_JAVA:string=" + str(not build_java) + " " + 
                              "-D SAFIR_BUILD_TARGET_ARCHITECTURE:string=" + target_architecture + " " +
                             "-D REBUILD=" + Rebuild + " " +
                             "..",
                             "Configure", what)

            self.run_command(cmake() + " --build . --config " + default_config,
                             "Build " + default_config, what)
            
            self.run_command(cmake() + " --build . --config " + default_config  +" --target Install",
                             "Install " + default_config, what)
            save_installed_files_manifest()
        finally:
            os.chdir(olddir)
        
    def build_dots(self):
        what = "Process DOU files - dots_generated"
        buildlog.writeHeader("Building dots using "+self.generator+"\n")
        olddir = os.getcwd();
        os.chdir("gen")
        Rebuild = "FALSE"
        if buildType == "rebuild":
            Rebuild = "TRUE"
        try:
            # workaround for bug in CMake with VS2010
            if self.generator.startswith("Visual Studio 10"): 
                self.run_command((cmake() + " -G \""+ "NMake Makefiles" + "\" "+
                                  "-D REBUILD=" + Rebuild + " " +
                                  "."),
                                 "Configure", what)
            else:
                self.run_command((cmake() + " -G \""+ self.generator + "\" "+
                                  "-D REBUILD=" + Rebuild + " " +
                                  "."),
                                 "Configure", what)
                solution = self.find_sln()
                
            self.run_command(cmake() + " --build . --config " + default_config,
                             "Build " + default_config, what)

            self.run_command(cmake() + " --build . --config " + default_config  +" --target Install",
                             "Install " + default_config, what)
            

            save_installed_files_manifest()
                        
        finally:
            os.chdir(olddir)    
            
    def clean(self):
        buildlog.writeHeader("Cleaning...\n")
        
        remove("others")
        remove(self.generator)
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

        buildlog.write("Cleaning done.\n")
           
    def build(self):
        self.build_dots()
        run_dots_depends()
        self.build_others()
        self.build_cpp()


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
        return sys.platform.startswith("linux2")
    

    def run_command(self, cmd, description, what, allow_fail = False):
        """Run a command"""
        buildlog.writeHeader(description + " '" + what + "'\n")
        buildlog.writeCommand(" ".join(cmd) + "\n")
        process = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)
        buildlog.logOutput(process)
        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + " ".join(cmd) + "' for " + what)
            else:
                buildlog.write("This command failed, but failure of this particular command is non-fatal to the build process, so I'm continuing\n")    

    def build_dots(self):
        what = "Process DOU files - dots_generated"        
        olddir = os.getcwd()
        os.chdir("gen")
        Rebuild = "FALSE"
        if buildType == "rebuild":
            Rebuild = "TRUE"
        try:
            self.run_command((cmake(),
                              "-D", "CMAKE_BUILD_TYPE:string=" + default_config,
                              "-D", "REBUILD=" + Rebuild,
                              "."),
                             "Configure", what)
            
            self.run_command(("make","-j",str(self.num_jobs)),
                             "Build " + default_config, what)
            self.run_command(("make","-j",str(self.num_jobs),"install"),
                             "Install " + default_config, what)

            save_installed_files_manifest()
        finally:
            os.chdir(olddir)

    def build_others(self):
        what = "dots_generated"        
        mkdir("build")
        olddir = os.getcwd()
        os.chdir("build")
        Rebuild = "FALSE"
        if buildType == "rebuild":
            Rebuild = "TRUE"
        try:
            self.run_command((cmake(),
                              "-D", "CMAKE_BUILD_TYPE:string=" + default_config,
                              "-D", "NO_ADA:string="+ str(not build_ada), 
                              "-D", "NO_JAVA:string=" + str(not build_java),
                              "-D", "REBUILD=" + Rebuild,
                              ".."),
                             "Configure", what)
            
            os.putenv("MONO_PATH", SAFIR_RUNTIME + "/bin")

            self.run_command(("make","-j",str(self.num_jobs)),
                             "Build " + default_config, what)
            self.run_command(("make","-j",str(self.num_jobs),"install"),
                             "Install " + default_config, what)

            save_installed_files_manifest()
            
        finally:
            os.chdir(olddir)
        
    def build(self):
        self.build_dots()
        run_dots_depends()
        self.build_others()

    def clean(self):
        buildlog.write("Cleaning!\n")
                                         
        remove("build")
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

def run_dots_configuration_check():
    process = subprocess.Popen(os.path.join(SAFIR_RUNTIME,"bin","dots_configuration_check"),stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)    
    buildlog.logOutput(process)
    if process.returncode != 0:
        die("dots_configuration_check failed! There is something wrong with your dou and dom files")
        
def check_config():
    buildlog.writeHeader("Checking dou and dom files\n")
    buildlog.writeCommand("dou and dom files located under $(SAFIR_RUNTIME)/data/text/dots/classes/ are checked.\n")
    run_dots_configuration_check()

def check_config_dots_generated():
    buildlog.writeHeader("Checking dou and dom files\n")
    buildlog.writeCommand("dou and dom files located under $(SAFIR_SDK)/dots/dots_generated/ are checked.\n")
    
    # Set a "special" environment variable for the sub-process. The env variable determines where dots_kernel should
    # load dou/dom files from
    os.putenv("SAFIR_DOTS_CLASSES_DIR", os.path.join(SAFIR_SDK,"dots","dots_generated"))

    run_dots_configuration_check()
        
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
            delta = buildlog.getDelta()
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
                buildlog.write("dobmake took " + time.strftime("%H:%M:%S", time.gmtime(time.time() - self.start_time)) + "\n")

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

    global buildlog

    buildlog = Logger()

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
