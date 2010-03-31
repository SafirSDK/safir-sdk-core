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

import os, glob, subprocess, threading, sys, stat

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
    raise DobmakeError(msg)

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
        except Exception, e:
            die ("Failed to remove file " + path + ". Got exception " + str(e))
            
    for name in os.listdir(path):
        chmod(os.path.join(path,name))
        if os.path.isdir(os.path.join(path,name)):
            remove(os.path.join(path,name))
        else:
            try:
                os.remove(os.path.join(path,name))
            except Exception, e:
                die ("Failed to remove file " + os.path.join(path,name) + ". Got exception " + str(e))

    try:
        chmod(path)
        os.rmdir(path)
    except Exception, e:
        die ("Failed to remove directory " + path + ". Got exception " + str(e))


class Logger(object):
    def __init__(self):
        self.logdata = list()
        self.delta = list()
        self.lock = threading.Lock()
        self.lastTag = None

    def emitStartTag(self,tag):
        if not html:
            return
        if tag == "header":
            sys.stdout.write("<h3>\n")
        if tag == "title":
            sys.stdout.write("<title>\n")
        if tag == "command":
            sys.stdout.write("<pre style=\"color: green\">\n")
        if tag == "pre":
            sys.stdout.write("<pre>\n")
        if tag == "error":
            sys.stdout.write("<font color=\"red\">\n")
        

    def emitEndTag(self,tag):
        if not html:
            return
        if tag == "header":
            sys.stdout.write("</h3>\n")
        if tag == "title":
            sys.stdout.write("</title>\n")
        if tag == "command":
            sys.stdout.write("</pre>\n")
        if tag == "pre":
            sys.stdout.write("</pre>\n")
        if tag == "error":
            sys.stdout.write("</font>\n")

    def write(self, data, tag = "normal"):
        if data is None: return
        if no_gui:
            if tag != self.lastTag:
                self.emitEndTag(self.lastTag)                
                self.emitStartTag(tag)
                self.lastTag = tag
            sys.stdout.write(data)

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
        def __init__(self, process, logger):
            threading.Thread.__init__(self)
            self.process = process
            self.logger = logger

        def run(self):
            newlines = 0
            while True:
                data = self.process.stdout.readline()
                if len(data) == 0: # and self.process.returncode is not None:
                    return
                data = data.replace("\r\n","\n")
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
        print "logger.close"


buildType = "build"
default_config="Release"

build_java = True
build_ada = True
no_gui = False
html = False

def parse_command_line():
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("--clean", action="store_true",dest="clean",default=False,
                      help="Remove all intermediary build files")
    parser.add_option("--rebuild", action="store_true",dest="rebuild",default=False,
                      help="Remove all old build files before starting the build")
    parser.add_option("--no-ada", action="store_true",dest="no_ada",default=False,
                      help="Dont attempt to build Ada code")
    parser.add_option("--no-java", action="store_true",dest="no_java",default=False,
                      help="Dont attempt to build Java code")
    parser.add_option("--no-gui", "--batch", "-b", action="store_true",dest="stdoutlog",default=False,
                      help="Run in batch (non-gui) mode")
    parser.add_option("--html-output", action="store_true",dest="html",default=False,
                      help="When running in batch mode, markup the output with html tags.")
    (options,args) = parser.parse_args()

    if options.clean and options.rebuild:
        print "Specifying both --clean and --rebuild is redundant, ignoring --clean"

    global buildType
    
    if options.clean:
        buildType="clean"
    if options.rebuild:
        buildType="rebuild"

    global build_java
    global build_ada
    
    if options.no_ada:
        build_ada = False
    if options.no_java:
        build_java = False

    global no_gui
    no_gui = options.stdoutlog


    global html
    html = options.html

class VisualStudioBuilder(object):
    def __init__(self):
        from ConfigParser import ConfigParser
        cfg = ConfigParser()
        
        cfgpath = os.path.join(SAFIR_SDK,"dots","dots_generated","dobmake.ini")
        if not os.path.exists(cfgpath):
            print "Could not find " + cfgpath + ", trying to create one"
            DIR = os.environ.get("VSINSTALLDIR")
            if DIR is None:
                die("Could not work out which studio you are using, please write your own " + cfgpath)
            cfg.add_section("main")
            cfg.set("main","VSPATH",os.path.join(DIR,"Common7","Tools"))
            configfile = open(cfgpath,"wb")
            try:
                cfg.write(configfile)
            finally:
                configfile.close()
            cfg = ConfigParser() #make a new parser so that we're going on from here like nothing happened.

        cfg.read(cfgpath)
        self.studio = os.path.normcase(os.path.normpath(cfg.get("main","VSPATH")))

        if not os.path.isdir(self.studio) or not os.path.isfile(os.path.join(self.studio,"vsvars32.bat")):
            die("Something seems to have happened to your dobmake.ini or Visual Studio installations!\n"+
                "VSPATH (in dots_generated/dobmake.ini) does not seem to point to a valid path")
        def getenv_and_normalize(variable):
            env = os.environ.get(variable)
            if env is not None:
                return os.path.normcase(os.path.normpath(env))
            return None

        VS80 = getenv_and_normalize("VS80COMNTOOLS")
        VS90 = getenv_and_normalize("VS90COMNTOOLS")
        
        if self.studio == VS80:
            self.generator = "Visual Studio 8 2005"
        elif self.studio == VS90:
            self.generator = "Visual Studio 9 2008"
        else:
            die("VSPATH (in dots_generated/dobmake.ini) is set to something I dont recognize\n" +
                "It should be either the value of %VS80COMNTOOLS% or %VS90COMNTOOLS%")

        #work out where to put temp files
        self.tmpdir = os.environ.get("TEMP")
        if self.tmpdir is None:
            self.tmpdir = os.environ.get("TMP")
            if self.tmpdir is None:
                die("Failed to find a temp directory!")
        if not os.path.isdir(self.tmpdir):
            die("I can't seem to use the temp directory " + self.tmpdir)

        #work out whether to use devenv.com or vcexpress.exe to build
        basepath = os.path.join(self.studio,"..","IDE")
        vcexpresspath = os.path.join(basepath,"vcexpress.exe")
        devenvpath = os.path.join(basepath,"devenv.com")
        if os.path.exists(vcexpresspath):
            self.build_cmd = vcexpresspath
        elif  os.path.exists(devenvpath):
            self.build_cmd = devenvpath
        else:
            die("I couldn't find either vcexpress.exe or devenv.com, so I dont know how to build stuff!")
            
    @staticmethod
    def can_use():
        VS80 = os.environ.get("VS80COMNTOOLS")
        VS90 = os.environ.get("VS90COMNTOOLS")
        return VS80 is not None or VS90 is not None

    def run_command(self, cmd, description, what, allow_fail = False):
        """Run a command"""
        batpath = os.path.join(self.tmpdir,"build.bat")
        bat = open(batpath,"w")
        bat.write("@echo off\n" +
                  "call \"" + os.path.join(self.studio,"vsvars32.bat") + "\"\n" +
                  cmd)
        bat.close()
        buildlog.writeHeader(description + " '" + what + "'\n")
        buildlog.writeCommand(cmd + "\n")
        process = subprocess.Popen(batpath,stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        
        buildlog.logOutput(process)

        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + cmd + "' for " + what)
            else:
                buildlog.write("This command failed, but failure of this particular command is non-fatal to the build process, so I'm continuing\n")

    def run_command2(self, cmd, description, what, allow_fail = False):
        """Run a command"""
        import subprocess
        
        buildlog.writeHeader(description + " '" + what + "'\n")
        buildlog.writeCommand(cmd + "\n")
        process = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

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
        what = "dots_generated"
        buildlog.writeHeader("Building C++ using "+self.generator + "\n")
        mkdir(self.generator)
        olddir = os.getcwd();
        os.chdir(self.generator)
        try:
            self.run_command2("cmake -G \""+ self.generator + "\" "+
                              "-D NO_JAVA:string=TRUE " + 
                              "-D NO_DOTNET:string=TRUE " + 
                              "-D NO_ADA:string=TRUE" + 
                              "-D NO_DOU_INSTALL:string=TRUE " + 
                              "-D NO_GENERATE_CODE:string=TRUE " + 
                              "..",
                              "Configure", what)

            solution = self.find_sln()
            
            for config in ("Debug","Release"):
                self.run_command("\"" + self.build_cmd + "\" " + solution + " /build " +config +" /Project INSTALL",
                                 "Build and Install " + config,what)
        finally:
            os.chdir(olddir)
            
    def build_others(self):
        what = "dots_generated"
        buildlog.writeHeader("Building others using "+self.generator+"\n")
        mkdir("others")
        olddir = os.getcwd();
        os.chdir("others")
        try:
            self.run_command(("cmake -G \""+ self.generator + "\" "+
                              "-D NO_CXX:string=TRUE " +
                              "-D NO_ADA:string=" + str(not build_ada) + " " + 
                              "-D NO_JAVA:string=" + str(not build_java) + " " + 
                              ".."),
                             "Configure", what)
            solution = self.find_sln()
            
            self.run_command("\"" + self.build_cmd + "\" " + solution + " /build " + default_config  +" /Project INSTALL",
                             "Build and Install " + default_config, what)
        finally:
            os.chdir(olddir)
        

    def clean(self):
        buildlog.write("Cleaning!\n")

        remove("others")
        remove(self.generator)
        remove("cpp")
        remove("java")
        remove("ada")
        remove("dotnet")
        remove("tags")
        for name in os.listdir("gen"):
            if name != "CMakeLists.txt":
                remove(os.path.join("gen",name))
            
    def build(self):
        self.build_others()
        self.build_cpp()


class UnixGccBuilder(object):
    def __init__(self):
        self.num_jobs = num_cpus() + 1

    @staticmethod
    def can_use():
        return sys.platform == "linux2"
    

    def run_command(self, cmd, description, what, allow_fail = False):
        """Run a command"""
        buildlog.writeHeader(description + " '" + what + "'\n")
        buildlog.writeCommand(" ".join(cmd) + "\n")
        process = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        buildlog.logOutput(process)
        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + " ".join(cmd) + "' for " + what)
            else:
                buildlog.write("This command failed, but failure of this particular command is non-fatal to the build process, so I'm continuing\n")    

    def build(self):
        what = "dots_generated"        
        mkdir("build")
        olddir = os.getcwd()
        os.chdir("build")
        try:
            self.run_command(("cmake",
                              "-D", "CMAKE_BUILD_TYPE:string=" + default_config,
                              "-D", "NO_ADA:string="+ str(not build_ada), 
                              "-D", "NO_JAVA:string=" + str(not build_java),
                              ".."),
                             "Configure", what)
            
            self.run_command(("make","-j",str(self.num_jobs),"install"),
                             "Build and Install " + default_config, what)
        finally:
            os.chdir(olddir)

    def clean(self):
        buildlog.write("Cleaning!\n")

        remove("build")
        remove("cpp")
        remove("java")
        remove("ada")
        remove("dotnet")
        remove("tags")
        for name in os.listdir("gen"):
            if name != "CMakeLists.txt":
                remove(os.path.join("gen",name))


def check_config():
    buildlog.writeHeader("Attempting to load dou and dom files\n")
    buildlog.writeCommand("dots_configuration_check\n")
    process = subprocess.Popen("dots_configuration_check",stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

    buildlog.logOutput(process)
    if process.returncode != 0:
        die("dots_configuration_check failed! There is something wrong with your dou and dom files")

def load_gui():
    import Tkinter, tkMessageBox, tkFont
    global MainDialog
    class MainDialog(Tkinter.Frame):
        def __init__(self,parent, builder):
            Tkinter.Frame.__init__(self,parent)
            self.parent = parent
            self.builder = builder
            self.grid(row=0,column=0,sticky=Tkinter.W + Tkinter.E + Tkinter.N + Tkinter.S)

            self.parent.grid_rowconfigure(0, weight=1)
            self.parent.grid_columnconfigure(0, weight=1)

            self.buildType=Tkinter.StringVar()
            self.buildType.set(buildType)
            self.buildAda = Tkinter.IntVar()
            self.buildAda.set(1)
            self.buildJava = Tkinter.IntVar()
            self.buildJava.set(1)
            self.createWidgets()
            self.buildTypeChanged()
            self.commandList = list()
            self.commandThread = None
            self.after(10,self.updateLog)



        def createWidgets(self):
            self.grid_rowconfigure(4, weight=1)
            self.grid_columnconfigure(4, weight=1)

            Tkinter.Label(self, 
                          text='Build directory:').grid(row=0,column=0,sticky=Tkinter.W + Tkinter.N)

            Tkinter.Label(self, 
                          text=os.path.join(SAFIR_SDK,"dots","dots_generated"), 
                          relief=Tkinter.SUNKEN).grid(row=0,column=1,columnspan=5,sticky=Tkinter.W + Tkinter.N)


            Tkinter.Label(self, 
                          text='Task:').grid(row=1,column=0,sticky=Tkinter.W + Tkinter.N)

            Tkinter.Radiobutton(self,
                                text="Build",
                                variable=self.buildType,
                                value="build",
                                command=self.buildTypeChanged).grid(row=1,column=1, sticky=Tkinter.W + Tkinter.N)

            Tkinter.Radiobutton(self,
                                text="Rebuild",
                                variable=self.buildType,
                                value="rebuild",
                                command=self.buildTypeChanged).grid(row=1,column=2, sticky=Tkinter.W + Tkinter.N)

            Tkinter.Radiobutton(self,
                                text="Clean",
                                variable=self.buildType,
                                value="clean",
                                command=self.buildTypeChanged).grid(row=1,column=3, sticky=Tkinter.W + Tkinter.N)
            Tkinter.Label(self, 
                          text='Languages:').grid(row=2,column=0,sticky=Tkinter.W + Tkinter.N)
            
            cppBox = Tkinter.Checkbutton(self,
                                         text="C++",
                                         state="disabled")
            cppBox.grid(row=2,column=1,sticky=Tkinter.W + Tkinter.N)
            cppBox.select()
            
            csBox = Tkinter.Checkbutton(self,
                                        text="C#",
                                        state="disabled")
            csBox.grid(row=2,column=2,sticky=Tkinter.W + Tkinter.N)
            csBox.select()
            
            
            self.adaBox = Tkinter.Checkbutton(self,
                                              text="Ada",
                                              variable = self.buildAda,
                                              command=self.adaBoxChanged)
            self.adaBox.grid(row=2,column=3,sticky=Tkinter.W + Tkinter.N)
            
            self.javaBox = Tkinter.Checkbutton(self,
                                               text="Java",
                                               variable = self.buildJava,
                                               command=self.javaBoxChanged)
            self.javaBox.grid(row=2,column=4,sticky=Tkinter.W + Tkinter.N)
            
            Tkinter.Label(self, text='Build output').grid(row=3,column=0,sticky=Tkinter.W + Tkinter.N)
          
            scrollbar = Tkinter.Scrollbar(self)            
            self.output = Tkinter.Text(self, yscrollcommand=scrollbar.set)
            
            scrollbar.grid(row=4,column=10,columnspan=1,sticky=Tkinter.W + Tkinter.E + Tkinter.N + Tkinter.S)

            scrollbar.config(command=self.output.yview)
            self.output.config(yscrollcommand=scrollbar.set)

            self.output.grid(row=4,column=0,columnspan=10,sticky=Tkinter.W + Tkinter.E + Tkinter.N + Tkinter.S)
            self.output.tag_config("pre",foreground="darkgrey")
            self.output.tag_config("title",font = tkFont.Font(family="Times",size=-24,weight="bold"))
            self.output.tag_config("command",foreground="darkgreen")
            self.output.tag_config("error",foreground="red")
            self.output.tag_config("header",font = tkFont.Font(family="Times",size=-18))
            
            self.runButton = Tkinter.Button(self, text="Run", command=self.run)
            self.runButton.grid(row=5,column=4,sticky=Tkinter.E )
            Tkinter.Button(self, text="Clear",command=self.clear).grid(row=5,column=5,pady=10,padx=10,sticky=Tkinter.E)
            self.cancelButton = Tkinter.Button(self, text="Quit",command=self.quit)
            self.cancelButton.grid(row=5,column=6,sticky=Tkinter.E)


        def clear(self):
            self.output.delete(1.0,Tkinter.END)

        def adaBoxChanged(self):
            global build_ada
            build_ada = (self.buildAda.get() == 1)

        def javaBoxChanged(self):
            global build_java
            build_java = (self.buildJava.get() == 1)

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
                self.output.insert(Tkinter.END,chunk[0],chunk[1])
                self.output.see(Tkinter.END)

        def run(self):
            self.runButton.config(state="disabled")
            self.cancelButton.config(state="disabled")
            if buildType == "clean" or buildType == "rebuild":
                self.commandList.append(self.builder.clean)

            if buildType == "build" or buildType == "rebuild":
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
                        print self.commandThread.traceback
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

        class CommandThread(threading.Thread):
            def __init__(self, command):
                threading.Thread.__init__(self)
                self.command = command
                self.result = None
                self.traceback = None

            def run(self):
                try:
                    self.result = self.command()
                except Exception, e:
                    self.result = e
                    import traceback
                    self.traceback = traceback.format_exc()

def main():
    global SAFIR_RUNTIME
    global SAFIR_SDK
    SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
    SAFIR_SDK = os.environ.get("SAFIR_SDK")
    if SAFIR_RUNTIME == None or SAFIR_SDK == None:
        print "You need to have both SAFIR_RUNTIME and SAFIR_SDK set"
        sys.exit(1)
    
    import time
    start_time = time.time()
    parse_command_line()

    global buildlog

    buildlog = Logger()

    if VisualStudioBuilder.can_use():
        builder = VisualStudioBuilder()
    elif UnixGccBuilder.can_use():
        builder = UnixGccBuilder()
    else:
        print "Failed to work out what builder to use!"
        return 1

    olddir = os.getcwd();
    dir = os.path.join(SAFIR_SDK,"dots","dots_generated")
    os.chdir(dir)

    if no_gui:
        print "Running in batch mode"
        
        if buildType == "clean" or buildType == "rebuild":
            builder.clean()
            
        if buildType == "build" or buildType == "rebuild":
            builder.build()
            check_config()
        
        print "Success! (dobmake took " + str(time.time() - start_time) + " seconds)"

    else:
        import Tkinter, tkMessageBox, tkFont
        load_gui()
        application = Tkinter.Tk()
        dlg = MainDialog(application,builder)
        application.title("Dobmake")
        application.mainloop()
    
    os.chdir(olddir)

    return 0



if __name__ == "__main__":
    sys.exit(main())
