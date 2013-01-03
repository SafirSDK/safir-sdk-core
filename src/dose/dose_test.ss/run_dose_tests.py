#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2011 (http://www.safirsdk.com)
#
# Created by: Lars Hagstrom (lars@foldspace.nu)
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

""" This is a python 2.x script! Will not work on python 3.x, currently. """

import sys, subprocess, os, shutil, difflib, time, xml.dom.minidom, glob, re, traceback, stat, signal
from xml.sax.saxutils import escape

def rmdir(directory):
    if os.path.exists(directory):
        try:
            shutil.rmtree(directory)
        except OSError:
            print "Failed to remove directory, will retry"
            time.sleep(0.2)
            shutil.rmtree(directory)

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

class Parameters:
    def __init__(self):
        from optparse import OptionParser
        import tempfile

        parser = OptionParser()
        parser.add_option("--jenkins", action="store_true",dest="jenkins",default=False,
                          help="Set up the needed environment variables for a Jenkins automated build," +
                          "and respect the langX matrix variables")
        parser.add_option("--autostart-slave", action="store_true", dest="autostart_slave",default=False,
                          help="Automatically start a multinode slave (Server_1) through jenkins." +
                          "This is only applicable when using --multinode, and is implied when using --jenkins")
        parser.add_option("--multinode", action="store_true",dest="multinode",default=False,
                          help="Run multinode tests instead of standalone tests")
        parser.add_option("--slave", action="store_true",dest="slave",default=False,
                          help="Be a multinode slave")
        parser.add_option("--no-java", action="store_true",dest="no_java",default=False,
                          help="Do not run java partners")
        parser.add_option("--lang0", action="store",dest="lang0",default="cpp",
                          help="Which partner 0 to run. [default: %default]. "
                          "If --jenkins is specified the lang0 environment variable will override this.")
        parser.add_option("--lang1", action="store",dest="lang1",default="cpp",
                          help="Which partner 1 to run. [default: %default]. "
                          "If --jenkins is specified the lang1 environment variable will override this.")
        parser.add_option("--lang2", action="store",dest="lang2",default="cpp",
                          help="Which partner 2 to run. [default: %default]. "
                          "If --jenkins is specified the lang2 environment variable will override this.")

        (options,args) = parser.parse_args()

        self.lang0 = options.lang0
        self.lang1 = options.lang1
        self.lang2 = options.lang2
        
        self.multinode = options.multinode
        self.slave = options.slave
        self.standalone = not options.multinode and not options.slave

        self.no_java = options.no_java

        if options.autostart_slave and not self.multinode:
            print "--autostart-slave can only be used together with --multinode"
            sys.exit(1)

        self.autostart_jenkins_slave = options.autostart_slave or (options.jenkins and self.multinode)

        if options.jenkins:
            WORKSPACE = os.environ.get("WORKSPACE")
            if not WORKSPACE:
                print "Environment variable WORKSPACE is not set, is this really a Jenkins build?!"
                sys.exit(1)
            os.environ["SAFIR_RUNTIME"] = os.path.join(WORKSPACE,"safir","runtime")
            if sys.platform.startswith("linux"):
                LD_LIBRARY_PATH = (os.environ.get("LD_LIBRARY_PATH") + ":") if os.environ.get("LD_LIBRARY_PATH") else ""
                os.environ["LD_LIBRARY_PATH"] = LD_LIBRARY_PATH + os.path.join(os.environ.get("SAFIR_RUNTIME"),"lib")
                separator = ":"
            else:
                separator = ";"
            os.environ["PATH"] = os.environ.get("PATH") + separator + os.path.join(os.environ.get("SAFIR_RUNTIME"),"bin")
        
            #Get env variables if they're set
            tmp = os.environ.get("lang0")
            if tmp is not None:
                self.lang0 = tmp;
            tmp = os.environ.get("lang1")
            if tmp is not None:
                self.lang1 = tmp;
            tmp = os.environ.get("lang2")
            if tmp is not None:
                self.lang2 = tmp;
        
            #make stdout unbuffered
            sys.stdout = os.fdopen(sys.stdout.fileno(), 'w', 0)

        
        self.SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
        if not self.SAFIR_RUNTIME:
            print "Cannot run without SAFIR_RUNTIME being set. Consider using --jenkins if this is a Jenkins build"
            sys.exit(1)
        
        self.dose_main_cmd = (os.path.join(self.SAFIR_RUNTIME,"bin","dose_main"),)
        self.foreach_cmd = (os.path.join(self.SAFIR_RUNTIME,"bin","foreach"),)
        self.swre_logger_cmd = (os.path.join(self.SAFIR_RUNTIME,"bin","swre_logger"),)
        
        self.dose_test_cpp_cmd = (os.path.join(self.SAFIR_RUNTIME, "bin", "dose_test_cpp"),)
        self.dose_test_ada_cmd = (os.path.join(self.SAFIR_RUNTIME, "bin", "dose_test_ada"),)
        self.dose_test_dotnet_cmd = (os.path.join(self.SAFIR_RUNTIME, "bin", "dose_test_dotnet.exe"),)
        self.dose_test_java_cmd = ("java", "-Xcheck:jni", "-Xfuture", "-jar", 
                                   os.path.join(self.SAFIR_RUNTIME, "bin", "dose_test_java.jar"),)
        
        self.testcases_path = os.path.join(self.SAFIR_RUNTIME, "data", "text", "dose_test", "testcases")
        self.dose_test_sequencer_cmd = (os.path.join(self.SAFIR_RUNTIME, "bin", "dose_test_sequencer"),
                                        "-d",
                                        self.testcases_path,
                                        #"--context", "-1", #random context
                                        "-l", self.lang0, self.lang1, self.lang2,
                                        #"--first", "1",
                                        #"--last", "1"
                                        )
        
        self.expected_output_path = os.path.join(self.SAFIR_RUNTIME, "data", "text", "dose_test", 
                                                 "output_multinode" if self.multinode else "output_standalone")
        self.parameters_path = os.path.join(self.SAFIR_RUNTIME,
                                            "data", 
                                            "text", 
                                            "dots", 
                                            "classes", 
                                            "definitions",
                                            "safir_core", 
                                            "config")

        self.PersistenceParameters_path = os.path.join(self.parameters_path, 
                                                       "Safir.Dob.PersistenceParameters.dou")
        self.NodeParameters_path = os.path.join(self.parameters_path, 
                                                "Safir.Dob.NodeParameters.dou")
        self.ThisNodeParameters_path = os.path.join(self.parameters_path, 
                                                    "Safir.Dob.ThisNodeParameters.dou")
        self.QueueParameters_path = os.path.join(self.parameters_path, 
                                                 "Safir.Dob.QueueParameters.dou")
        self.DistributionChannelParameters_path = os.path.join(self.parameters_path, 
                                                 "Safir.Dob.DistributionChannelParameters.dou")

        self.test_parameters_path = os.path.join(self.SAFIR_RUNTIME,
                                                 "data","text","dose_test","test_parameters")

        self.test_QueueParameters_path = os.path.join(self.test_parameters_path,
                                                      "Safir.Dob.QueueParameters.dou")
        self.test_multinode_DistributionChannelParameters_path = \
            os.path.join(self.test_parameters_path, "multinode",
                         "Safir.Dob.DistributionChannelParameters.dou")

        self.test_standalone_NodeParameters_path = \
            os.path.join(self.test_parameters_path, "standalone",
                         "Safir.Dob.NodeParameters.dou")

        self.test_multinode_NodeParameters_path = \
            os.path.join(self.test_parameters_path, "multinode",
                         "Safir.Dob.NodeParameters.dou")

        self.tempdir = tempfile.mkdtemp(prefix="dose_test_backup")

def getText(nodelist):
    rc = []
    for node in nodelist:
        if node.nodeType == node.TEXT_NODE:
            rc.append(node.data)
    return ''.join(rc)

def UpdateConfig(parameters):
    print "** Updating configuration files"
    #backup dou files to temporary directory
    shutil.copy2(parameters.PersistenceParameters_path, parameters.tempdir)
    shutil.copy2(parameters.NodeParameters_path, parameters.tempdir)
    shutil.copy2(parameters.ThisNodeParameters_path, parameters.tempdir)
    shutil.copy2(parameters.QueueParameters_path, parameters.tempdir)
    shutil.copy2(parameters.DistributionChannelParameters_path, parameters.tempdir)
    
    # Update PersistenceParameters
    dom = xml.dom.minidom.parse(parameters.PersistenceParameters_path)
    for param in dom.getElementsByTagName("parameter"):
        name = getText(param.getElementsByTagName("name")[0].childNodes)
        value = param.getElementsByTagName("value")[0]
        if name == "SystemHasPersistence":
            value.childNodes[0].data = "False"
        if name == "TestMode":
            value.childNodes[0].data = "True"
    with open(parameters.PersistenceParameters_path,"w") as file:
       file.write(dom.toxml())

    # Update NodeParameters
    if parameters.standalone:
        shutil.copy2(parameters.test_standalone_NodeParameters_path, parameters.parameters_path)
    else:
        shutil.copy2(parameters.test_multinode_NodeParameters_path, parameters.parameters_path)

    # Update ThisNodeParameters
    if parameters.slave:
        dom = xml.dom.minidom.parse(parameters.ThisNodeParameters_path)
        for param in dom.getElementsByTagName("parameter"):
            name = getText(param.getElementsByTagName("name")[0].childNodes)
            value = param.getElementsByTagName("value")[0]
            if name == "NodeNumber":
                value.childNodes[0].data = "1"
        with open(parameters.ThisNodeParameters_path,"w") as file:
            file.write(dom.toprettyxml())


    # Update QueueParameters
    shutil.copy2(parameters.test_QueueParameters_path, parameters.parameters_path)

    # Update DistributionChannelParameters
    if parameters.multinode or parameters.slave:
        shutil.copy2(parameters.test_multinode_DistributionChannelParameters_path, parameters.parameters_path)

def RestoreConfig(parameters):
    print "** Restoring configuration files"
    #restore dou files from temporary directory
    dous = glob.glob(os.path.join(parameters.tempdir,"*.dou"))
    for dou in dous:
        shutil.copy2(os.path.join(parameters.tempdir,dou), parameters.parameters_path)
    rmdir(parameters.tempdir)

class Results:
    def __init__(self):
        self.num_tc_failed = 0
        self.num_tc_performed = 0

    def write_test_result(self, expr, name, output):
        with open(name + ".junit.xml","w") as junitfile:
            junitfile.write("<?xml version=\"1.0\"?>\n<testsuite>\n  <testcase name=\"" + name + "\" classname=\"dose_test\"")
            if expr:
                """success"""
                junitfile.write("/>\n")
            else:
                """failure"""
                print " -",name, "failed!"
                self.num_tc_failed += 1
                junitfile.write(">\n    <error message=\"Failed\">" + 
                                output +
                                "\n</error>\n  </testcase>\n")
            junitfile.write("</testsuite>")
        return expr
    
    def split(self,data):
        result = dict()
        split_data = data.split("==========================================================================\n")
        for chunk in split_data:
            if len(chunk) == 0:
                continue
            chunk_lines = chunk.splitlines(True)
            number = int(chunk_lines[0].replace("TESTCASE ",""))
            result[number] = chunk_lines
        return result
    
    def create_tc_dict(self,parameters):
        testfilenames = os.listdir(parameters.testcases_path)
        testcases = dict()
        pattern = re.compile("([0-9]*)-(.*).xml")
        for tf in testfilenames:
            match = pattern.match(tf)
            if match:
                testcases[int(match.group(1))] = match.group(1) + "-" + match.group(2)
        return testcases
    
    
    def write_diff(self,tcname, tcno, diff0, diff1, diff2):
        res = len(diff0) == 0 and len(diff1) == 0 and len(diff2) == 0
        output = ""
        if not res:
            output += "(A line with a '-' in front means that it is missing, and one with a '+' in front means that it has been added, compared to the expected result.)\n\n"
            if len(diff0) != 0:
                output += "Partner 0 diff:\n" + escape("".join(diff0)) + "\n\n"
            if len(diff1) != 0:
                output += "Partner 1 diff:\n" + escape("".join(diff1)) + "\n\n"
            if len(diff2) != 0:
                output += "Partner 2 diff:\n" + escape("".join(diff2)) + "\n\n"
            
        self.write_test_result(res, tcname, output)
        return res
    
    def check_output(self,parameters):
        print "** Parsing test output"
        resdir = glob.glob("run0-*")
        if len(resdir) == 0:
            print "No output directories found, cannot parse output!"
            return
        elif len(resdir) != 1:
            print "Spurious directories where I am running, cannot parse output!"
            print str(resdir)
            return
        resdir = resdir[0]
    
        tcnames = self.create_tc_dict(parameters)
    
        output = list((None,None,None))
        expected_output = list((None,None,None))
        alltc = list((None,None,None))
    
        for partner in range(3):
            filename = "partner" + str(partner) + ".txt"
            with open(os.path.join(resdir,filename)) as file:
                output[partner] = self.split(file.read())
            with open(os.path.join(parameters.expected_output_path,filename)) as file:
                expected_output[partner] = self.split(file.read())
    
        tcnos = list(set(expected_output[0].keys()))
        tcnos.sort()
        self.write_test_result(len (set(tcnos) - set(output[0].keys())) == 0, 
                               "all_tests_performed", 
                               "The following tests appear to not have been run: "+
                               str(sorted(list(set(tcnos) - set(output[0].keys())))))
        self.write_test_result(len (set(output[0].keys())- set(tcnos)) == 0, 
                               "all_tests_have_results", 
                               "The following tests appear to not to be listed in expected outputs: "+
                               str(sorted(list(set(output[0].keys())- set(tcnos)))))
    
        for tc in tcnos:
            if tc not in output[0]:
                self.write_test_result(False,
                                       tcnames[tc],
                                       "Test does not appear to have been run")
                continue
            diff0 = list(difflib.unified_diff(expected_output[0][tc],output[0][tc]))
            diff1 = list(difflib.unified_diff(expected_output[1][tc],output[1][tc]))
            diff2 = list(difflib.unified_diff(expected_output[2][tc],output[2][tc]))
            self.write_diff(tcnames[tc], tc, diff0, diff1, diff2)
            self.num_tc_performed += 1

        self.check_swre_output()


    def check_swre_output(self):
        files = glob.glob("swre_logger.*output.txt")
        if len(files) != 1:
            self.write_test_result(False,
                                   "swre_logger_output",
                                   "Could not find swre_logger_output (files = " + str(files) + ")")
            return
        with open(files[0]) as file:
            data = file.read()
        self.write_test_result(len(data.strip()) == 0,
                               "swre_logger_output",
                               "swre_logger output is not empty:\n" + data.replace("\r",""))
            

def preexec(): # Don't forward signals.
    os.setpgrp()
        
class Runner:
    def __init__(self):
        self.procs = dict()

    def __launchProcess(self, name, cmd, stdoutToFile = True):
        if stdoutToFile:
            output = open(name + ".output.txt","w")
        else:
            output = None
        cf = 0
        if sys.platform == "win32":
            proc = subprocess.Popen(cmd, 
                                    stdout = output, 
                                    stderr = subprocess.STDOUT,
                                    creationflags = subprocess.CREATE_NEW_PROCESS_GROUP)
        else:
            proc = subprocess.Popen(cmd, 
                                    stdout = output, 
                                    stderr = subprocess.STDOUT,
                                    preexec_fn = preexec)

        self.procs[name] = (proc,output)
        return proc

    def initialize(self,parameters):
        if parameters.standalone:
            partners = (0,1,2)
        elif parameters.multinode:
            partners = (0,1)
        else: #slave
            partners = (2,)

        print "** Starting dose_main and partners", str(partners)
        #if we're a slave we log to stdout, not file
        self.dose_main = self.__launchProcess("dose_main", 
                                              parameters.dose_main_cmd, 
                                              stdoutToFile = not parameters.slave)

        if not parameters.slave:
            self.__launchProcess("foreach", parameters.foreach_cmd)

        self.__launchProcess("swre_logger", parameters.swre_logger_cmd)

        for i in partners:
            self.__launchProcess("dose_test_cpp." + str(i), parameters.dose_test_cpp_cmd + (str(i),))
            #self.__launchProcess("dose_test_ada." + str(i), parameters.dose_test_ada_cmd + (str(i),))
            self.__launchProcess("dose_test_dotnet." + str(i), parameters.dose_test_dotnet_cmd + (str(i),))
            if not parameters.no_java:
                self.__launchProcess("dose_test_java." + str(i), parameters.dose_test_java_cmd + (str(i),))

    def __kill(self, name, proc):
        try:
#            print "Terminating", name
            if sys.platform == "win32":
                #can't send CTRL_C_EVENT to processes started with subprocess, unfortunately
                proc.send_signal(signal.CTRL_BREAK_EVENT)
            else:
                proc.terminate()
            for i in range (100):
                if proc.poll() is not None:
#                    print " Terminate successful"
                    return
                time.sleep(0.1)
            if proc.poll() is None:
                print "Killing", name
                proc.kill()
                proc.wait()
        except OSError:
            pass

    def killprocs(self, results):
        self.__kill("dose_main", self.dose_main)

        #Processes have up to 10 seconds to exit from the stop order
        future = time.time() + 10 #ten seconds                
        for name, (proc,output) in self.procs.iteritems():
            while time.time() < future:
                if proc.poll() is not None:
                    break;
                time.sleep(0.1)
            if proc.poll() is None:
                self.__kill(name,proc)
                proc.wait()

            results.write_test_result(proc.returncode == 0,
                                      name + ".returncode",
                                      "Process exited with return code " + str(proc.returncode) + ", expected 0")
            if proc.returncode != 0:
                print "", name, "exited with returncode", proc.returncode
            if output is not None:
                output.close()

    def run_sequencer(self,parameters):
        print "** Running test sequencer with languages set to", parameters.lang0, parameters.lang1, parameters.lang2
        print "---- Output from dose_test_sequencer: ----"
        sequencer = self.__launchProcess("dose_test_sequencer", 
                                         parameters.dose_test_sequencer_cmd, 
                                         stdoutToFile = False)
        sequencer.wait()
        print "------------------------------------------"

    def wait_for_dose_main(self):
        self.dose_main.wait()

class JenkinsInterface:
    def __init__(self, parameters):
        import urllib
        self.server = os.environ.get("JENKINS_URL_OVERRIDE")
        if self.server is None:
            self.server = os.environ.get("JENKINS_URL")
            if self.server is None:
                print "No JENKINS_URL found"
                sys.exit(1)
        print "Using jenkins server",self.server
            
        cliurl = self.server + "/jnlpJars/jenkins-cli.jar"
        #print "will download jenkins-cli.jar using url", cliurl
        self.clijar = os.path.join(parameters.tempdir,"jenkins-cli.jar")
        urllib.urlretrieve(cliurl,self.clijar)
        
    def __run_command(self,cmd,input = None):
        args = list()
        args += ("java","-jar",self.clijar,"-s",self.server)
        
        if type(cmd) is str:
            args.append(cmd)
        else:
            args += cmd

        proc = subprocess.Popen(args, 
                                stdout = subprocess.PIPE, 
                                stderr = subprocess.STDOUT,
                                stdin = None if input is None else subprocess.PIPE)
        res = proc.communicate(input)
        if proc.returncode == 0:
            return res[0]
        else:
            print res[0]
            raise Exception("Failed to run jenkins command", str(args))
        
    def __run_groovy(self,script):
        return self.__run_command(("groovy","="),script)

    def help(self):
        return self.__run_command("groovy")

    def who_am_i(self):
        return self.__run_command("who-am-i")

    def list_jobs(self):
        output = self.__run_groovy("import hudson.model.*\n" + 
                                   "for(item in Hudson.instance.items) {println(item.name)}")
        jobs = output.splitlines()
        return jobs

    def __to_bool(self,output):
        output = output.strip()
        if output not in ("true","false"):
            raise Exception("Unexpected boolean value:" + output)
        return output == "true"


    def is_building(self,job):
        script = """
                 import hudson.model.*
                 item = Hudson.instance.getItem("JOB_NAME")
                 println(item.isBuilding())
                 """
        script = script.replace("JOB_NAME",job)
        output = self.__run_groovy(script)
        return self.__to_bool(output)

    def wait_for_job(self,job,duration=None):
        if duration is None:
            while self.is_building(job):
                time.sleep(1.0)
        else:
            future = time.time() + duration
            while time.time() < future and self.is_building(job):
                time.sleep(1.0)

    def build(self,job):
        self.__run_command(("build",job))
        while not self.is_building(job):
            time.sleep(1.0)

    def cancel_job(self,job):
        script = """
                 import hudson.model.*
                 item = Hudson.instance.getItem("JOB_NAME")
                 executor = item.getLastBuild().getExecutor()
                 if (executor != null)
                 {
                   executor.interrupt()
                 }
                 """
        script = script.replace("JOB_NAME",job)
        self.__run_groovy(script)

    def get_console_output(self,job):
        script = """
                 import hudson.model.*
                 item = Hudson.instance.getItem("JOB_NAME")
                 build = item.getLastBuild()
                 text = build.getLogText()
                 println ("text length " + text.length())
                 //build.getLogText().writeLogTo(0, System.out)
                 println(build.getLog())
                 println ("end log")
                 """
        script = script.replace("JOB_NAME",job)
        print self.__run_groovy(script)

    
    def terminate_dose_main(self,job):
        """Send a SIGTERM to dose main, to let the slave script end nicely"""
        script = """
                 import hudson.model.*
                 import hudson.util.*
                 item = Hudson.instance.getItem("JOB_NAME")
                 executor = item.getLastBuild().getExecutor()
                 if (executor != null)
                 {
                   computer = executor.getOwner()
                   RemotingDiagnostics.executeGroovy("println \\"killall dose_main\\".execute().text", computer.getChannel())
                 }
                 """
        script = script.replace("JOB_NAME",job)
        self.__run_groovy(script)

            
class JenkinsController:
    def __init__(self,parameters):
        self.interface = JenkinsInterface(parameters)
        self.job_name = "multinode-test-slave-1"
        auth = self.interface.who_am_i()
        if auth.find("authenticated") == -1:
            print "Failed to authenticate using ssh keys, please check that keys are set up correctly"
            print auth
            sys.exit(1)

    def start_slave(self):
        print " * Starting slave"
        self.interface.wait_for_job(self.job_name)
        self.interface.build(self.job_name)

    def stop_slave(self):
        try:
            print " * Stopping slave"
            self.interface.terminate_dose_main(self.job_name)
            self.interface.wait_for_job(self.job_name,60)
            self.interface.cancel_job(self.job_name)
            self.interface.wait_for_job(self.job_name)
        except:
            print "Failed to stop slave!"


def main():
    rmdir("dose_test_output")
    mkdir("dose_test_output")
    os.chdir("dose_test_output")

    parameters = Parameters()

    if parameters.autostart_jenkins_slave:
        jenkinsController = JenkinsController(parameters)
        jenkinsController.start_slave()

    UpdateConfig(parameters)
    runner = Runner()
    results = Results()

    completed = False
    try:
        runner.initialize(parameters)
        if parameters.slave:
            runner.wait_for_dose_main()
        else:
            runner.run_sequencer(parameters)
            completed = True
    except:
        print "exception occurred!"
        traceback.print_exc()
    
    runner.killprocs(results)
    if parameters.autostart_jenkins_slave:
        jenkinsController.stop_slave()

    RestoreConfig(parameters)

    if completed:
        results.check_output(parameters)

        if results.num_tc_failed == 0:
            print "== All tests were successful!!"
        else:
            print "!!",results.num_tc_failed, "testcases appear to have failed!"

        if results.num_tc_performed<20:
            print "!! Less than 20 testcases seem to have been performed, this is a sign of trouble!"
            return 1

    return 0


result = main()

sys.exit(result)
