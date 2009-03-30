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

import os, glob, xml.dom.minidom, re

try:
    os.remove("buildlog.txt")
    os.remove("builderr.txt")
except OSError:
    print 
buildlog = open("buildlog.txt", 'w')
builderr = open("builderr.txt", 'w')

SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
SAFIR_SDK = os.environ.get("SAFIR_SDK")

command_file=None
skip_list=None
CLEAN=False

def die(msg):
    import sys
    print "Build script died with message '" + msg + "'"
    sys.exit(-1);

def check_environment():
    if SAFIR_RUNTIME == None or SAFIR_SDK == None:
        die("You need to have both SAFIR_RUNTIME and SAFIR_SDK set");
    #TODO check cmake?! and other needed stuff

def command_output(cmd):
    " Capture a command's standard output. "
    import subprocess
    buildlog.write("Executing '" + str(cmd) + "'")
    process = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    result = process.communicate()
    return (process.returncode,result[0],result[1])

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
        #print "mkdir %s" % repr(newdir)
        if tail:
            os.mkdir(newdir)

def parse_command_line():
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("--command-file", "-f",action="store",type="string",dest="command_file",
                      help="The command to execute")
    parser.add_option("--skip-list",action="store",type="string",dest="skip_list",
                      help="A space-separated list of regular expressions of lines in the command file to skip")
    parser.add_option("--clean", action="store_true",dest="clean",default=False,
                      help="Run 'clean' before building each subsystem.")

    (options,args) = parser.parse_args()

    global skip_list;
    if (options.skip_list == None):
        skip_list = "".split();
    else:
        skip_list = options.skip_list.split();
        
    global CLEAN
    CLEAN = options.clean
    if (options.command_file == None):
        die("You need to specify the command file to use")
        
    if not os.path.isfile(options.command_file):
        die("The specified command file could not be found")
    global command_file
    command_file = open(options.command_file,'r')

def write_errors(what, step, errors):
    if (errors != None and len(errors) != 0):
        builderr.write("------------- ERRORS when " +step +" " + what + " ----------------\n");
        builderr.write(errors)
        builderr.write("\------------ END ERRORS ---------------\n\n");
        builderr.flush()
        print what + " " + step + " warnings/error written to builderr.txt"


def build_make(what, makefile):
    "Build a Makefile project"
    if CLEAN:
        command_output("make -f "+ makefile + " clean");
    output = command_output("make -f "+ makefile);
    buildlog.write(output[1]);
    write_errors(what, "building", output[2])
    if (output[0] != 0):
        die ("Failed to make " + what)
    output = command_output("make -f "+ makefile + " install");
    buildlog.write(output[1]);
    write_errors(what, "installing", output[2])
    if (output[0] != 0):
        die ("Failed to install " + what)

def fix_vcproj_file(filename):
    "Fix bad stuff in project file. Return the name of the file to use"
    "We don't use dom parser since it screws up the post-build steps"
    file = open(filename,"r")
    string = file.read()
    modified = False
    if string.find("WarnAsError=\"true\"") != -1:
        print "  - Project file was modified to turn off WarnAsError"
        string = string.replace("WarnAsError=\"true\"", "WarnAsError=\"false\"");
        modified = True

    if string.find("libboost_crypto") != -1:
        print "  - Project file was modified to remove (unused) dependency to boost_crypto"
        pat = re.compile("libboost_crypto-vc80.*\.lib ");
        string = pat.sub("",string)
        modified = True

    if string.find("libexpatd.lib") != -1:
        print "  - Project file was modified to change dependency libexpatd.lib to libexpat.lib"
        string = string.replace("libexpatd.lib","libexpat.lib")
        modified = True

    if modified:
        new_filename = "modified_" + filename
        file = open(new_filename,"w")
        file.write(string)
        file.close()
        return new_filename
    else:
        return filename

def build_vcproj(what, configurations):
    if CLEAN:
        build_command = "/rebuild"
    else:
        build_command = "/build"

    projs = glob.glob("*.vcproj")
    for proj in projs:
        if proj.startswith("modified_"):
            continue
        
        proj = fix_vcproj_file(proj)
        
        for config in configurations:
            output = command_output(("devenv.com", build_command,  config, proj)) 
            buildlog.write(output[1]);
            write_errors(what, "building", output[2])
            if (output[0] != 0):
                die ("Failed to build " + what + " in config " + config)

def build_csproj(what):
    if CLEAN:
        build_command = "/rebuild"
    else:
        build_command = "/build"

    projs = glob.glob("*.csproj")
    for proj in projs:
        if proj.startswith("modified_"):
            continue
        
        output = command_output(("devenv.com", build_command,  "release", proj)) 
        buildlog.write(output[1]);
        write_errors(what, "building", output[2])
        if (output[0] != 0):
            die ("Failed to build " + what + " in release config")



def build_cmake(what):
    "Build a cmake project"
    output = command_output(("cmake", "-D", "CMAKE_BUILD_TYPE:string=Release", "-G", "NMake Makefiles", "-Wno-dev", "."));
    buildlog.write(output[1]);
    write_errors(what,"configuring",output[2])
    if (output[0] != 0):
        die ("Failed to do cmake . for " + what)
    output = command_output(("nmake", "/NOLOGO"));
    buildlog.write(output[1]);
    write_errors(what,"building",output[2])
    if (output[0] != 0):
        die ("Failed to make " + what)
    output = command_output(("nmake","/NOLOGO", "install"));
    buildlog.write(output[1]);
    write_errors(what,"installing",output[2])
    if (output[0] != 0):
        die ("Failed to install " + what)

    
def build_dir(dir, options):
    import os
    if (not os.path.isdir(dir)):
        print "Skipping " + dir + ", since it does not exist (or is not a directory)"
        return
    olddir = os.getcwd();
    os.chdir(dir)
    try:
#        print "Attempting build and install of " + dir;
        buildlog.write("------------- BUILDING " + dir + " ----------------\n");
        if glob.glob("*.vcproj"):
            build_vcproj(dir, options)
        elif glob.glob("*.csproj"):
            build_csproj(dir)
        elif os.path.isfile("CMakeLists.txt"):
            build_cmake(dir)
        elif os.path.isfile("Makefile.linux"):
            build_make(dir, "Makefile.linux")
        elif os.path.isfile("Makefile"):
            build_make(dir, "Makefile")
        else:
            print "No supported build file found in " + dir + ", skipping."
            buildlog.write("No supported build file found here!!!");
        buildlog.write("\n-------------- BUILD FINISHED ----------------\n\n");
    finally:
        os.chdir(olddir)

def copy_dob_files(dir):
    import os
    import re
    import shutil
    if not os.path.isdir(SAFIR_SDK + "/dots/dots_generated"):
        mkdir(SAFIR_SDK+ "/dots/dots_generated")

    pattern = re.compile("[a-zA-Z0-9\.\-]*\.do[um]$")
    for filename in os.listdir(dir):
        if pattern.match(filename):
            shutil.copy(dir + "/" + filename,SAFIR_SDK + "/dots/dots_generated/")

def copy_file(filename, dest_env, destpath):
    import shutil
    dest = None
    if dest_env == "None":
        dest = destpath
    if dest_env == "SAFIR_SDK":
        dest = SAFIR_SDK + "/" + destpath
    elif dest_env == "SAFIR_RUNTIME":
        dest = SAFIR_RUNTIME + "/" + destpath
    shutil.copy(filename,dest)

def sign_assembly(filename):
    "Sign an assembly using the safirkey.snk key"
    output = command_output(("sn","-R",filename, "build/safirkey.snk"))
    buildlog.write(output[1]);
    write_errors(filename,"signing",output[2])
    if (output[0] != 0):
        die ("Failed to sign " + filename)

def in_skip_list(line):
    "Check the argument against all regexps in the skip-list"
    import re
    for expr in skip_list:
        p=re.compile(expr)
        if p.search(line):
            return True
    return False;

def main():
    check_environment()
    parse_command_line()

    olddir = os.getcwd();

    for line in command_file:
        if line[0] == '#':
            continue

        if in_skip_list(line.strip()):
            print "- Skipping '" + line.strip() + "' matches skip-list"
            continue
            
        split_line = line.split()
        if len(split_line) == 0:
            continue


        if split_line[0] == "build_dir":
            print "Building " + split_line[1]
            if len(split_line) > 2:
                build_dir(split_line[1], split_line[2:len(split_line)])
            else:
                build_dir(split_line[1],None)

        if split_line[0] == "copy_file":
            print "Copying an auxillary file ("+ split_line[1] +")"
            copy_file(split_line[1], split_line[2], split_line[3])

        if split_line[0] == "copy_dob_files":
            print "Copying dob file from " + split_line[1]
            copy_dob_files(split_line[1])
            
        if split_line[0] == "build_dots_generated":
            print "Building dots_generated"
            build_dir(SAFIR_SDK + "/dots/dots_generated", ("debug", "release"))
        
        if split_line[0] == "sign_assembly":
            print "Signing assembly " + split_line[1]
            sign_assembly(SAFIR_RUNTIME + "/bin/" + split_line[1])

    os.chdir(olddir)
    
    builderr.close()
    if os.stat("builderr.txt").st_size == 0:
        print "Build completely successful!"
        os.remove("builderr.txt")
    else:
        print "Build was successful, but there were some warnings (or errors?) while building."
        print "Look in buildlog.txt and builderr.txt for clues"

if __name__ == "__main__":
    import sys
    sys.exit(main())
