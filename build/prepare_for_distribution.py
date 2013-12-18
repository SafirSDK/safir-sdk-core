#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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
import os, shutil, stat, subprocess, sys, re
from optparse import OptionParser

errors = 0
def logError(data):
    global errors
    errors = errors + 1
    log(data)

def log(data):
    print(data)
    sys.stdout.flush()

parser = OptionParser()
parser.add_option("--jenkins", action="store_true",dest="jenkins",default=False,
                  help="Set up and use environment variables for a Jenkins automated build.")

(options,args) = parser.parse_args()

if options.jenkins:
    WORKSPACE = os.environ.get("WORKSPACE")
    if not WORKSPACE:
        logError("Environment variable WORKSPACE is not set, is this really a Jenkins build?!")
        sys.exit(1)

    os.environ["SAFIR_RUNTIME"] = os.path.join(WORKSPACE,"safir","runtime")
    os.environ["SAFIR_SDK"] = os.path.join(WORKSPACE,"safir","sdk")
    #os.environ["PATH"] = os.environ.get("PATH") + os.pathsep + os.path.join(os.environ.get("SAFIR_RUNTIME"),"bin")

PATH = os.environ.get("PATH").split(os.pathsep)
SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
SAFIR_SDK = os.environ.get("SAFIR_SDK")

SDK_LIB_DESTINATION = os.path.join(SAFIR_SDK, "lib")
DLL_DESTINATION = os.path.join(SAFIR_RUNTIME, "bin")
HEADER_DESTINATION = os.path.join(SAFIR_SDK, "include")
DOCS_DESTINATION = os.path.join(SAFIR_SDK, "docs")

def rmdir(directory):
    if os.path.exists(directory):
        try:
            shutil.rmtree(directory)
        except OSError:
            log("Failed to remove directory, will retry")
            time.sleep(0.2)
            shutil.rmtree(directory)

def try_remove(path):
    if os.path.isfile(path):
        os.remove(path)
            
def copy_file(name,destDir):
    if not os.path.isfile(name):
        logError("ERROR! " + name + " is not a file!")
    if not os.path.isdir(destDir):
        logError("ERROR! " + destDir + " is not a directory!")
    try:
        #the way this copy is meant to work is to copy the file contents
        #and copy the create and modification dates but *NOT* any other
        #metadata, such as permissions or ACLs.
        destpath = os.path.join(destDir,os.path.split(name)[-1]) #full path
        shutil.copy(name, destpath)
        stat = os.stat(name)
        os.utime(destpath, (stat.st_atime, stat.st_mtime)) #copy create and modify times correctly
    except:
        import filecmp
        if not filecmp.cmp(name,os.path.join(destDir,os.path.split(name)[-1])):
            logError("Caught exception while copying " + name + " to " + destDir + "/. Maybe the destination file or directory is read-only?")

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


def copy_tree(srcdir, dstdir):
    import fnmatch, os
    # dstdir must exist first
    srcnames = os.listdir(srcdir)
    for name in srcnames:
        srcfname = os.path.join(srcdir, name)
        dstfname = os.path.join(dstdir, name)
        if os.path.isdir(srcfname):
            copy_tree(srcfname, dstfname)
        else:
            mkdir(dstdir)
            copy_file(srcfname, dstdir)


def find_dll(names):
    for name in names:
        for path in PATH:
            if not SAFIR_RUNTIME in path:
                fn = os.path.join(path,name)
                if os.path.isfile(fn):
                    return path
    log("could not find " + str(names))

def copy_dll(name, alternatives = None, Log_Error = True):
    for path in PATH:
        if not SAFIR_RUNTIME in path:
            fn = os.path.join(path,name)
            if os.path.isfile(fn):
                copy_file(fn, DLL_DESTINATION)
                return True
    if alternatives is not None:
        for alt in alternatives:
            res = copy_dll(alt, None, False)
            if res:
                return True
    if Log_Error:
       logError("could not find "+ name)
    return False

def copy_exe(name, alternatives = None, Log_Error = True):
    copy_dll(name,alternatives,Log_Error)

def copy_lib(name, alternatives = None, Log_Error = True):
    for path in PATH:
        if not SAFIR_RUNTIME in path:
            fn = os.path.join(path,name)
            if os.path.isfile(fn):
                copy_file(fn, SDK_LIB_DESTINATION)
                return True
            fn = os.path.join(path,"../lib", name)
            if os.path.isfile(fn):
                copy_file(fn, SDK_LIB_DESTINATION)
                return True
    if alternatives is not None:
        for alt in alternatives:
            res = copy_lib(alt, None, False)
            if res:
                return True
    if Log_Error:
       logError("could not find "+ name)
    return False

def copy_boost_libs(dir, libraries):
    file_name_filter = re.compile(r"boost_(.*)-vc.*-mt-.*\.lib")
    dirlist = os.listdir(dir)
    for file in dirlist:
        match = file_name_filter.match(file)
        if match is not None and match.group(1) in libraries:
            copy_file(os.path.join(dir,file), SDK_LIB_DESTINATION)

def copy_boost_dlls(dir, libraries):
    file_name_filter = re.compile(r"boost_(.*)-vc.*-mt-.*\.dll")
    dirlist = os.listdir(dir)
    for file in dirlist:
        match = file_name_filter.match(file)
        if match is not None and match.group(1) in libraries:
            copy_file(os.path.join(dir,file), DLL_DESTINATION)

def copy_qt_dlls(dir):
    """Try to copy a bunch of qt files. This is a mismash of qt4 and qt5 stuff,
    but since QTDIR should only contain one qt version, this should work.
    The stuff that is marked as for qt4 are the ones that are needed for Qt4,
    all others are for qt5."""

    names = ("Qt5Core.dll",
             "Qt5Widgets.dll",
             "Qt5Gui.dll",
             "LibGLESv2.dll",
             "LibEGL.dll",
             "icudt51.dll",
             "icuin51.dll",
             "icuuc51.dll",
             "QtCore4.dll",  #for qt4
             "QtGui4.dll")  #for qt4

    for file in names:
        path = os.path.join(dir, "bin", file)
        if os.path.isfile(path):
            copy_file(path, DLL_DESTINATION)

    qwindows = os.path.join(dir, "bin", "plugins", "platforms", "qwindows.dll")
    if os.path.isfile(qwindows):
        copy_file(path, os.path.join(DLL_DESTINATION, "platforms"))

def copy_header_dir(dir):
    if not os.path.isdir(dir):
        logError("ERROR! " + dir + " is not a directory")
        return
    dst = os.path.join(HEADER_DESTINATION,os.path.split(dir)[-1])
    copy_tree(dir, dst)


def copy_headers(dir,files):
    if not os.path.isdir(dir):
        logError("ERROR! " + dir + " is not a directory")
        return
    for file in files:
        copy_file(os.path.join(dir,file),HEADER_DESTINATION)

def windows():
    """Copy lots of our dependencies into the sdk and runtime directories,
    to make use of the SDK binaries *a lot* simpler"""
    

    ##############
    log("Copying boost stuff")
    boost_dir = os.environ.get("BOOST_ROOT")
    if boost_dir is None:
        boost_dir = find_dll(("boost_date_time-vc100-mt-1_41.dll",
                              "boost_date_time-vc100-mt-1_48.dll",
                              "boost_date_time-vc100-mt-1_50.dll",
                              "boost_date_time-vc100-mt-1_51.dll",
                              "boost_date_time-vc100-mt-1_54.dll"))
        boost_dir = os.path.join(boost_dir,"..")

    # find lib dir    
    boost_lib_dir = os.path.join(boost_dir, "lib");
    if not os.path.isdir(boost_lib_dir):
        boost_lib_dir = os.path.join(boost_dir, "stage", "lib");

    boost_libraries = ("chrono",
                       "date_time",
                       "filesystem",
                       "iostreams",
                       "program_options",
                       "random",
                       "regex",
                       "system",
                       "thread")

    copy_boost_libs(boost_lib_dir, boost_libraries)
    copy_boost_dlls(boost_lib_dir, boost_libraries)
    copy_header_dir(os.path.join(boost_dir, "boost"))

    ############
    qt_dir = os.environ.get("QTDIR")
    if qt_dir is None:
        logError("QTDIR is not set! Will not copy qt stuff")
    else:
        log("Copying the Qt runtime dlls from " + qt_dir)
        copy_qt_dlls(qt_dir)

    ############
    log("Copying expat stuff")
    copy_dll("expat.dll",("libexpat.dll",))
    copy_lib("expat.lib",("libexpat.lib",))
    expat_dir=os.path.join(find_dll(("expat.dll","libexpat.dll")),"..")
    expat_header_dir_alt1 = os.path.join(expat_dir,"Source","lib")
    expat_header_dir_alt2 = os.path.join(expat_dir,"include")
    if os.path.exists(expat_header_dir_alt1):
        copy_headers(expat_header_dir_alt1,("expat.h","expat_external.h"))
    elif os.path.exists(expat_header_dir_alt2):
        copy_headers(expat_header_dir_alt2,("expat.h","expat_external.h"))
    else:
        logError("Failed to find expat headers!")
    

    log("Copying Ada stuff - GNAT runtime")
    copy_dll("libgnat-2013.dll", Log_Error = False)
    copy_dll("libgnarl-2013.dll", Log_Error = False)
    copy_dll("libgcc_s_dw2-1.dll", Log_Error = False)

    ###########
    log("Copying jom.exe")
    copy_exe("jom.exe")

def linux():
    pass

def common():
    log("Running dobmake clean: ------------")
    #do a dobmake clean in dots_generated
    dobmake_path = os.path.join(SAFIR_RUNTIME,"bin","dobmake.py")
    retcode = subprocess.call(["python",dobmake_path, "-b", "--clean"])
    if retcode != 0:
        logError("Failed to do 'dobmake.py -b --clean'")
    log("-----------------------------------")
    log("Removing some dobmake state")

    #this file does not exist on linux, probably...
    try_remove(os.path.join(SAFIR_SDK,"dots", "dots_generated", "dobmake.ini"))
    
    os.remove(os.path.join(SAFIR_SDK,"dots", "dots_generated", "installed_files.txt"))
    log("Removing logs from tests during build")
    rmdir(os.path.join(SAFIR_RUNTIME,"log"))

def main():
    log("Preparing Safir SDK Core binary for distribution") 
    if sys.platform.startswith("linux"):
        linux()
    else:
        windows()

    common()
    if errors != 0:
        log ("There were " + str(errors) + " errors.")
    return errors

if __name__ == "__main__":
    import sys
    sys.exit(main())
