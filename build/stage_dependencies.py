#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2008-2014 (http://safir.sourceforge.net)
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
import os, shutil, stat, subprocess, sys, re, filecmp
from optparse import OptionParser

class StagingError(Exception):
    pass

def copy_file(name,destDir):
    if not os.path.isfile(name):
        raise StagingError(name + " is not a file!")
    if not os.path.isdir(destDir):
        raise StagingError(destDir + " is not a directory!")
    try:
        #the way this copy is meant to work is to copy the file contents
        #and copy the create and modification dates but *NOT* any other
        #metadata, such as permissions or ACLs.
        destpath = os.path.join(destDir,os.path.split(name)[-1]) #full path
        shutil.copy(name, destpath)
        stat = os.stat(name)
        os.utime(destpath, (stat.st_atime, stat.st_mtime)) #copy create and modify times correctly
    except:
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
"""

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



def copy_qt_dlls(dir):
    #""Try to copy a bunch of qt files. This is a mismash of qt4 and qt5 stuff,
    but since QTDIR should only contain one qt version, this should work.
    The stuff that is marked as for qt4 are the ones that are needed for Qt4,
    all others are for qt5.""#

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
            log (" " + file)
            copy_file(path, DLL_DESTINATION)

    qwindows = os.path.join(dir, "plugins", "platforms", "qwindows.dll")
    if os.path.isfile(qwindows):
        log (" qwindows.dll")
        platforms = os.path.join(DLL_DESTINATION, "platforms")
        mkdir(platforms)
        copy_file(qwindows, platforms)
        



def copy_headers(dir,files):
    if not os.path.isdir(dir):
        logError("ERROR! " + dir + " is not a directory")
        return
    for file in files:
        copy_file(os.path.join(dir,file),HEADER_DESTINATION)
"""
class __WindowsStager(object):
    def __init__(self, logger, stage):
        self.logger = logger
        
        self.LIB_DESTINATION = os.path.join(stage, "Development", "Program Files", "safir_sdk_core", "lib")
        self.DLL_DESTINATION = os.path.join(stage, "Runtime", "Program Files", "safir_sdk_core", "bin")
        self.HEADER_DESTINATION = os.path.join(stage, "Development", "Program Files", "safir_sdk_core", "include")

    def __copy_boost(self):
        self.logger.log("Copying boost stuff", "detail")
        boost_dir = os.environ.get("BOOST_ROOT")
        if boost_dir is None: 
            raise StagingError("Failed to find boost installation")
        
        # find lib dir    
        boost_lib_dir = os.path.join(boost_dir, "lib");
        if not os.path.isdir(boost_lib_dir):
            raise StagingError("Failed to find boost lib dir")

        boost_libraries = ("chrono", #we don't need this ourselves, but users may?
                           "date_time",
                           "filesystem",
                           "program_options",
                           "regex",
                           "system",
                           "thread",
                           "timer")

        self.__copy_boost_libs(boost_lib_dir, boost_libraries)
        self.__copy_boost_dlls(boost_lib_dir, boost_libraries)
        self.__copy_header_dir(os.path.join(boost_dir, "boost"))
        
    def run(self):
        self.__copy_boost()
        
    """
        ############
        qt_dir = os.environ.get("QTDIR")
        if qt_dir is None:
            logError("QTDIR is not set! Will not copy qt stuff")
        else:
            log("Copying the Qt runtime dlls from " + qt_dir)
            copy_qt_dlls(qt_dir)

        ############
        log("Copying Ada stuff - GNAT runtime")
        copy_dll("libgnat-2013.dll", Log_Error = False)
        copy_dll("libgnarl-2013.dll", Log_Error = False)
        copy_dll("libgcc_s_dw2-1.dll", Log_Error = False)

        ###########
        log("Copying jom.exe")
        copy_exe("jom.exe")
    """
    def __copy_header_dir(self, dir):
        if not os.path.isdir(dir):
            raise StagingError(dir + " is not a directory")

        dst = os.path.join(self.HEADER_DESTINATION,os.path.split(dir)[-1])
        copy_tree(dir, dst)

    def __copy_boost_libs(self, dir, libraries):
        file_name_filter = re.compile(r"boost_(.*)-vc.*-mt-.*\.lib")
        dirlist = os.listdir(dir)
        for file in dirlist:
            match = file_name_filter.match(file)
            if match is not None and match.group(1) in libraries:
                copy_file(os.path.join(dir,file), self.LIB_DESTINATION)
               

    def __copy_boost_dlls(self, dir, libraries):
        file_name_filter = re.compile(r"boost_(.*)-vc.*-mt-.*\.dll")
        dirlist = os.listdir(dir)
        for file in dirlist:
            match = file_name_filter.match(file)
            if match is not None and match.group(1) in libraries:
                copy_file(os.path.join(dir,file), self.DLL_DESTINATION)     
                
def stage_dependencies(logger, stage):
    """
    Throws StagingError if something goes wrong.
    """
    logger.log("Copying Safir SDK Core binary dependencies to staging area", "header") 
    if sys.platform == "win32":
        stager = __WindowsStager(logger,stage)
        stager.run()
    
    else:
        logger.log("Nothing to do...", "detail")
    

