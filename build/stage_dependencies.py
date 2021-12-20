#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2008-2014 (http://safirsdkcore.com)
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
import os, shutil, stat, subprocess, sys, re, filecmp
from optparse import OptionParser


class StagingError(Exception):
    pass


def copy_file(name, destDir):
    if not os.path.isfile(name):
        raise StagingError(name + " is not a file!")
    if not os.path.isdir(destDir):
        raise StagingError(destDir + " is not a directory!")
    try:
        #the way this copy is meant to work is to copy the file contents
        #and copy the create and modification dates but *NOT* any other
        #metadata, such as permissions or ACLs.
        destpath = os.path.join(destDir, os.path.split(name)[-1])  #full path
        shutil.copy(name, destpath)
        stat = os.stat(name)
        os.utime(destpath, (stat.st_atime, stat.st_mtime))  #copy create and modify times correctly
    except:
        if not filecmp.cmp(name, os.path.join(destDir, os.path.split(name)[-1])):
            logError("Caught exception while copying " + name + " to " + destDir +
                     "/. Maybe the destination file or directory is read-only?")


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


class __WindowsStager():
    def __init__(self, logger, stage):
        self.logger = logger

        if os.path.isdir(os.path.join(stage,"Runtime", "Program Files (x86)")):
            program_files = "Program Files (x86)"
        elif os.path.isdir(os.path.join(stage,"Runtime", "Program Files")):
            program_files = "Program Files"
        else:
            raise StagingError("Could not work out whether to stage to x86 program files")

        self.LIB_DESTINATION = os.path.join(stage, "Development", program_files, "safir-sdk-core", "lib")
        self.DLL_DESTINATION = os.path.join(stage, "Runtime", program_files, "safir-sdk-core", "bin")
        self.HEADER_DESTINATION = os.path.join(stage, "Development", program_files, "safir-sdk-core", "include")

    def __copy_dll(self, name):
        for path in os.environ.get("PATH").split(os.pathsep):
            fn = os.path.join(path, name)
            if os.path.isfile(fn):
                copy_file(fn, self.DLL_DESTINATION)
                return
        raise StagingError("Could not find " + name)

    def __copy_exe(self, name):
        self.__copy_dll(name)

    def __copy_boost(self):
        self.logger.log("Copying boost stuff", "detail")
        headers_copied = False
        for dir in ["Debug", "RelWithDebInfo", "MinSizeRel", "Release"]:
            src_dir = os.path.abspath(os.path.join(".",dir))
            if not os.path.isdir(src_dir):
                continue
            lib_dir = os.path.join(src_dir, "lib")
            dll_dir = os.path.join(src_dir, "bin")
            include_dir = os.path.join(src_dir, "include")

            if not os.path.isdir(lib_dir) or not os.path.isdir(dll_dir) or not os.path.isdir(include_dir):
                raise StagingError("Failed to find one of the boost dirs")
        
            self.__copy_boost_libs(lib_dir)
            self.__copy_boost_dlls(dll_dir)
            if not headers_copied:
                self.__copy_boost_headers(include_dir)
                headers_copied = True

    def __copy_ninja(self):
        self.logger.log("Checking if ninja.exe is a chocolatey shim")
        result = subprocess.run(("ninja.exe", "--shimgen-help"), encoding="utf-8", stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        if re.search("This is a shim", result.stdout) is not None:
            match = re.search("Target: '(.*)'$", result.stdout, re.MULTILINE)
            if match is None:
                raise StagingError("Failed to parse ninja shim info!")
            self.logger.log(f" - Copying shimmed ninja.exe from {match.group(1)}")
            copy_file(match.group(1), self.DLL_DESTINATION)
        else:
            self.logger.log(" - Copying non-shimmed ninja.exe")
            self.__copy_exe("ninja.exe")

    def run(self):
        self.__copy_boost()
        self.__copy_ninja()

    def __copy_header_dir(self, dir):
        if not os.path.isdir(dir):
            raise StagingError(dir + " is not a directory")

        dst = os.path.join(self.HEADER_DESTINATION, os.path.split(dir)[-1])
        copy_tree(dir, dst)

    def __copy_boost_headers(self, dir):
        path_name_filter = re.compile(r"boost-[0-9_]*")
        dirlist = os.listdir(dir)
        for path in dirlist:
            match = path_name_filter.match(path)
            if match is not None and os.path.isdir(os.path.join(dir, path, "boost")):
                self.logger.log(f" -  {path}/boost")
                self.__copy_header_dir(os.path.join(dir, path, "boost"))


    def __copy_boost_libs(self, dir):
        file_name_filter = re.compile(r"boost_(.*)-vc.*-mt-.*\.lib")
        dirlist = os.listdir(dir)
        for file in dirlist:
            match = file_name_filter.match(file)
            if match is not None:
                self.logger.log(f" -  {file}")
                copy_file(os.path.join(dir, file), self.LIB_DESTINATION)

    def __copy_boost_dlls(self, dir):
        file_name_filter = re.compile(r"boost_(.*)-vc.*-mt-.*\.dll")
        dirlist = os.listdir(dir)
        for file in dirlist:
            match = file_name_filter.match(file)
            if match is not None:
                self.logger.log(f" -  {file}")
                copy_file(os.path.join(dir, file), self.DLL_DESTINATION)

def stage_dependencies(logger, stage):
    """
    Throws StagingError if something goes wrong.
    """
    logger.log("Copying Safir SDK Core binary dependencies to staging area", "header")
    if sys.platform == "win32":
        stager = __WindowsStager(logger, stage)
        stager.run()

    else:
        logger.log("Nothing to do...", "detail")
