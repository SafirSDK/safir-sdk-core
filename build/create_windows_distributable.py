#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2008-2009 (http://www.safirsdk.com)
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

import os, shutil, stat, subprocess

PATH = os.environ.get("PATH").split(";")
SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
SAFIR_SDK = os.environ.get("SAFIR_SDK")

LIB_DESTINATION = os.path.join(SAFIR_SDK, "lib")
DLL_DESTINATION = os.path.join(SAFIR_RUNTIME, "bin")
HEADER_DESTINATION = os.path.join(SAFIR_SDK, "include")
DOCS_DESTINATION = os.path.join(SAFIR_SDK, "docs")


def copy_file(name,destination):
    if not os.path.isfile(name):
        print ("ERROR! " + name + " is not a file!")
    if not os.path.isdir(destination):
        print ("ERROR! " + destination + " is not a directory!")
    try:
        shutil.copy2(name, destination)
        os.chmod(os.path.join(destination,os.path.split(name)[-1]),stat.S_IWRITE|stat.S_IREAD)
    except:
        import filecmp
        if not filecmp.cmp(name,os.path.join(destination,os.path.split(name)[-1])):
            print ("Caught exception while copying " + name + " to " + destination + "/. Maybe the destination file or directory is read-only?")

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


def copy_tree(srcdir, dstdir, include_patterns=None, exclude_regex=None):
    """Excludes overrule includes"""
    import fnmatch, os
    # dstdir must exist first
    srcnames = os.listdir(srcdir)
    for name in srcnames:
        srcfname = os.path.join(srcdir, name)
        dstfname = os.path.join(dstdir, name)
        if os.path.isdir(srcfname):
            copy_tree(srcfname, dstfname, include_patterns,exclude_regex)
        else:
            match = False
            if include_patterns is None:
                match = True
            else:
                for pattern in include_patterns:
                    if fnmatch.fnmatch(name,pattern):
                        match = True
                        break
            if exclude_regex is not None:
                if exclude_regex.match(srcfname) is not None:
                    match = False
            
            if match == True:
                mkdir(dstdir)
                copy_file(srcfname, dstdir)


def find_dll(names):
    for name in names:
        for path in PATH:
            if not SAFIR_RUNTIME in path:
                fn = os.path.join(path,name)
                if os.path.isfile(fn):
                    return path
    print ("could not find ", names)

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
       print ("could not find "+ name)
    return False

def copy_lib(name):
    for path in PATH:
        if not SAFIR_RUNTIME in path:
            fn = os.path.join(path,name)
            if os.path.isfile(fn):
                copy_file(fn, LIB_DESTINATION)
                return
            fn = os.path.join(path,"../lib", name)
            if os.path.isfile(fn):
                copy_file(fn, LIB_DESTINATION)
                return
    print ("could not find "+ name)

def copy_libs_from_dir(dir):
    dirlist = os.listdir(dir)
    for file in dirlist:
        if os.path.splitext(file)[1] == ".lib":
            copy_file(os.path.join(dir,file),LIB_DESTINATION)

def copy_dlls_from_dir(dir):
    dirlist = os.listdir(dir)
    for file in dirlist:
        if os.path.splitext(file)[1] == ".dll":
            copy_file(os.path.join(dir,file),DLL_DESTINATION)

def copy_header_dir(dir, patterns=None):
    if not os.path.isdir(dir):
        print ("ERROR! " + dir + " is not a directory");
        return
    dst = os.path.join(HEADER_DESTINATION,os.path.split(dir)[-1])
    copy_tree(dir,dst, include_patterns=patterns)


def copy_headers(dir,files):
    if not os.path.isdir(dir):
        print ("ERROR! " + dir + " is not a directory");
        return
    for file in files:
        copy_file(os.path.join(dir,file),HEADER_DESTINATION)

def copy_docs_dir(dir, targetname, exclude_regex=None):
    import re
    if not os.path.isdir(dir):
        print ("ERROR! " + dir + " is not a directory");
        return
    dst = os.path.join(DOCS_DESTINATION,targetname)
    copy_tree(dir, dst, exclude_regex=re.compile(exclude_regex))

def main():
    #Copy Boost 
    boost_dll_dir = find_dll(("boost_filesystem-vc143-mt-x32-1_85.dll",
                              "boost_filesystem-vc143-mt-x64-1_85.dll"))
    boost_dir = os.path.join(boost_dll_dir,"..")
        
    copy_libs_from_dir(boost_dll_dir)
    copy_dlls_from_dir(boost_dll_dir)
    copy_header_dir(os.path.join(boost_dir, "boost"))

    #Copy ACE stuff
    copy_dll("ACE.dll")
    copy_dll("ACEd.dll")
    copy_lib("ACE.lib")
    copy_lib("ACEd.lib")
    copy_header_dir(os.path.join(os.environ.get("ACE_ROOT"),"ace"),("*.cpp","*.h","*.inl"))

    #Copy the Qt runtime dll
    copy_dll("Qt5Core.dll")
    copy_dll("Qt5Gui.dll")
    copy_dll("Qt5Widgets.dll")
    qt_bin_dir = find_dll(("Qt5Core.dll",))
    print ("qt_bin_dir:", qt_bin_dir)
    copy_tree(os.path.join(qt_bin_dir,"archdatadir"), DLL_DESTINATION, include_patterns=("qwindows.dll","qwindowsvistastyle.dll"))
    with open(os.path.join(DLL_DESTINATION,"qt.conf"),"w") as f:
        f.write("[Paths]\nprefix=.")

    #Copy the expat stuff
    copy_dll("libexpat.dll")
    copy_lib("libexpat.lib")
    expat_header_dir = os.path.join(find_dll(("libexpat.dll",)),"..","include")
    copy_headers(expat_header_dir,("expat.h","expat_external.h"))

    #Copy the GNAT runtime.
    copy_dll("libgcc_s_dw2-1.dll", alternatives=("libgcc_s_seh-1.dll",))
    copy_dll("libgnat-24.1.dll", alternatives=("libgnat-2021.dll",))
    copy_dll("libgnarl-24.1.dll", alternatives=("libgnarl-2021.dll",))

    #copy example apps to good spot
    copy_docs_dir("examples/vehicleapp/vehicleapp_core.ss", "examples/vehicleapp", r".*\.svn.*")
    copy_docs_dir("examples/vehicledb/vehicledb_core.ss", "examples/vehicledb", r".*\.svn.*")
    copy_docs_dir("examples/vehiclemmi/vehiclemmi_core.ss", "examples/vehiclemmi", r".*\.svn.*")

    #do a make clean or dobmake clean in dots_generated
    dobmake_path = os.path.join(SAFIR_RUNTIME,"bin","dobmake.py")
    retcode = subprocess.call(["python",dobmake_path, "-b", "--clean"])
    if retcode != 0:
        print ("Failed to do 'dobmake.py -b --clean'")

if __name__ == "__main__":
    import sys
    sys.exit(main())
