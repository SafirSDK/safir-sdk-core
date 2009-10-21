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

import os, shutil, stat

PATH = os.environ.get("PATH").split(";")
SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
SAFIR_SDK = os.environ.get("SAFIR_SDK")

LIB_DESTINATION = os.path.join(SAFIR_SDK, "lib")
DLL_DESTINATION = os.path.join(SAFIR_RUNTIME, "bin")
HEADER_DESTINATION = os.path.join(SAFIR_SDK, "include")
DOCS_DESTINATION = os.path.join(SAFIR_SDK, "docs")


def copy_file(name,destination):
    if not os.path.isfile(name):
        print "ERROR! " + name + " is not a file!"
    if not os.path.isdir(destination):
        print "ERROR! " + destination + " is not a directory!"
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
    print "could not find ", names

def copy_dll(name, alternatives = None):
    for path in PATH:
        if not SAFIR_RUNTIME in path:
            fn = os.path.join(path,name)
            if os.path.isfile(fn):
                copy_file(fn, DLL_DESTINATION)
                return True
    if alternatives is not None:
        for alt in alternatives:
            res = copy_dll(alt)
            if res:
                return True
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

def clean_dir(dir):
    olddir = os.getcwd()
    try:
        import subprocess
        os.chdir(dir)
        retcode = subprocess.call([".\make", "clean"])
        if retcode != 0:
            print "Failed to do 'make clean' in " + dir
    finally:
        os.chdir(olddir)

def main():
    #Copy Boost stuff
    boost_dir = os.environ.get("BOOST_DIR")
    if boost_dir is None:
        boost_dir = os.environ.get("BOOST_ROOT")
    if boost_dir is None:
        boost_dir = find_dll(("boost_date_time-vc80-mt-1_38.dll",
                              "boost_date_time-vc80-mt-1_39.dll",
                              "boost_date_time-vc80-mt-1_40.dll",
                              "boost_date_time-vc80-mt-1_41.dll",
                              "boost_date_time-vc80-mt-1_42.dll",
                              "boost_date_time-vc90-mt-1_38.dll",
                              "boost_date_time-vc90-mt-1_39.dll",
                              "boost_date_time-vc90-mt-1_40.dll",
                              "boost_date_time-vc90-mt-1_41.dll",
                              "boost_date_time-vc90-mt-1_42.dll"))
        boost_dir = os.path.join(boost_dir,"..")
    copy_libs_from_dir(os.path.join(boost_dir, "lib"))
    copy_dlls_from_dir(os.path.join(boost_dir, "lib"))
    copy_header_dir(os.path.join(boost_dir, "boost"))

    #Copy ACE stuff
    copy_dll("ACE.dll")
    copy_dll("ACEd.dll")
    copy_lib("ACE.lib")
    copy_lib("ACEd.lib")
    copy_header_dir(os.path.join(os.environ.get("ACE_ROOT"),"ace"),("*.cpp","*.h","*.inl"))

    #Copy the Qt runtime dll
    copy_dll("QtCore4.dll")
    copy_dll("QtGui4.dll")

    #Copy the expat stuff
    copy_dll("libexpat.dll")
    copy_lib("libexpat.lib")
    expat_header_dir = os.path.join(find_dll(("libexpat.dll",)),"..","Source","lib")
    copy_headers(expat_header_dir,("expat.h","expat_external.h"))

    #Copy the GNAT runtime, xmlada and templates_parser.
    copy_dll("libgcc_s.dll")
    copy_dll("libgnat-6.2.dll",("libgnat-2009.dll",))
    copy_dll("libgnarl-6.2.dll", ("libgnarl-2009.dll",))
    copy_dll("libxmlada_unicode.dll")
    copy_dll("libxmlada_input_sources.dll")
    copy_dll("libxmlada_sax.dll")
    copy_dll("libtemplates_parser.dll")
    copy_dll("mingwm10.dll")

    #copy example apps to good spot
    copy_docs_dir("examples/vehicleapp/vehicleapp_core.ss", "examples/vehicleapp", ".*\.svn.*")
    copy_docs_dir("examples/vehicledb/vehicledb_core.ss", "examples/vehicledb", ".*\.svn.*")
    copy_docs_dir("examples/vehiclemmi/vehiclemmi_core.ss", "examples/vehiclemmi", ".*\.svn.*")

    #do a make clean or dobmake clean in dots_generated
    dobmake_path = os.path.join(SAFIR_RUNTIME,"bin","dobmake.py")
    if os.path.exists(dobmake_path):
        import subprocess
        retcode = subprocess.call(["python",dobmake_path, "-b", "--clean"])
        if retcode != 0:
            print "Failed to do 'dobmake.py -b --clean'"
    else:
        clean_dir(os.path.join(SAFIR_SDK,"dots","dots_generated"))
        return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())
