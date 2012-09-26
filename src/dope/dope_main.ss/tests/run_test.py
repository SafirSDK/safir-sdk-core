#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2012 (http://www.safirsdk.com)
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

import subprocess, os, time, sys, shutil, glob

sys.stdout = os.fdopen(sys.stdout.fileno(), 'w', 0)

sys.path.append("testutil")
from testenv import TestEnv, TestEnvStopper

def remove(filename):
    for i in range (10):
        try:
            os.remove(filename)
            break
        except:
            time.sleep(0.1)

def rmdir(directory):
    if os.path.exists(directory):
        try:
            shutil.rmtree(directory)
        except OSError:
            print "Failed to remove directory, will retry"
            time.sleep(0.2)
            shutil.rmtree(directory)


if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")

file_storage_path = os.path.join(SAFIR_RUNTIME,"data/binary/dope");

owner_path = os.path.join(exe_path,"entity_owner")

rmdir(file_storage_path)

print "Set a bunch of entities"
env = TestEnv()
with TestEnvStopper(env):
    env.launchProcess("entity_owner", (owner_path,"set")).wait()
    while(len(glob.glob(os.path.join(file_storage_path,"DopeTest.*.bin"))) != 110):
        time.sleep(0.1)

#if not env.ReturnCodesOk():
#    print "Some process exited with an unexpected value"
#    sys.exit(1)

print "See if dope loads them at startup"
env = TestEnv()
with TestEnvStopper(env):
    env.launchProcess("entity_owner", (owner_path,"accept")).wait()

#if not env.ReturnCodesOk():
#    print "Some process exited with an unexpected value"
#    sys.exit(1)

output = env.Output("entity_owner")
if output.count("OnInjectedNewEntity") != 110:
    print "could not find the right number of 'OnInjectedNewEntity' in output"
    sys.exit(1)

if output.count("<name>DopeTest.SmallEntity</name>") != 100:
    print "could not find the right number of 'DopeTest.SmallEntity' in output"
    sys.exit(1)

if output.count("<name>DopeTest.BigEntity</name>") != 10:
    print "could not find the right number of 'DopeTest.BigEntity' in output"
    sys.exit(1)

if len(glob.glob(os.path.join(file_storage_path,"DopeTest.*.xml"))) != 0:
    print "Unexpected xml files found!"
    sys.exit(1)

print "Convert binaries to xml"

#files are automatically chosen by parameter setting
subprocess.call(os.path.join(SAFIR_RUNTIME,"bin","dope_bin2xml"))

if len(glob.glob(os.path.join(file_storage_path,"DopeTest.*.xml"))) != 110:
    print "dope_bin2xml appears to have failed"
    sys.exit(1)

#remove bin files to check that dope can load xml
for f in glob.glob(os.path.join(file_storage_path,"DopeTest.*.bin")):
    remove(f)

print "Check that dope can load xml"

env = TestEnv()
with TestEnvStopper(env):
    env.launchProcess("entity_owner", (owner_path,"accept")).wait()

#if not env.ReturnCodesOk():
#    print "Some process exited with an unexpected value"
#   sys.exit(1)

output = env.Output("entity_owner")
if output.count("OnInjectedNewEntity") != 110:
    print "could not find the right number of 'OnInjectedNewEntity' in output"
    sys.exit(1)

if output.count("<name>DopeTest.SmallEntity</name>") != 100:
    print "could not find the right number of 'DopeTest.SmallEntity' in output"
    sys.exit(1)

if output.count("<name>DopeTest.BigEntity</name>") != 10:
    print "could not find the right number of 'DopeTest.BigEntity' in output"
    sys.exit(1)

if len(glob.glob(os.path.join(file_storage_path,"DopeTest.*.bin"))) != 110:
    print "Unexpected bin files found!"
    sys.exit(1)

if len(glob.glob(os.path.join(file_storage_path,"DopeTest.*.xml"))) != 0:
    print "Unexpected bin files found!"
    sys.exit(1)

print "update the entities"

env = TestEnv()
with TestEnvStopper(env):
    #remove all bin files (that have been loaded by dope by now), so 
    #that we can wait for all entities to be written again
    for f in glob.glob(os.path.join(file_storage_path,"DopeTest.*.bin")):
        remove(f)

    env.launchProcess("entity_owner", (owner_path,"update")).wait()

    while(len(glob.glob(os.path.join(file_storage_path,"DopeTest.*.bin"))) != 110):
        time.sleep(0.1)

#if not env.ReturnCodesOk():
#    print "Some process exited with an unexpected value"
#    sys.exit(1)

print "Load them again and check output"
env = TestEnv()
with TestEnvStopper(env):
    env.launchProcess("entity_owner", (owner_path,"accept")).wait()

#if not env.ReturnCodesOk():
#    print "Some process exited with an unexpected value"
#    sys.exit(1)

output = env.Output("entity_owner")
if output.count("name is changed") != 100:
    print "could not find the right number of updated SmallEntity in output"
    sys.exit(1)

if output.count("99999999") != 10:
    print "could not find the right number of updated BigEntity in output"
    sys.exit(1)


rmdir(file_storage_path)

print "Success"
sys.exit(0)
