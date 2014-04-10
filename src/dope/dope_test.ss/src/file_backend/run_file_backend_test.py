#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
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
from __future__ import print_function
import subprocess, os, time, sys, shutil, glob, argparse
from testenv import TestEnv, TestEnvStopper

def log(data):
    print(data)
    sys.stdout.flush()

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
            log("Failed to remove directory, will retry")
            time.sleep(0.2)
            shutil.rmtree(directory)


parser = argparse.ArgumentParser("test script")
parser.add_argument("--safir-show-config", required=True)
parser.add_argument("--dose-main", required=True)
parser.add_argument("--dope-main", required=True)
parser.add_argument("--entity-owner", required=True)
parser.add_argument("--dope-bin2xml", required=True)

arguments = parser.parse_args()
            
file_storage_path = "/tmp/safir_sdk_core/persistence" #TODO: read from config

rmdir(file_storage_path)

subprocess.call((arguments.safir_show_config, "--typesystem"))

log("Set a bunch of entities")
env = TestEnv(arguments.dose_main, arguments.dope_main, arguments.safir_show_config)
with TestEnvStopper(env):
    env.launchProcess("entity_owner", (arguments.entity_owner,"set")).wait()
    while(len(glob.glob(os.path.join(file_storage_path,"DopeTest.*.bin"))) != 110):
        time.sleep(0.1)
        if not env.ReturnCodesOk():
            log("Some process exited with an unexpected value")
            sys.exit(1)


if not env.ReturnCodesOk():
    log("Some process exited with an unexpected value")
    sys.exit(1)

syslog_output = env.Syslog()
if len(syslog_output) != 0:
    log("Unexpected syslog output:\n" + syslog_output)
    sys.exit(1)

log("See if dope loads them at startup")
env = TestEnv(arguments.dose_main, arguments.dope_main, arguments.safir_show_config)
with TestEnvStopper(env):
    env.launchProcess("entity_owner", (arguments.entity_owner,"accept")).wait()

if not env.ReturnCodesOk():
    log("Some process exited with an unexpected value")
    sys.exit(1)

syslog_output = env.Syslog()
if len(syslog_output) != 0:
    log("Unexpected syslog output:\n" + syslog_output)
    sys.exit(1)

output = env.Output("entity_owner")
if output.count("OnInjectedNewEntity") != 110:
    log("could not find the right number of 'OnInjectedNewEntity' in output")
    sys.exit(1)

if output.count("<DopeTest.SmallEntity>") != 100:
    log("could not find the right number of 'DopeTest.SmallEntity' in output")
    sys.exit(1)

if output.count("<DopeTest.BigEntity>") != 10:
    log("could not find the right number of 'DopeTest.BigEntity' in output")
    sys.exit(1)

if len(glob.glob(os.path.join(file_storage_path,"DopeTest.*.xml"))) != 0:
    log("Unexpected xml files found!")
    sys.exit(1)

log("Convert binaries to xml")

#files are automatically chosen by parameter setting
subprocess.call(arguments.dope_bin2xml)

if len(glob.glob(os.path.join(file_storage_path,"DopeTest.*.xml"))) != 110:
    log("dope_bin2xml appears to have failed")
    sys.exit(1)

#remove bin files to check that dope can load xml
for f in glob.glob(os.path.join(file_storage_path,"DopeTest.*.bin")):
    remove(f)

log("Check that dope can load xml")

env = TestEnv(arguments.dose_main, arguments.dope_main, arguments.safir_show_config)
with TestEnvStopper(env):
    env.launchProcess("entity_owner", (arguments.entity_owner,"accept")).wait()

if not env.ReturnCodesOk():
    log("Some process exited with an unexpected value")
    sys.exit(1)

syslog_output = env.Syslog()
if len(syslog_output) != 0:
    log("Unexpected syslog output:\n" + syslog_output)
    sys.exit(1)

output = env.Output("entity_owner")
if output.count("OnInjectedNewEntity") != 110:
    log("could not find the right number of 'OnInjectedNewEntity' in output")
    sys.exit(1)

if output.count("<DopeTest.SmallEntity>") != 100:
    log("could not find the right number of 'DopeTest.SmallEntity' in output")
    sys.exit(1)

if output.count("<DopeTest.BigEntity>") != 10:
    log("could not find the right number of 'DopeTest.BigEntity' in output")
    sys.exit(1)

if len(glob.glob(os.path.join(file_storage_path,"DopeTest.*.bin"))) != 110:
    log("Unexpected bin files found!")
    sys.exit(1)

if len(glob.glob(os.path.join(file_storage_path,"DopeTest.*.xml"))) != 0:
    log("Unexpected bin files found!")
    sys.exit(1)

log("update the entities")

env = TestEnv(arguments.dose_main, arguments.dope_main, arguments.safir_show_config)
with TestEnvStopper(env):
    #remove all bin files (that have been loaded by dope by now), so 
    #that we can wait for all entities to be written again
    for f in glob.glob(os.path.join(file_storage_path,"DopeTest.*.bin")):
        remove(f)

    env.launchProcess("entity_owner", (arguments.entity_owner,"update")).wait()

    while(len(glob.glob(os.path.join(file_storage_path,"DopeTest.*.bin"))) != 110):
        time.sleep(0.1)

if not env.ReturnCodesOk():
    log("Some process exited with an unexpected value")
    sys.exit(1)

syslog_output = env.Syslog()
if len(syslog_output) != 0:
    log("Unexpected syslog output:\n" + syslog_output)
    sys.exit(1)

log("Load them again and check output")
env = TestEnv(arguments.dose_main, arguments.dope_main, arguments.safir_show_config)
with TestEnvStopper(env):
    env.launchProcess("entity_owner", (arguments.entity_owner,"accept")).wait()

if not env.ReturnCodesOk():
    log("Some process exited with an unexpected value")
    sys.exit(1)

syslog_output = env.Syslog()
if len(syslog_output) != 0:
    log("Unexpected syslog output:\n" + syslog_output)
    sys.exit(1)
    
output = env.Output("entity_owner")
if output.count("name is changed") != 100:
    log("could not find the right number of updated SmallEntity in output")
    sys.exit(1)

if output.count("99999999") != 10:
    log("could not find the right number of updated BigEntity in output")
    sys.exit(1)


rmdir(file_storage_path)

log("Success")
sys.exit(0)
