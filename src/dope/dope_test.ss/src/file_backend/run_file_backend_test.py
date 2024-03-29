#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
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
import subprocess, os, time, sys, shutil, glob, argparse, re
from testenv import TestEnv, TestEnvStopper

import configparser
from io import StringIO


def log(data):
    print(data)
    sys.stdout.flush()


def remove(filename):
    for i in range(NUM_BIG):
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


env = None

try:
    parser = argparse.ArgumentParser("test script")
    parser.add_argument("--safir-show-config", required=True)
    parser.add_argument("--safir-control", required=True)
    parser.add_argument("--dose-main", required=True)
    parser.add_argument("--dope-main", required=True)
    parser.add_argument("--entity-owner", required=True)
    parser.add_argument("--dope-bin2xml", required=True)

    arguments = parser.parse_args()

    config_str = subprocess.check_output((arguments.safir_show_config, "--locations"), universal_newlines=True)
    #ConfigParser wants a section header so add a dummy one.
    config_str = '[root]\n' + config_str
    config = configparser.ConfigParser()
    config.read_file(StringIO(config_str))

    file_storage_path = os.path.join(config.get('root', 'lock_file_directory'), "..", "persistence")

    rmdir(file_storage_path)

    log("Find out how many entities entity_owner will set")
    num_str = subprocess.check_output((arguments.entity_owner, "num"), universal_newlines=True)
    NUM_SMALL = int(re.search(r"NUM_SMALL = ([0-9]+)", num_str).group(1))
    NUM_BIG = int(re.search(r"NUM_BIG = ([0-9]+)", num_str).group(1))
    log("NUM_SMALL = " + str(NUM_SMALL) + " and NUM_BIG = " + str(NUM_BIG))

    log("Set a bunch of entities")
    env = TestEnv(arguments.safir_control, arguments.dose_main, arguments.dope_main, arguments.safir_show_config)
    with TestEnvStopper(env):
        env.launchProcess("entity_owner", (arguments.entity_owner, "set")).wait()
        while (len(glob.glob(os.path.join(file_storage_path, "DopeTest.*.bin"))) != NUM_SMALL + NUM_BIG):
            time.sleep(0.1)
            if not env.ProcessDied():
                log("Some process exited with an unexpected value")
                sys.exit(1)

    syslog_output = env.Syslog()
    if len(syslog_output) != 0:
        log("Unexpected syslog output:\n" + syslog_output)
        sys.exit(1)

    if not env.ReturnCodesOk():
        log("Some process exited with an unexpected value")
        sys.exit(1)

    log("See if dope loads them at startup")
    env = TestEnv(arguments.safir_control, arguments.dose_main, arguments.dope_main, arguments.safir_show_config)
    with TestEnvStopper(env):
        env.launchProcess("entity_owner", (arguments.entity_owner, "accept")).wait()

    syslog_output = env.Syslog()
    if len(syslog_output) != 0:
        log("Unexpected syslog output:\n" + syslog_output)
        sys.exit(1)

    if not env.ReturnCodesOk():
        log("Some process exited with an unexpected value")
        sys.exit(1)

    output = env.Output("entity_owner")
    if output.count("OnInjectedNewEntity") != NUM_SMALL + NUM_BIG:
        log("could not find the right number of 'OnInjectedNewEntity' in output")
        log(output)
        sys.exit(1)

    if output.count("<DopeTest.SmallEntity>") != NUM_SMALL:
        log("could not find the right number of 'DopeTest.SmallEntity' in output")
        sys.exit(1)

    if output.count("Correct string!") != NUM_SMALL or output.count("Incorrect string!") != 0:
        log("Unicode string check failed")
        sys.exit(1)

    if output.count("<DopeTest.BigEntity>") != NUM_BIG:
        log("could not find the right number of 'DopeTest.BigEntity' in output")
        sys.exit(1)

    if len(glob.glob(os.path.join(file_storage_path, "DopeTest.*.xml"))) != 0:
        log("Unexpected xml files found!")
        sys.exit(1)

    log("Convert binaries to xml")

    #files are automatically chosen by parameter setting
    subprocess.call(arguments.dope_bin2xml)

    if len(glob.glob(os.path.join(file_storage_path, "DopeTest.*.xml"))) != NUM_SMALL + NUM_BIG:
        log("dope_bin2xml appears to have failed")
        sys.exit(1)

    #remove bin files to check that dope can load xml
    for f in glob.glob(os.path.join(file_storage_path, "DopeTest.*.bin")):
        remove(f)

    log("Check that dope can load xml")

    env = TestEnv(arguments.safir_control, arguments.dose_main, arguments.dope_main, arguments.safir_show_config)
    with TestEnvStopper(env):
        env.launchProcess("entity_owner", (arguments.entity_owner, "accept")).wait()

    syslog_output = env.Syslog()
    if len(syslog_output) != 0:
        log("Unexpected syslog output:\n" + syslog_output)
        sys.exit(1)

    if not env.ReturnCodesOk():
        log("Some process exited with an unexpected value")
        sys.exit(1)

    output = env.Output("entity_owner")
    if output.count("OnInjectedNewEntity") != NUM_SMALL + NUM_BIG:
        log("could not find the right number of 'OnInjectedNewEntity' in output")
        log(output)
        sys.exit(1)

    if output.count("<DopeTest.SmallEntity>") != NUM_SMALL:
        log("could not find the right number of 'DopeTest.SmallEntity' in output")
        sys.exit(1)

    if output.count("Correct string!") != NUM_SMALL or output.count("Incorrect string!") != 0:
        log("Unicode string check failed")
        sys.exit(1)

    if output.count("<DopeTest.BigEntity>") != NUM_BIG:
        log("could not find the right number of 'DopeTest.BigEntity' in output")
        sys.exit(1)

    if len(glob.glob(os.path.join(file_storage_path, "DopeTest.*.bin"))) != NUM_SMALL + NUM_BIG:
        log("Unexpected bin files found!")
        sys.exit(1)

    if len(glob.glob(os.path.join(file_storage_path, "DopeTest.*.xml"))) != 0:
        log("Unexpected bin files found!")
        sys.exit(1)

    log("update the entities")

    env = TestEnv(arguments.safir_control, arguments.dose_main, arguments.dope_main, arguments.safir_show_config)
    with TestEnvStopper(env):
        #Remove all bin files (that have been loaded by dope by now), so
        #that we can wait for all entities to be written again.
        #Since dope will be writing the files a while after everything has
        #started (it will get them from dose) we need to stay here
        #for a while and check for files reappearing. Hacky, but good enough.
        while (len(glob.glob(os.path.join(file_storage_path, "DopeTest.*.bin"))) != 0):
            for f in glob.glob(os.path.join(file_storage_path, "DopeTest.*.bin")):
                remove(f)
            time.sleep(10)

        env.launchProcess("entity_owner", (arguments.entity_owner, "update")).wait()

        while (len(glob.glob(os.path.join(file_storage_path, "DopeTest.*.bin"))) != NUM_SMALL + NUM_BIG):
            time.sleep(0.1)

    #The above removing of files while dope is writing them may cause
    #some logs to be generated, which is expected. We try to get rid
    #of them to not generate false negatives.
    syslog_output = env.Syslog()
    syslog_output_filtered = ""
    for line in syslog_output.splitlines():
        if line.find("boost::filesystem::permissions") == -1:
            syslog_output_filtered += line
    if len(syslog_output_filtered) != 0:
        log("Unexpected syslog output:\n" + syslog_output_filtered)
        sys.exit(1)

    if not env.ReturnCodesOk():
        log("Some process exited with an unexpected value")
        sys.exit(1)

    log("Load them again and check output")
    env = TestEnv(arguments.safir_control, arguments.dose_main, arguments.dope_main, arguments.safir_show_config)
    with TestEnvStopper(env):
        env.launchProcess("entity_owner", (arguments.entity_owner, "accept")).wait()

    syslog_output = env.Syslog()
    if len(syslog_output) != 0:
        log("Unexpected syslog output:\n" + syslog_output)
        sys.exit(1)

    if not env.ReturnCodesOk():
        log("Some process exited with an unexpected value")
        sys.exit(1)

    output = env.Output("entity_owner")
    if output.count("name is changed") != NUM_SMALL:
        log("could not find the right number of updated SmallEntity in output")
        sys.exit(1)

    if output.count("99999999") != NUM_BIG:
        log("could not find the right number of updated BigEntity in output")
        sys.exit(1)

    rmdir(file_storage_path)

except:
    print("Unexpected exception!")
    if env is not None:
        syslog_output = env.Syslog()
        if len(syslog_output) != 0:
            print("syslog output:\n" + syslog_output)
    raise

log("Success")
sys.exit(0)
