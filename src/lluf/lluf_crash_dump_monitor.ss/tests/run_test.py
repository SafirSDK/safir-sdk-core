#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://www.safirsdk.com)
#
# Created by: Anders Widén <anders.widen@consoden.se>
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
import subprocess, os, time, sys, shutil
import argparse

try:
    import ConfigParser
except ImportError:
    import configparser as ConfigParser
try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO

parser = argparse.ArgumentParser("test script for LowLevelLogger")
parser.add_argument("--show-config", required=True)
parser.add_argument("--dump-monitor", required=True)
parser.add_argument("--config-dir", required=True)

arguments = parser.parse_args()

config_dir = arguments.config_dir
if not os.path.isdir(config_dir):
    print ("arg is not a directory")
    sys.exit(1)

os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = config_dir

#Run the program that writes the ini file configuration to standard output
proc = subprocess.Popen((arguments.show_config,"--locations"),
                        stdout=subprocess.PIPE,
                        stderr=subprocess.STDOUT,
                        universal_newlines=True)
# ConfigParser wants a section header so add a dummy one.
conf_str = '[root]\n' + proc.communicate()[0]

config = ConfigParser.ConfigParser()
config.readfp(StringIO(conf_str))

crash_dump_dir = config.get('root','crash_dump_directory')

# Make a fresh empty crash dump dir ...
if os.path.exists(crash_dump_dir):
    shutil.rmtree(crash_dump_dir)    
os.makedirs(crash_dump_dir)

# ... and create some dummy dump files

# Create the first file and get its last modification time. This info is then used
# to make the last modification time to differ substantially for the other files.
file = os.path.join(crash_dump_dir, "dump1")
open(file, 'w+')
stat = os.stat(file)
last_access_time = stat.st_atime
last_mod_time = stat.st_mtime
                         
for i in range(2, 31):
    f = os.path.join(crash_dump_dir, "dump" + str(i))                     
    open(f, 'w+')
    # make the file "older" than the previous one
    last_mod_time = last_mod_time - 4000
    os.utime(f, (last_access_time, last_mod_time))                         

# start the program that we are testing                        
proc = subprocess.Popen((arguments.dump_monitor, "--max-dump-files", "25", "--check-interval", "1", "--run-once"))
proc.communicate()

# We have created 30 files and max dumpfiles is set to 25 which maybe make
# you beleive that 5 files should deleted, leaving 25 files. Not so, actually
# the dump monitor always deletes 10 extra files leaving us with 15 files in this case.
remaining_dump_files = os.listdir(crash_dump_dir)

shutil.rmtree(crash_dump_dir) # we don't need the dump file directory anymore

if len(remaining_dump_files) != 15:
    print("Expected 15 remaining dump files but there are ", len(remaining_dump_files))
    sys.exit(1)

def get_num(str):
    return int(''.join(ele for ele in str if ele.isdigit()))

# Check that the newest files (this is the files that have the lowest numbers) are left
for file in remaining_dump_files:
    n = get_num(file)
    print ("n:", n)
    if n < 1 or n > 15:
        print("It seems that not the oldest dump files have been removed!")
        sys.exit(1)

print("Success!")
sys.exit(0)
  


