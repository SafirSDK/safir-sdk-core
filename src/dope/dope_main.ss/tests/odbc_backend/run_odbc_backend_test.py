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
#import subprocess, os, time, sys, shutil, glob, xml.dom.minidom
import sys, os, shutil, xml.dom.minidom, glob, time, subprocess, re

sys.path.append("../../../../swre/swre_test.ss/testutil")
from testenv import TestEnv, TestEnvStopper

def rmdir(directory):
    if os.path.exists(directory):
        try:
            shutil.rmtree(directory)
        except OSError:
            log("Failed to remove directory, will retry")
            time.sleep(0.2)
            shutil.rmtree(directory)


class Parameters:
    def __init__(self):
        from optparse import OptionParser
        import tempfile

        parser = OptionParser()
        parser.add_option("--driver", action="store",type="choice",choices=("mimer","mysql","postgres"),dest="driver",default=False,
                          help="Database engine to test against")
        parser.add_option("--hostname", action="store",dest="hostname",default="localhost",
                          help="Hostname of the database server")
        parser.add_option("--database", action="store",dest="database",
                          help="Hostname of the database server")

        (options,args) = parser.parse_args()

        if not options.driver:
            print "Need --driver argument"
            sys.exit(1)

        if not options.database:
            print "Need --database argument"
            sys.exit(1)

        self.driver = options.driver

        self.SAFIR_RUNTIME = os.environ.get("SAFIR_RUNTIME")
        if not self.SAFIR_RUNTIME:
            print "Cannot run without SAFIR_RUNTIME being set."
            sys.exit(1)

        self.parameters_path = os.path.join(self.SAFIR_RUNTIME,
                                            "data", 
                                            "text", 
                                            "dots", 
                                            "classes", 
                                            "safir_core", 
                                            "config")

        self.PersistenceParameters_path = os.path.join(self.parameters_path, 
                                                       "Safir.Dob.PersistenceParameters.dou")

        self.tempdir = tempfile.mkdtemp(prefix="dose_test_backup")

        if self.driver == "mimer":
            driver = os.environ.get("MIMER_ODBC_DRIVER_NAME")
            if driver is None:
                driver = "MIMER"
        elif self.driver == "mysql":
            driver = os.environ.get("MYSQL_ODBC_DRIVER_NAME")
            if driver is None:
                driver = "MySQL"

        #set up the connection string, starting with the driver specific parts
        if self.driver == "mimer":
            self.connection_string = "Driver={{{driver}}};Protocol=tcp;Node={hostname};"
        elif self.driver == "mysql":
            self.connection_string = "DRIVER={{{driver}}};Server={hostname};"
        #add common bits
        self.connection_string += "Database={database};Uid=dopeuser;Pwd=dopeuser"

        self.connection_string = self.connection_string.format(driver = driver,
                                                               hostname = options.hostname,
                                                               database = options.database)

        print "Using connection string", self.connection_string

def getText(nodelist):
    rc = []
    for node in nodelist:
        if node.nodeType == node.TEXT_NODE:
            rc.append(node.data)
    return ''.join(rc)

def UpdateConfig(parameters):
    print "** Updating configuration files"
    #backup dou files to temporary directory
    shutil.copy2(parameters.PersistenceParameters_path, parameters.tempdir)
    
    # Update PersistenceParameters
    dom = xml.dom.minidom.parse(parameters.PersistenceParameters_path)
    for param in dom.getElementsByTagName("parameter"):
        name = getText(param.getElementsByTagName("name")[0].childNodes)
        value = param.getElementsByTagName("value")[0]
        if name == "Backend":
            value.childNodes[0].data = "Odbc"
        if name == "OdbcStorageConnectString":
            value.childNodes[0].data = parameters.connection_string
    with open(parameters.PersistenceParameters_path,"w") as file:
       file.write(dom.toxml())

def RestoreConfig(parameters):
    print "** Restoring configuration files"
    #restore dou files from temporary directory
    dous = glob.glob(os.path.join(parameters.tempdir,"*.dou"))
    for dou in dous:
        shutil.copy2(os.path.join(parameters.tempdir,dou), parameters.parameters_path)
    rmdir(parameters.tempdir)

#make stdout unbuffered
sys.stdout = os.fdopen(sys.stdout.fileno(), 'w', 0)

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

owner_path = os.path.join(exe_path,"entity_owner")
run_sql_path = os.path.join(exe_path,"run_sql")

run_sql_pattern = re.compile(r"Got ([0-9]+) rows.")

def run_sql(parameters,statement):
    process = subprocess.Popen((run_sql_path,
                                "--connection-string", parameters.connection_string,
                                "--statement", statement),
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.STDOUT, 
                               universal_newlines=True)
    output = process.communicate()[0]
    if process.returncode != 0:
        print "Unexpected return code when running statement:", statement
        print output
        sys.exit(1)
    match = run_sql_pattern.search(output)
    try:
        return int(match.group(1))
    except Exception as e:
        print "Failed to get rows out of output:"
        print output
        sys.exit(1)

def count_rows(parameters):
    return run_sql(parameters,"select typeid from PersistentEntity")

def count_big(parameters):
    return run_sql(parameters,"select typeid from PersistentEntity where binarydata is not null")

def count_small(parameters):
    return run_sql(parameters,"select typeid from PersistentEntity where binarysmalldata is not null")

def delete_all_rows(parameters):
    return run_sql(parameters,"delete from PersistentEntity")

def count_xml(parameters):
    return run_sql(parameters,"select typeid from PersistentEntity where xmldata is not null")

def count_binares(parameters):
    return run_sql(parameters,"select typeid from PersistentEntity where binarydata is not null or binarysmalldata is not null")

def delete_binary(parameters):
    return run_sql(parameters,"update PersistentEntity set binarydata=null,binarysmalldata=null")

parameters = Parameters()
try:

    UpdateConfig(parameters)

    print "== Clear the db"
    delete_all_rows(parameters)

    print "== Set a bunch of entities"
    env = TestEnv()
    with TestEnvStopper(env):
        env.launchProcess("entity_owner", (owner_path,"set")).wait()
        print "waiting for 110 entities in db"
        while count_rows(parameters) != 110:
            time.sleep(1)

    if not env.ReturnCodesOk():
        print "Some process exited with an unexpected value"
        sys.exit(1)


    print "== See if dope loads them at startup"
    env = TestEnv()
    with TestEnvStopper(env):
        env.launchProcess("entity_owner", (owner_path,"accept")).wait()

    if not env.ReturnCodesOk():
        print "Some process exited with an unexpected value"
        sys.exit(1)

    output = env.Output("entity_owner")
    if output.count("OnInjectedNewEntity") != 110:
        print "could not find the right number of 'OnInjectedNewEntity' in output"
        sys.exit(1)

    if output.count("<name>DopeTest.SmallEntity</name>") != 100:
        print "could not find the right number of 'DopeTest.SmallEntity' in output"
        sys.exit(1)

    if count_small(parameters) != 100:
        print "could not find the right number of entries in the small column in the database"
        sys.exit(1)

    if output.count("<name>DopeTest.BigEntity</name>") != 10:
        print "could not find the right number of 'DopeTest.BigEntity' in output"
        sys.exit(1)

    if count_big(parameters) != 10:
        print "could not find the right number of entries in the big column in the database"
        sys.exit(1)


    if count_xml(parameters) != 0:
        print "Unexpected xml data found!"
        sys.exit(1)

    print "== Convert binaries to xml"

    #db is automatically chosen by parameter setting
    subprocess.call(os.path.join(parameters.SAFIR_RUNTIME,"bin","dope_bin2xml"))

    if count_xml(parameters) != 110 or count_rows(parameters) != 110:
        print "dope_bin2xml appears to have failed"
        sys.exit(1)

    #remove bin to check that dope can load xml
    delete_binary(parameters)
    if count_small(parameters) != 0 or count_big(parameters):
        print "Failed to delete binaries"
        sys.exit(1)

    print "== Check that dope can load xml"

    env = TestEnv()
    with TestEnvStopper(env):
        env.launchProcess("entity_owner", (owner_path,"accept")).wait()

    if not env.ReturnCodesOk():
        print "Some process exited with an unexpected value"
        sys.exit(1)

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

    if count_big(parameters) != 10 or count_small(parameters) != 100:
        print "xml does not appear to have been translated into binaries"
        sys.exit(1)

    if count_xml(parameters) != 0:
        print "Unexpected xml found!"
        sys.exit(1)


    print "== update the entities"


    env = TestEnv()
    with TestEnvStopper(env):
        #remove all bin binaries (that have been loaded by dope by now), so 
        #that we can wait for all entities to be written again
        #problem is that dope is rewriting them...
        while count_binares(parameters) != 0:
            delete_binary(parameters)
            time.sleep(1) #TODO don't rely on timing!

        env.launchProcess("entity_owner", (owner_path,"update")).wait()

        print "waiting for 110 entities in db"
        while count_binares(parameters) != 110:
            time.sleep(1)

    if not env.ReturnCodesOk():
        print "Some process exited with an unexpected value"
        sys.exit(1)

    print "== Load them again and check output"
    env = TestEnv()
    with TestEnvStopper(env):
        env.launchProcess("entity_owner", (owner_path,"accept")).wait()

    if not env.ReturnCodesOk():
        print "Some process exited with an unexpected value"
        sys.exit(1)

    output = env.Output("entity_owner")
    if output.count("name is changed") != 100:
        print "could not find the right number of updated SmallEntity in output"
        sys.exit(1)

    if output.count("99999999") != 10:
        print "could not find the right number of updated BigEntity in output"
        sys.exit(1)


finally:
    print "== Clear the db"
    delete_all_rows(parameters)

    RestoreConfig(parameters)

print "Success"
sys.exit(0)
