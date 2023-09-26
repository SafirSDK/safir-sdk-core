#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys

content1 = """<?xml version="1.0" encoding="utf-8" ?>
<class xmlns="urn:safir-dots-unit" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
"""

content2 = """
    <baseClass>Safir.Dob.Entity</baseClass>
    <members>
        <member>
            <summary>Int32.</summary>
            <name>MyInt32</name>
            <type>Int32</type>            
        </member>
        <member>
            <summary>String.</summary>
            <name>MyString</name>
            <type>String</type>            
        </member>
        <member>
            <summary>InstanceId.</summary>
            <name>MyInstanceId</name>
            <type>InstanceId</type>            
        </member>
        <member>
            <summary>TypeId.</summary>
            <name>MyTypeId</name>
            <type>TypeId</type>            
        </member>
        <member>
            <summary>EntityId.</summary>
            <name>MyEntityId</name>
            <type>EntityId</type>            
        </member>
    </members>
    <parameters>
        <parameter>
            <name>IntParam</name>
            <type>Int32</type>
            <value>123</value>
        </parameter>
        <parameter>
            <name>StringParam</name>
            <type>String</type>
            <value>Hello world</value>
        </parameter>
    </parameters>
</class>
"""

def main(argv):
    """Main program"""
    if len(argv) < 3:
        print('usage: dou_file_generator numFiles storePath')
        return

    num_files = int(argv[1])
    path = argv[2]

    for i in range(num_files):
        name = "Gen.MyEntity" + str(i)
        file_path = os.path.join(path, name + ".dou")        
        f = open(file_path, "w")
        f.write(content1)
        f.write("<name>" + name + "</name>")
        f.write(content2)
        f.close()

#------------------------------------------------
# If this is the main module, start the program
#------------------------------------------------
if __name__ == "__main__":
    main(sys.argv)
