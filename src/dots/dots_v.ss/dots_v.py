#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013 (http://www.safirsdk.com)
#
# Created by: Björn Weström
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

# Prepared for Python 3
from __future__ import print_function
##from __future__ import unicode_literals

import sys, os, re, hashlib
import xml.etree.ElementTree as ET
from glob import glob
import codecs

CURRENT_SECTION = None
CURRENT_GENERATED_FILENAME = None
CURRENT_GENERATED_FILE = None
PARENT_NAMESPACE = None
dou_file_root = ""
dod_parameter_names = [\
        "File_Suffix", "Filename_Separator", "Output_Directory", "Namespace_Separator", \
        "Namespace_Prefix_File_Suffix", "Parent_Filename", "Namespace_Underscore_Style", "Filename_Underscore_Style", \
        "Classname_Underscore_Style", "Membername_Underscore_Style", "Enum_Value_Underscore_Style", "Namespace_Case_Style", \
        "Filename_Case_Style", "Classname_Case_Style", "Membername_Case_Style", "Enum_Value_Case_Style", \
        "Object_Type", "Index_Type" \
        ]
dod_parameters = {}
dod_exceptions = {}
dod_types = {}

loglevel = 3

class Dou(object):    
    def __init__(self):
        self.type = ""
        self.baseClass = ""
        self.baseClassOrg = ""
        self.name = ""
        self.summary = ""
        self.classname = ""
        self.namespaces = []
        self.members = []
        self.unique_dependencies = []
        self.dependency_base = []
        self.parameters = []
        self.createRoutines = []
        self.values = []
        self.member_name_to_type_lookup = {}


class DouMember(object):
    
    def __init__(self, summary, name, type, maxLength, array, arraySize, arraySizeRef):
        self.summary = summary
        self.name = name
        self.type = type
        self.maxLength = maxLength
        self.array = array
        self.arraySize = arraySize
        self.arraySizeRef = arraySizeRef

class DouCreateRoutine(object):
    
    def __init__(self, summary, name, parameters, values):
        self.summary = summary
        self.name = name
        self.parameters = parameters
        self.values = values

class DouCreateRoutineParameter(object):
    
    def __init__(self, summary, name, type, maxLength, arraySize):
        self.summary = summary
        self.name = name
        self.type = type
        self.maxLength = maxLength
        self.arraySize = arraySize

class DouCreateRoutineValue(object):
    
    def __init__(self, member, parameter, index, array, arraySize):
        self.member = member
        self.parameter = parameter
        self.index = index
        self.array = array
        self.arraySize = arraySize
        
class DouParameter(object):
    
    def __init__(self, summary, name, type, value, arrayElements):
        self.summary = summary
        self.name = name
        self.type = type
        self.value = value
        self.arrayElements = arrayElements
        
def readTextPropery(xml_root, element):
    prefixed = "{urn:safir-dots-unit}" + element
    xe = xml_root.find(prefixed)
    if xe is None: return None
    # This is a simple trick to detect tags even if they are empty
    if xe.text is None: return ""
    return xe.text

def dou_uniform_translate(typename):
    typename = typename.title()
    if typename == "Class" : typename = "Object"
    return typename

# We don't need to clear this between the dou or dod files, since the result is the same for all
duo_uniform_lookup_cache = {}    
def dou_uniform_lookup_init():
    for path, dirs, files in os.walk(dou_file_root): # Walk directory tree
        for file in files:
            if file.endswith(".dou"):
                dou_xml = ET.parse(os.path.join(path, file))
                xml_root = dou_xml.getroot()
                dou_type = xml_root.tag.split("}")[1]
                # Make first char uppercase
                dou_type = dou_uniform_translate(dou_type)
                duo_uniform_lookup_cache[os.path.splitext(file)[0]] = dou_type


def dou_uniform_lookup(typename):
    if typename in duo_uniform_lookup_cache: return duo_uniform_lookup_cache[typename]
                
    print("** ERROR - Cannot match dou type", typename, file=sys.stderr)
    return "** ERROR - Cannot match dou type"

namespace_prefixes = {}
# We only need to call this once per dod file (if Namespace prefixes are used) 
# since it does not differ between dou files
def namespace_prefix_init():
    global namespace_prefixes
    
    if dod_parameters['Namespace_Prefix_File_Suffix'] == "" : return
    if len(namespace_prefixes) != 0 and \
            (namespace_prefixes["¤¤Namespace_Prefix_File_Suffix¤¤"] == \
            dod_parameters['Namespace_Prefix_File_Suffix']):
        # The dict has already been initialized for this file suffix, leave it
        return
    
    namespace_prefixes.clear()
    namespace_prefixes["¤¤Namespace_Prefix_File_Suffix¤¤"] = dod_parameters['Namespace_Prefix_File_Suffix']
    
    for path, dirs, files in os.walk(dou_file_root): # Walk directory tree
        for file in files:
            if file.endswith(dod_parameters['Namespace_Prefix_File_Suffix']):
                prefix_file = codecs.open(os.path.join(path, file), "r", encoding="utf-8")
                found = False
                line = prefix_file.readline()
                while len(line) > 0 and not found:
                    if not line.startswith("#") and not line.startswith("--") and line.rstrip() != "": 
                        found = True
                        namespace = file[:file.find(dod_parameters['Namespace_Prefix_File_Suffix'])]
                        namespace_prefixes[namespace] = line.rstrip()
                    line = prefix_file.readline()

def namespace_prefixer(typename):
    # global namespace_prefixes   Read Only
    bestmatch_prefix = ""
    
    if dod_parameters['Namespace_Prefix_File_Suffix'] == "": return typename
    
    ts = typename.split(".")
    if namespace_prefixes is not None:
        # check for namespace prefix files, best match is longest match
        ns = ""
        for n in ts:
            if len(ns) == 0: ns = n
            else: ns = ns + "." + n
            
            if ns in namespace_prefixes:
                bestmatch_prefix = namespace_prefixes[ns]                    

    if bestmatch_prefix == "": return typename
    return bestmatch_prefix + "." + typename

seek_member_in_base_class_cache = {}
def seek_member_in_base_class(seek_name, baseClass):
    if baseClass is None: 
        return None
        
    if (seek_name, baseClass) in seek_member_in_base_class_cache: 
        return seek_member_in_base_class_cache[(seek_name, baseClass)]
    
    parent = None
    baseClassFile = baseClass + ".dou"
    
    for path, dirs, files in os.walk(dou_file_root): # Walk directory tree
        if baseClassFile in files:
            dou_xml = ET.parse(os.path.join(path,baseClassFile))
            xml_root = dou_xml.getroot()
            parent = readTextPropery(xml_root, "baseClass")
            members = xml_root.find("{urn:safir-dots-unit}members")
            if members is not None:
                for m in members:
                    m_name = readTextPropery(m, "name")
                    if seek_name == m_name:
                        # Found it!
                        found_member = DouMember( readTextPropery(m, "summary"), \
                                    m_name, \
                                    readTextPropery(m, "type"), \
                                    readTextPropery(m, "maxLength"), \
                                    readTextPropery(m, "array"), \
                                    readTextPropery(m, "arraySize"), \
                                    m.find("{urn:safir-dots-unit}arraySizeRef") is not None)
                        seek_member_in_base_class_cache[(seek_name, baseClass)] = found_member
                        return found_member
    
    # No match, try next level
    return seek_member_in_base_class(seek_name, parent)

def parse_dou(dou_xmlfile):
    global dod_types
    #global dod_exceptions    Read only
    #global dod_parameters    Read only

    parsed = Dou()
    dou_xml = ET.parse(dou_xmlfile)
    xml_root = dou_xml.getroot()
    parsed.type = xml_root.tag.split("}")[1]
    parsed.name = readTextPropery(xml_root, "name")
    # Find namespaces
    if parsed.name.find(".") != -1:
        nss = parsed.name.split(".")
        parsed.classname = nss.pop()
        for n in nss:    
            parsed.namespaces.append(n)
    else:
        parsed.classname = parsed.name
    
    if dod_parameters['Namespace_Prefix_File_Suffix'] != "":
        namespace_prefix_init()
        
    parsed.baseClass = readTextPropery(xml_root, "baseClass")
    parsed.baseClassOrg = parsed.baseClass

    if not parsed.baseClass is None: # and not parsed.baseClass == "Object":
        # Check for exceptions
        if parsed.baseClass in dod_exceptions:
            baseClass = parsed.baseClass
            parsed.baseClass = dod_exceptions[baseClass].generated
            #parsed.unique_membertypes.append(dod_exceptions[baseClass].generated)
            #parsed.unique_dependencies.append(type_formatter(dod_exceptions[baseClass].dependency))
            parsed.unique_dependencies.append(dependency_formatter(dod_exceptions[baseClass].dependency))
        elif parsed.baseClass in dod_types:
            baseClass = parsed.baseClass
            parsed.baseClass = dod_types[baseClass].generated
            #parsed.unique_membertypes.append(dod_types[baseClass].generated)
            #parsed.unique_dependencies.append(type_formatter(dod_types[baseClass].dependency))
            parsed.unique_dependencies.append(dod_types[baseClass].dependency)
        else:
            parsed.baseClass = type_formatter(parsed.baseClass)
            #parsed.unique_membertypes.append(parsed.baseClass)
            parsed.unique_dependencies.append(dependency_formatter(parsed.baseClass))
            
    parsed.summary = summary_formatter(readTextPropery(xml_root, "summary"))
    
    m_type = parsed.name
    #parsed.unique_membertypes.append(m_type)
    dod_types[m_type] = DodType(m_type, m_type, dou_uniform_translate(parsed.type), type_formatter(m_type), dependency_formatter(m_type))

    if parsed.type == "property":
        # For unknown reasons, the old dots_v adds a dependency to Object for all property dous
        parsed.unique_dependencies.append(type_formatter(dod_types["Object"].dependency))
    
    member_name_to_type_lookup = {}
    member_name_to_is_array_lookup = {}
    
    members = xml_root.find("{urn:safir-dots-unit}members")
    if members is not None:
        for m in members:
        #.findall("{urn:safir-dots-unit}member")
            m_type = readTextPropery(m, "type")
            m_name = readTextPropery(m, "name")
            m_array = readTextPropery(m, "array")
            m_arraySize = readTextPropery(m, "arraySize")
            m_arraySizeRef = m.find("{urn:safir-dots-unit}arraySizeRef") is not None
            parsed.members.append( DouMember( summary_formatter(readTextPropery(m, "summary")), \
                                        m_name, \
                                        m_type, \
                                        readTextPropery(m, "maxLength"), \
                                        m_array, \
                                        m_arraySize,
                                        m_arraySizeRef) )
            member_name_to_type_lookup[m_name] = m_type
            member_name_to_is_array_lookup[m_name] = (m_arraySize is not None or m_array is not None or m_arraySizeRef)
            
            #if not m_type in parsed.unique_membertypes: 
            #    parsed.unique_membertypes.append(m_type)
            if not (m_type in dod_types): 
                # This is a dou defined object
                uniform_type = dou_uniform_lookup(m_type)
                dod_types[m_type] = DodType(m_type, m_type, uniform_type, type_formatter(m_type), dependency_formatter(m_type))
                    
                parsed.unique_dependencies.append(dependency_formatter(m_type))
            elif len(dod_types[m_type].dependency) > 0:
                #parsed.unique_dependencies.append(type_formatter(dod_types[m_type].dependency))
                parsed.unique_dependencies.append(dod_types[m_type].dependency)
                
            if (m_type == "String" or member_name_to_is_array_lookup[m_name]): # and not dod_parameters["Index_Type"] in parsed.unique_membertypes:
                #parsed.unique_membertypes.append(dod_parameters["Index_Type"])
                parsed.unique_dependencies.append(dod_types[dod_parameters["Index_Type"]].dependency)
            
    crs = xml_root.find("{urn:safir-dots-unit}createRoutines")
    if crs is not None:
        for cr in crs:
            parameters = []
            ps = cr.find("{urn:safir-dots-unit}parameters")
            if ps is not None:
                for p in ps:
                    parameter_tag = p.tag.split("}")[1]
                    if parameter_tag == "member":
                        found = False
                        for m in parsed.members:
                            if p.text == m.name:
                                parameters.append( DouCreateRoutineParameter( m.summary, \
                                                                m.name, \
                                                                m.type, \
                                                                m.maxLength, \
                                                                m.arraySize) )
                                found = True
                        if not found:
                            found_member = seek_member_in_base_class(p.text, parsed.baseClassOrg)
                            if found_member is not None:
                                parameters.append(DouCreateRoutineParameter( summary_formatter(found_member.summary), \
                                                                found_member.name, \
                                                                found_member.type, \
                                                                found_member.maxLength, \
                                                                found_member.arraySize) )
                                member_name_to_type_lookup[found_member.name] = found_member.type
                                member_name_to_is_array_lookup[found_member.name] = (found_member.arraySize is not None or found_member.array is not None or found_member.arraySizeRef)
                                
                                if not found_member.type in dod_types:
                                    dod_types[found_member.type] = DodType(found_member.type, \
                                                                        found_member.type, \
                                                                        dou_uniform_lookup(found_member.type), \
                                                                        type_formatter(found_member.type), \
                                                                        dependency_formatter(found_member.type))
                                    parsed.unique_dependencies.append(dependency_formatter(found_member.type))
                                elif len(dod_types[found_member.type].dependency) > 0:
                                    parsed.unique_dependencies.append(dod_types[found_member.type].dependency)
                            else:
                                print(">!< cannot find member reference", p.text, file=sys.stderr)
                    elif parameter_tag == "parameter":
                        
                        m_type = readTextPropery(p, "type")
                        m_arraySize = readTextPropery(m, "arraySize")
                        parameters.append( DouCreateRoutineParameter( summary_formatter(readTextPropery(p, "summary")), \
                                                        readTextPropery(p, "name"), \
                                                        m_type, \
                                                        readTextPropery(p, "maxLength"), \
                                                        m_arraySize ) )
                        
                        if not m_type is None:
                            #if not (m_type in parsed.unique_membertypes):
                            #    parsed.unique_membertypes.append(m_type)
                            if not (m_type in dod_types): 
                                # This is a dou defined object
                                uniform_type = dou_uniform_lookup(m_type)
                                dod_types[m_type] = DodType(m_type, m_type, uniform_type, type_formatter(m_type), dependency_formatter(m_type))
                                parsed.unique_dependencies.append(dependency_formatter(m_type))
                            elif len(dod_types[m_type].dependency) > 0:
                                #parsed.unique_dependencies.append(type_formatter(dod_types[m_type].dependency))
                                parsed.unique_dependencies.append(dod_types[m_type].dependency)

                        if (m_type == "String" or m_arraySize is not None): # and not dod_parameters["Index_Type"] in parsed.unique_membertypes:
                            #parsed.unique_membertypes.append(dod_parameters["Index_Type"])
                            parsed.unique_dependencies.append(dod_types[dod_parameters["Index_Type"]].dependency)

                    
                    else:
                        print("** ERROR unknown CreateRoutineParameter subtag", parameter_tag, file=sys.stderr)

            values = []
            vs = cr.find("{urn:safir-dots-unit}values")
            if vs is not None:
                for v in vs:
                    m_parameter = readTextPropery(v, "parameter")
                    m_member = readTextPropery(v, "member")
                    m_type = member_name_to_type_lookup[m_member]
                    m_array = None
                    if member_name_to_is_array_lookup[m_member]: m_array = True
                    values.append( DouCreateRoutineValue( m_member, \
                                                            m_parameter, \
                                                            readTextPropery(v, "index"), \
                                                            m_array, \
                                                            readTextPropery(v, "arraySize") ) )
                    parameter_class = m_parameter[:m_parameter.rfind(".")]
                    if not parameter_class == parsed.name:
                        parsed.unique_dependencies.append(dependency_formatter(parameter_class))
                
            parsed.createRoutines.append( DouCreateRoutine(     summary_formatter(readTextPropery(cr, "summary")), \
                                                                readTextPropery(cr, "name"), \
                                                                parameters, \
                                                                values ) )
    
    values = xml_root.find("{urn:safir-dots-unit}values")
    if values is not None:
        for v in values:
            parsed.values.append( v.text )

    parameters = xml_root.find("{urn:safir-dots-unit}parameters")
    if parameters is not None:
        for p in parameters:
            arrayElements = []
            #aes = p.find("{urn:safir-dots-unit}arrayElements")
            aes = p.find(".//{urn:safir-dots-unit}arrayElements")
            if not aes is None:
                for ae in aes:
                    arrayElements.append(ae.text)
                    
            m_type = readTextPropery(p, "type")
            parsed.parameters.append( DouParameter( summary_formatter(readTextPropery(p, "summary")), \
                                        readTextPropery(p, "name"), \
                                        m_type, \
                                        readTextPropery(p, "value"), \
                                        arrayElements) )

            if len(arrayElements) > 0: # and not dod_parameters["Index_Type"] in parsed.unique_membertypes:
                #parsed.unique_membertypes.append(dod_parameters["Index_Type"])
                parsed.unique_dependencies.append(dod_types[dod_parameters["Index_Type"]].dependency)

                                        
            #if not m_type in parsed.unique_membertypes: 
            #    parsed.unique_membertypes.append(m_type)
            if not (m_type in dod_types): 
                # This is a dou defined object
                uniform_type = dou_uniform_lookup(m_type)
                dod_types[m_type] = DodType(m_type, m_type, uniform_type, type_formatter(m_type), dependency_formatter(m_type))
                parsed.unique_dependencies.append(dependency_formatter(m_type))
            elif len(dod_types[m_type].dependency) > 0:
                #parsed.unique_dependencies.append(type_formatter(dod_types[m_type].dependency))
                parsed.unique_dependencies.append(dod_types[m_type].dependency)

    # Remove duplicates from dependencies and membertypes
    parsed.unique_dependencies = filter_duplicates(parsed.unique_dependencies)
    #parsed.unique_membertypes = filter_duplicates(parsed.unique_membertypes)
    # Sort dependencies by path
    parsed.unique_dependencies.sort()
    # For some reason, the "ValueContainers" dependency is put last by the ADA dots_v, so do the same here
    vc_dependency = dod_types[dod_parameters["Index_Type"]].dependency
    if vc_dependency in parsed.unique_dependencies:
        parsed.unique_dependencies.remove(vc_dependency)
        parsed.unique_dependencies.append(vc_dependency)
    
    # remove empty dependency
    empty_dependency = ""
    if empty_dependency in parsed.unique_dependencies:
        parsed.unique_dependencies.remove(empty_dependency)
    
    # remove recursive dependencies
    if dod_types[parsed.name].dependency in parsed.unique_dependencies:
        parsed.unique_dependencies.remove(dod_types[parsed.name].dependency)    
#        dependency_formatter()
#    if dod_types[parsed.name].dependency in parsed.unique_dependencies:
#        parsed.unique_dependencies.remove(dod_types[parsed.name].dependency)    
    
    parsed.member_name_to_type_lookup = member_name_to_type_lookup
    
    # list all base classes for dependencies
    for dep in parsed.unique_dependencies:
        if dep.find(".") != -1:
            dep_base = dep[:dep.rfind(".")]
            if not dep_base in parsed.dependency_base:
                parsed.dependency_base.append(dep_base)            
        elif dep.find(dod_parameters["Namespace_Separator"]) != -1:
            dep_base = dep[:dep.rfind(dod_parameters["Namespace_Separator"])]
            if not dep_base in parsed.dependency_base:
                parsed.dependency_base.append(dep_base)            
    parsed.dependency_base.sort()
    
    return parsed

def filter_duplicates(seq):
    seen = set()
    seen_add = seen.add
    return [ x for x in seq if x not in seen and not seen_add(x)]    

def read_dod_parameter(line, parameter_name, dod_parameters):
    if line.startswith(parameter_name): 
        # All parameters are strings with quotes
        dod_parameters[parameter_name] = line.split("\"")[1]
        if loglevel > 5: print("[DEBUG5]", parameter_name , "is: [" , dod_parameters[parameter_name] , "]")
        return 1
        
    return 0

def are_dod_parameters_complete(dod_parameters, dod_parameter_names):
    for dp in dod_parameter_names:
        if not dp in dod_parameters: 
            print("** Missing parameter:", dp, file=sys.stderr)
            return 0
    return 1
    
class DodException(object):
    def __init__(self, dou_name, generated, dependency):
        self.dou_name = dou_name
        self.generated = generated
        self.dependency = dependency
    
    def printme(self):
        return "dou:", self.dou_name, "generated:", self.generated, "dependency:", self.dependency
        
def read_dod_exception(line, dod_exceptions):
    # Format:
    # Exception:<dou>:"<generated>":"<dependency>"
    ls = line.split(":", 2)
    dou_name = ls[1]
    ls2 = ls[2].split("\"")
    generated = ls2[1]
    dependency = ls2[3]
    
    parsed_exception = DodException(dou_name, generated, dependency)
    dod_exceptions[parsed_exception.dou_name] = parsed_exception
    if loglevel > 5: print("[DEBUG5]", parsed_exception.dou_name , "is: [" , parsed_exception.printme() , "]")
    
    return 0

class DodType(object):
    def __init__(self, dou_name, set_get, uniform_type, generated, dependency):
        self.dou_name = dou_name
        self.set_get = set_get
        self.uniform_type = uniform_type
        self.generated = generated
        self.dependency = dependency
    
    def printme(self):
        return "dou:", self.dou_name, "set/get:", self.set_get, "uniform:", self.uniform_type, "generated:", self.generated, "dependency:", self.dependency

    
def read_dod_type(line, dod_types):
    # Format:
    # Type:<dou>:<set/get>:"<generated>":"<dependency>"
    ls = line.split(":", 3)
    dou_name = ls[1]
    set_get = ls[2]
    ls3 = ls[3].split("\"")
    generated = ls3[1]
    dependency = ls3[3]
    
    parsed_type = DodType(dou_name, set_get, set_get, generated, dependency)
    dod_types[parsed_type.dou_name] = parsed_type
    if loglevel > 5: print("[DEBUG5]", parsed_type.dou_name , "is: [" , parsed_type.printme() , "]")
    
    return 0

def underscore_formatter(name, style):
    if style == "Keep": return name
    elif style == "Add":
        # Underscores allowed before 
        #    - word (captial + small) preceeded by any char except . _ : ¤
        #    - capital letter preceeded with small letter or number
        #    - number preceeded by capital or small letter
        #    - small letter preceeded by a number
        #    - last capital letter in a sequence of capital letters, if followed by small letter or number
        s1 = re.sub(u'([^\._:\u00A4])([A-Z][a-z]+)', r'\1_\2', name)
        s2 = re.sub('([a-z])([A-Z])', r'\1_\2', s1)
        s3 = re.sub('([0-9])([A-Za-z])', r'\1_\2', s2)
        s4 = re.sub('([A-Za-z])([0-9])', r'\1_\2', s3)
        return s4
    else:
        print("** Error, unsupported underscore format", style, file=sys.stderr)
        return name

def case_formatter(name, style):
    # *_Case_Style is one of: "Upper", "Lower", "Camel", "Keep"
    if style == "Keep": return name
    elif style == "Upper": return name.upper()
    elif style == "Lower": return name.lower()
    else:
        print("** Error, unsupported case format", style, file=sys.stderr)
        return name

def separator_formatter(name, style):
    if style != dod_parameters["Namespace_Separator"]:
        name = name.replace(dod_parameters["Namespace_Separator"], style)
        
    return name.replace(".", style)

def directory_name_formatter(directory_name):
    directory_name = underscore_formatter(directory_name, dod_parameters["Filename_Underscore_Style"])
    directory_name = separator_formatter(directory_name, dod_parameters["Filename_Separator"])
    directory_name = case_formatter(directory_name, dod_parameters["Namespace_Case_Style"])
    return directory_name
    
def filename_formatter(filename):
    filename = underscore_formatter(filename, dod_parameters["Filename_Underscore_Style"])
    filename = separator_formatter(filename, dod_parameters["Filename_Separator"])
    filename = case_formatter(filename, dod_parameters["Filename_Case_Style"])
    return filename    
    
def member_formatter(member):
    member = underscore_formatter(member, dod_parameters["Membername_Underscore_Style"])
    member = case_formatter(member, dod_parameters["Membername_Case_Style"])
    return member

def classname_formatter(classname):
    classname = underscore_formatter(classname, dod_parameters["Classname_Underscore_Style"])
    classname = case_formatter(classname, dod_parameters["Classname_Case_Style"])
    return classname

def namespace_formatter(namespace):
    if namespace is None: return namespace
    
    namespace = underscore_formatter(namespace, dod_parameters["Namespace_Underscore_Style"])
    namespace = separator_formatter(namespace, dod_parameters["Namespace_Separator"])
    namespace = case_formatter(namespace, dod_parameters["Namespace_Case_Style"])
    return namespace

def type_formatter(type):
    if type is None: return type
    
    if type.find(".") != -1:
        parts = type.split(".")
        classname = parts.pop()
        namespace = namespace_prefixer(".".join(parts))
        return namespace_formatter(namespace) + dod_parameters["Namespace_Separator"] + classname_formatter(classname)
    elif type.find(dod_parameters["Namespace_Separator"]) != -1: 
        parts = type.split(dod_parameters["Namespace_Separator"])
        classname = parts.pop()
        namespace = namespace_prefixer(".".join(parts))
        return namespace_formatter(namespace) + dod_parameters["Namespace_Separator"] + classname_formatter(classname)
    else:
        # No namespace to format
        return classname_formatter(type)


def unit_formatter(unit):
    return dependency_formatter(unit)
        
def enum_formatter(enum):
    enum = underscore_formatter(enum, dod_parameters["Enum_Value_Underscore_Style"])
    enum = case_formatter(enum, dod_parameters["Enum_Value_Case_Style"])
    return enum    
    
def dependency_formatter(dependency):
    return type_formatter(dependency).replace(dod_parameters["Namespace_Separator"], ".")

def dependencybase_formatter(dependency):
    return namespace_formatter(dependency).replace(dod_parameters["Namespace_Separator"], ".")

def summary_formatter(summary):
    if summary is None: return ""

    # Some summaries include indentation after linebreaks, which screws up the indendation
    # made by the parser, so we have to remove them
    if summary.find("\n") != -1:
        summary_split = summary.split("\n")

        summary = ""
        
        if len(summary_split[0].strip()) > 0:
            # First line is not empty, this line is treated separately since it should not be indented.
            summary = summary_split[0].lstrip()
            summary_split.pop(0)
        else:
            # First line(s) empty? Kill them!
            while (len(summary_split) > 0) and (len(summary_split[0].strip()) == 0): summary_split.pop(0)

        # trim end of last line
        if (len(summary_split) > 0) : summary_split[-1] = summary_split[-1].rstrip(" ")
            
        # Last line empty? Kill it!
        if (len(summary_split) > 0) and (len(summary_split[-1].strip(" ")) == 0): summary_split.pop()
        
        # look for min indentation
        min_strip = 10000
        for s in summary_split:
            if s.strip() == "": continue # Ignore blank line
            this_strip = len(s) - len(s.lstrip(" "))
            if this_strip < min_strip: min_strip = this_strip

        i = 0
        for s in summary_split:
            if i != 0 or len(summary) > 0: summary = summary + "\n" 
            summary = summary + s[min_strip:]
            i = i + 1
    else:
        # stips whitespace before and after on oneliners
        summary = summary.strip()
    
    return summary

def md5_first64(str):
    digest = hashlib.md5(str.encode("utf-8")).hexdigest()
    md5_first64_bytes = digest[:16]
    if sys.byteorder == "little":
        md5_first64_bytes = digest[14:16] + digest[12:14] + digest[10:12] + digest[8:10] + digest[6:8] + digest[4:6] + digest[2:4] + digest[0:2]
    first64 = int(md5_first64_bytes, 16)
    # Since the int() function makes a 64-bit unsigned instead of 64-bit signed, we need to convert it
    if (first64 & (1 << 63)): first64 = (-(1<<63) + (first64 & ((1<<63)-1)))
    return first64
    
def process_at_variable_lookup(var, dou, table_line, parent_table_line):
    index = table_line - 1
    
    var = var.upper()
    
    if var == "SECTION": return CURRENT_SECTION
    elif var == "UNIT": return unit_formatter(dou.name)
    elif var == "UNITTYPE": return dou.type
    elif var == "DEPENDENCY": 
        # Special case - the ADA dod file uses an uniterated MATCH(..):DEPENDENCY, which means match to all strings in the iterator
        if index < 0: 
            return dependency_formatter(u"\u00A4".join(dou.unique_dependencies))
        return dou.unique_dependencies[index]
    elif var == "DEPENDENCYBASE": return dou.dependency_base[index]
    elif var == "TABLE_LINE": return table_line
    elif var == "CLASS": return type_formatter(dou.classname)
    elif var == "NAMESPACEV": return namespace_formatter(dou.namespaces[index])
    elif var == "NAMESPACE": 
        if CURRENT_SECTION == "Parent":
            return dependencybase_formatter(PARENT_NAMESPACE)
        else:
            return namespace_formatter(".".join(dou.namespaces))
    elif var == "REVNAMESPACE": return namespace_formatter(dou.namespaces[(len(dou.namespaces) - 1) - index])
    elif var == "CLASSSUMMARY" : return dou.summary
    elif var == "BASECLASS" : 
        return dou.baseClass
        # Check for generic Object type
        #if dou.baseClass == "Object" : return dod_types[dou.baseClass].generated
        #else: return dou.baseClass
    elif var == "CREATEROUTINESUMMARY" : return dou.createRoutines[index].summary
    elif var == "CREATEROUTINE" : return member_formatter(dou.createRoutines[index].name)
    elif var == "CREATEROUTINE'LENGTH" : return get_iterator_length("CREATEROUTINE", dou, 0, 0)
    elif var == "CREATEPARAMETERTYPE" : 
        return dod_types[dou.createRoutines[parent_table_line - 1].parameters[index].type].generated
    elif var == "UNIFORM_CREATEPARAMETERTYPE" :
        return dod_types[dou.createRoutines[parent_table_line - 1].parameters[index].type].uniform_type
    elif var == "CREATEPARAMETER" :
        if parent_table_line == -1: 
            # this only occurs when checking for existance and not iterating
            return len(dou.createRoutines[index].parameters) > 0
        else:
            return member_formatter(dou.createRoutines[parent_table_line - 1].parameters[index].name)
    elif var == "CREATEPARAMETERISARRAY" : return create_parameter_is_array(dou, table_line, parent_table_line)
    elif var == "CREATEPARAMETERISLAST" : return create_parameter_is_last(dou, table_line, parent_table_line)
    elif var == "UNIFORM_CREATEVALUETYPE" : 
        member = dou.createRoutines[parent_table_line - 1].values[index].member
        m_type = dou.member_name_to_type_lookup[member]
        return dod_types[m_type].uniform_type
    elif var == "CREATEVALUEPARAMETER" :
        p1 = dou.createRoutines[parent_table_line - 1].values[index].parameter        
        return member_formatter(p1[p1.rfind(".")+1:])
    elif var == "CREATEVALUEPARAMETERCLASS" :
        p1 = dou.createRoutines[parent_table_line - 1].values[index].parameter        
        return type_formatter(p1[:p1.rfind(".")])
    elif var == "CREATEVALUEPARAMETERINDEX": 
        return dou.createRoutines[parent_table_line - 1].values[table_line - 1].index
    elif var == "CREATEVALUE" :
        return member_formatter(dou.createRoutines[parent_table_line - 1].values[index].member)
    elif var == "CREATEVALUETYPE" :
        member = dou.createRoutines[parent_table_line - 1].values[index].member
        m_type = dou.member_name_to_type_lookup[member]
        return dod_types[m_type].generated
    elif var == "CREATEVALUEISARRAY": return create_value_is_array(dou, table_line, parent_table_line)
    elif var == "CREATEVALUEPARAMETERINDEX": return create_value_parameter_index(dou, table_line, parent_table_line)        
    elif var == "MEMBER" : return member_formatter(dou.members[index].name)
    elif var == "XMLMEMBER" : return dou.members[index].name
    elif var == "MEMBER'LENGTH" : return get_iterator_length("MEMBER", dou, 0, 0)
    elif var == "MEMBERCLASS" : return classname_formatter(dou.classname)
    elif var == "MEMBERSUMMARY" : return dou.members[index].summary
    elif var == "UNIFORM_MEMBERTYPE" : return dod_types[dou.members[index].type].uniform_type
    elif var == "MEMBERTYPE" : return dod_types[dou.members[index].type].generated
    elif var == "MEMBERISSTRING" : return member_is_string(dou, table_line)
    elif var == "MEMBERISARRAY" : return member_is_array(dou, table_line)
    elif var == "PARAMETER" : return member_formatter(dou.parameters[index].name)
    elif var == "PARAMETER'LENGTH" : return get_iterator_length("PARAMETER", dou, 0, 0)
    elif var == "XMLPARAMETER" : return dou.parameters[index].name
    elif var == "PARAMETERCLASS" : return namespace_formatter(dou.classname)
    elif var == "PARAMETERSUMMARY" : return dou.parameters[index].summary
    elif var == "UNIFORM_PARAMETERTYPE" : return dod_types[dou.parameters[index].type].uniform_type
    elif var == "PARAMETERTYPE" : return dod_types[dou.parameters[index].type].generated
    elif var == "PARAMETERISARRAY" : return parameter_is_array(dou, table_line)
    elif var == "TYPEID" : 
        if dou.name == "": return ""
        return str(md5_first64(dou.name))
    elif var == "CHECKSUM" : 
        if dou.name == "": return ""
        checksum_str = dou.name
        for ev in dou.values:
            checksum_str = checksum_str + "." + ev
        return str(md5_first64(checksum_str))
    elif var == "ENUMVALUE" : 
        if index == -2:
            # This happends for single value enums in the cpp-h.dod
            return enum_formatter(dou.values[0])
        else:
            return enum_formatter(dou.values[index])
    elif var == "ENUMVALUE'LENGTH": return get_iterator_length("ENUMVALUE", dou, 0, 0)
    print("** ERROR - invalíd var lookup,", var, file=sys.stderr)
    return None

def get_iterator_length(var, dou, table_line, parent_table_line):
    # if the variable is no iterator return -1
    # if it is an empty iterator return 0
    # otherwise return length of iterator
    if var == "UNIT": return -1
    elif var == "UNITTYPE": return -1
    elif var == "DEPENDENCY": return len(dou.unique_dependencies)
    elif var == "DEPENDENCYBASE": return len(dou.dependency_base)
    elif var == "TABLE_LINE": return -1
    elif var == "CLASS": return -1
    elif var == "NAMESPACEV" or var == "REVNAMESPACE": return len(dou.namespaces)
    elif var == "CLASSSUMMARY" : return -1
    elif var == "BASECLASS" : return -1
    elif var == "CREATEROUTINE" or var == "CREATEROUTINESUMMARY": return len(dou.createRoutines)
    elif var == "CREATEPARAMETER" or var == "UNIFORM_CREATEPARAMETERTYPE" or var == "CREATEPARAMETERTYPE": 
        if len(dou.createRoutines) == 0 : return 0
        return len(dou.createRoutines[parent_table_line - 1].parameters)
    elif var == "CREATEROUTINESUMMARY": return len(dou.createRoutines)
    elif var == "CREATEVALUE": return len(dou.createRoutines[parent_table_line - 1].values)
    elif var == "MEMBER" or var == "MEMBERTYPE" or var == "MEMBERCLASS" or var == "UNIFORM_MEMBERTYPE": return len(dou.members)
    elif var == "PARAMETER" or var == "PARAMETERSUMMARY" : return len(dou.parameters)
    elif var == "ENUMVALUE" : return len(dou.values)
    
    print("** ERROR ** - bad iterator: ", var, file=sys.stderr)
    return -1

def create_parameter_is_array(dou, table_line, parent_table_line):
    if len(dou.createRoutines) == 0: return trim_false(False)
    return trim_false(len(dou.createRoutines) > 0 and len(dou.createRoutines[parent_table_line - 1].parameters) > 0 and dou.createRoutines[parent_table_line - 1].parameters[table_line - 1].arraySize is not None)
    
def create_parameter_is_last(dou, table_line, parent_table_line):
    if len(dou.createRoutines) == 0: return trim_false(False)
    return trim_false(len(dou.createRoutines[parent_table_line - 1].parameters) == table_line)

def create_value_is_array(dou, table_line, parent_table_line):
    if len(dou.createRoutines) == 0: return trim_false(False)
    if len(dou.createRoutines[parent_table_line - 1].values) == 0 : return trim_false(False)
    return trim_false(dou.createRoutines[parent_table_line - 1].values[table_line - 1].arraySize is not None or dou.createRoutines[parent_table_line - 1].values[table_line - 1].array is not None)

def create_value_parameter_index(dou, table_line, parent_table_line):
    if len(dou.createRoutines) == 0: return False
    if len(dou.createRoutines[parent_table_line - 1].values) == 0 : return False
    return dou.createRoutines[parent_table_line - 1].values[table_line - 1].index is not None
    
def parameter_is_array(dou, table_line):
    return trim_false(len(dou.parameters) > 0 and len(dou.parameters[table_line - 1].arrayElements) > 0)

def member_is_array(dou, table_line):
    return trim_false(len(dou.members) > 0 and ((dou.members[table_line - 1].arraySize is not None) or (dou.members[table_line - 1].array is not None) or dou.members[table_line - 1].arraySizeRef))
    
def member_is_string(dou, table_line):
    return trim_false(len(dou.members) > 0 and dou.members[table_line - 1].type == "String")

def trim_false(boolean):
    # The tags-txt.dod requests "IS" values outside IF clauses, and for some
    # reason the old dots_v prints True for True but empty string for False.
    if not boolean : return ""
    return True
    
def process_at_exist(at_string, dou, table_line, parent_table_line):
    exist_scrap, var = at_string.split(":",2)
    
    if var == "MEMBER": return len(dou.members) > 0
    elif var == "MEMBERISSTRING" : return member_is_string(dou, table_line)
    elif var == "MEMBERISARRAY" : return member_is_array(dou, table_line)
    elif var == "MEMBERSUMMARY": return len(dou.members) > 0 and dou.members[table_line - 1].summary is not None and len(dou.members[table_line - 1].summary) > 0
    elif var == "CLASSSUMMARY": return dou.summary is not None and len(dou.summary) > 0
    elif var == "DEPENDENCY": return len(dou.unique_dependencies) > 0
    elif var == "PARAMETER": return len(dou.parameters) > 0
    elif var == "PARAMETERSUMMARY": return len(dou.parameters) > 0 and dou.parameters[table_line - 1].summary is not None and len(dou.parameters[table_line - 1].summary) > 0
    elif var == "PARAMETERISARRAY" : return parameter_is_array(dou, table_line)
    elif var == "CREATEROUTINE": return len(dou.createRoutines) > 0
    elif var == "CREATEROUTINESUMMARY": return len(dou.createRoutines) > 0 and dou.createRoutines[table_line - 1].summary is not None and len(dou.createRoutines[table_line - 1].summary) > 0
    elif var == "CREATEPARAMETER": 
        if len(dou.createRoutines) == 0: return False
        if parent_table_line == -1: 
            # Iterating on createroutine
            return len(dou.createRoutines[table_line - 1].parameters) > 0
        else:
            return len(dou.createRoutines[parent_table_line - 1].parameters) > 0
    elif var == "CREATEPARAMETERISLAST" : return create_parameter_is_last(dou, table_line, parent_table_line)
    elif var == "CREATEPARAMETERISARRAY" : return create_parameter_is_array(dou, table_line, parent_table_line)
    elif var == "CREATEVALUE" :
        if len(dou.createRoutines) == 0: return False
        if parent_table_line == -1: 
            # Iterating on createroutine
            return len(dou.createRoutines[table_line - 1].values) > 0
        else:
            return len(dou.createRoutines[parent_table_line - 1].values) > 0
    elif var == "CREATEVALUEISARRAY": return create_value_is_array(dou, table_line, parent_table_line)
    elif var == "CREATEVALUEPARAMETERINDEX": return create_value_parameter_index(dou, table_line, parent_table_line)        
    else:
        print("process_at_exist: Missing", var, file=sys.stderr)
        return False
    
def process_at_line(line, dou, table_line, parent_table_line, strings_with_quotes):
    pre, at_string, post = extract_at_string(line)
    if len(at_string) > 0:
        # maybe there is another @ string
        post = process_at_line(post, dou, table_line, parent_table_line, strings_with_quotes)
        processed = process_at_str(at_string, dou, table_line, parent_table_line, strings_with_quotes)
        if processed is None: 
            if loglevel >= 4: print("** Warning: Lookup of nonexistent variable: ", at_string, table_line, parent_table_line, file=sys.stderr)
            processed = ""
        
        return pre + processed + post
    else:
        return line

def extract_at_string(line):
    # substitute string starts with @_ and ends with _@
    start = line.find("@_")
    stop = line.find("_@", start)
    
    if start != -1 and stop != -1 :
        return [line[:start], line[start+2:stop], line[stop+2:]]
    else:
        return ["", "", ""]

def safe_at_string_splitter(at_string):
    # Normally the @-string is splitted on colons, but there may be "escaped" colons inside parentheses
    # (these exist in MATCH expressions at the moment)
    # So, we just keep track of wheter we are inside a parentheses when splitting!
    
    parentheses_level = 0
    splitted = []
    part = ""
    
    for c in at_string:
        if c == "(": 
            parentheses_level += 1
            part += c
        elif c == ")": 
            parentheses_level -= 1
            part += c            
        elif c == ":" and parentheses_level == 0:
            splitted.append(part)
            part = ""
        else:
            part += c
            
    splitted.append(part)
    return splitted
        
def process_at_str(at_string, dou, table_line, parent_table_line, strings_with_quotes):
    result = ""
    
    if at_string.find("EXIST:") != -1 : 
        # Check for existance, treat specially
        result = process_at_exist(at_string, dou, table_line, parent_table_line)
    elif at_string.find(":") == -1:
        # Simple case, only variable lookup
        result = process_at_variable_lookup(at_string, dou, table_line, parent_table_line)
    else:
        # Variable lookup + processing
        ls = safe_at_string_splitter(at_string)
        result = process_at_variable_lookup(ls.pop(), dou, table_line, parent_table_line)
        
        while len(ls) > 0:
            command = ls.pop()
            if command == "UPPER": 
                result = result.upper()
            elif command == "LOWER": 
                result = result.lower()
            elif command == "SLICE(1..1)": 
                # Get the first character
                result = result[0]
            elif command.startswith("MATCH("):
                # Replace one occurance with regexp
                
                # ensure we only remove leading and last parentheses in case 
                # there are any in the expression
                ptn = command[command.find("(")+1:-1]
                match_result = re.match(ptn, result)
                result = match_result is not None

            elif command.startswith("REPLACE("):
                # Replace one occurance with regexp
                
                # ensure we only remove leading and last parentheses in case 
                # there are any in the expression
                rc1 = command[command.find("(")+1:-1]
                # first replace all escaped slashes with something else (¤)
                rc1 = rc1.replace("\/", u"\u00A4")
                # Then split on the non-espaced slash
                ptn, repl = rc1.split("/",1)
                # now put the slashes back, unescaped
                ptn = ptn.replace(u"\u00A4", "/")
                repl = repl.replace(u"\u00A4", "/")
                result = re.sub(ptn, repl, result, 1)
                
            elif command.startswith("REPLACE_ALL("):
                # ensure we only remove leading and last parentheses in case 
                # there are any in the expression
                rc1 = command[command.find("(")+1:-1]
                # first replace all escaped slashes with something else
                rc1 = rc1.replace("\/", u"\u00A4")
                # Then split on the non-espaced slash
                ptn, repl = rc1.split("/",1)
                # now put the slashes back, unescaped
                ptn = ptn.replace(u"\u00A4", "/")
                repl = repl.replace(u"\u00A4", "/")
                result = re.sub(ptn, repl, result)
                
            else:
                print("** ERROR - Unknown @ command", command, file=sys.stderr)
    
    if isinstance(result, (bool, int)): result = str(result)
    elif strings_with_quotes and not result.isdigit(): result = '"' + result + '"'
    return result

def get_at_string_iterator_length(line, dou, table_line, parent_table_line):
    ## Now handled before calling this function
    #if line.find("IF@@") != -1: 
        # Don't seek iterator in an IF statement
    #    return -1    
    #if (line.startswith("@@--") or line.startswith("@@ ") or line == "@@\n"):
        # Don't seek iterator in a comment
    #    return -1
    
    start = line.find("@_")
    stop = line.find("_@", start)
    if start == -1 or stop == -1:
        # No at string in this line
        return -1

    result = ""

    subcommand = line[start+2:stop]
    if subcommand.find(":") == -1:
        # Simple case, only variable lookup
        result = subcommand
    else:
        # Variable lookup + processing
        ls = subcommand.split(":")
        result = ls.pop()
    
    len = get_iterator_length(result, dou, table_line, parent_table_line)
    if (len != -1): 
        return len
    else: 
        return get_at_string_iterator_length(line[stop+2:], dou, table_line, parent_table_line)

    
def parse_table_clause(current_line, file, dou, process_table_content, parent_table_line):
    table_line = 1
    table_start = file.tell()
    iterator_len = -1

    if process_table_content: 
        # Find the iterator
        line = file.get_next_line()
        while len(line) > 0 and iterator_len == -1 and not line.startswith("@@END_TABLE@@"):
            if line[0] != '@':
                # We don't seek iterator in IF expression or in comments
                iterator_len = get_at_string_iterator_length(line, dou, table_line, parent_table_line)
            line = file.get_next_line()
    
    if (iterator_len == -1) or (iterator_len < table_line): process_table_content = 0
    file.seek(table_start)
    line = file.get_next_line()
    while len(line) > 0:
        if line[0] == '@':
            # watch out for next IF
            if line.startswith("@@IF@@"):
                parse_if_clause(line, file, dou, process_table_content, table_line, parent_table_line)
            elif line.startswith("@@TABLE@@"):
                parse_table_clause(line, file, dou, process_table_content, table_line)
            elif line.startswith("@@END_TABLE@@"):
                table_line += 1
                if iterator_len < table_line:
                    # Nothing more to iterate                
                    return
                else:
                    # One more go
                    file.seek(table_start)
            # skip comment
            #if not (line.startswith("@@--") or line.startswith("@@ ") or line == "@@\n"):            
        elif process_table_content:
            if line.find("@_") != -1:
                if iterator_len > 0:
                    res = process_at_line(line, dou, table_line, parent_table_line, False)
                    write_to_file(res)
            else:
                # Plain string, just output
                write_to_file(line)

        line = file.get_next_line()

def extract_parentheses_strings(line):
    # concatenated if string starts with ( and ends with )
    start = line.find("(")
    stop = line.find(")", start)
    p1 = ""
    logic = ""
    p2 = ""
    
    if start != -1 and stop != -1 :
        p1 = line[start+1:stop]
    
    start2 = line.find("(", stop+1)
    stop2 = line.find(")", start2)
    
    if start2 != -1 and stop2 != -1 :
        p2 = line[start2+1:stop2]
    
    if stop != -1 and start2 != -1 :
        logic = line[stop+1:start2-1].strip()
    
    return [p1, logic, p2]

def if_string_subst(line):
    # trim spaces around equal sign to simplfy
    if line.find('/=') != -1: line = line.replace('/=', '!=')
    else: line = line.replace('=', '==')
    
    line = line.replace('= ', '=')

    # Generic types
    line = line.replace('=Object', '= "Object"')
    line = line.replace('=String', '= "String"')
    line = line.replace('=Binary', '= "Binary"')
    line = line.replace('=Boolean', '= "Boolean"')
    line = line.replace('=InstanceId', '= "InstanceId"')
    line = line.replace('=HandlerId', '= "HandlerId"')
    line = line.replace('=ChannelId', '= "ChannelId"')
    line = line.replace('=EntityId', '= "EntityId"')
    line = line.replace('=Enumeration', '= "Enumeration"')
    line = line.replace('=class', '= "class"')
    line = line.replace('=exception', '= "exception"')
    line = line.replace('=enumeration', '= "enumeration"')
    line = line.replace('=property', '= "property"')
    
    # C#
    line = line.replace('=System.Single', '= "System.Single"')
    line = line.replace('=System.Double', '= "System.Double"')
    
    # java
    line = line.replace('=com.saabgroup.safir.dob.typesystem.Object', '= "com.saabgroup.safir.dob.typesystem.Object"')
    line = line.replace('=float', '= "float"')
    line = line.replace('=double', '= "double"')
    
    # ADA
    line = line.replace('=Entity_Id', '= "Entity_Id"')
    line = line.replace('=Instance_Id', '= "Instance_Id"')
    line = line.replace('=Channel_Id', '= "Channel_Id"')
    line = line.replace('=Handler_Id', '= "Handler_Id"')
    line = line.replace('=Safir.Dob.Typesystem.Object', '= "Safir.Dob.Typesystem.Object"')
    return line
    
def evaluate_if_condition(line, dou, table_line, parent_table_line):
    # substitution to allow python evaluation
    line = if_string_subst(line)
    # substitute all the @_ expressions
    line = process_at_line(line, dou, table_line, parent_table_line, True)
    # line may start with @@IF@@ or @@ELSIF@@
    start = line.find("IF@@")
    expression = line[start+4:]
    # then let python do the work!
    return eval(expression)    

def parse_parameters(line):
    valid_parameter = 0
    if line.startswith("Exception"):
        read_dod_exception(line, dod_exceptions)
        valid_parameter = 1
    elif line.startswith("Type"):
        read_dod_type(line, dod_types)
        valid_parameter = 1
    else:
        for dod_param in dod_parameter_names:
            if read_dod_parameter(line, dod_param, dod_parameters): 
                valid_parameter = 1
                break
            
    if not valid_parameter:
        print("** ERROR - Cannot parse parameter line:", line, file=sys.stderr)

    
if_level = 0        
def parse_if_clause(current_line, file, dou, process_if_content, table_line, parent_table_line):
    global if_level

    if_level += 1
    process_this_level = 0
    match_on_this_level = 0
    if process_if_content: 
        if evaluate_if_condition(current_line, dou, table_line, parent_table_line):
            process_this_level = 1
            match_on_this_level = 1
    
    while 1:
        line = file.get_next_line()
        if not line: break
        
        if line[0] == '@':
            # watch out for next IF, ELSIF or ENDIF
            if line.startswith("@@IF@@"):
                # Another IF encountered, recurse with skip flag
                parse_if_clause(line, file, dou, process_this_level, table_line, parent_table_line)
            
            elif line.startswith("@@ELSE@@"):
                # Else enountered, process it only if we did not process any previous block on this level
                if process_if_content:
                    if not match_on_this_level: 
                        process_this_level = 1
                        match_on_this_level = 1
                    else:
                        process_this_level = 0
            
            elif line.startswith("@@ELSIF@@"):
                # Else If encountered, means current if is finished and we must evaluate the expression,
                # but only if we did not process any previous block on this level
                if process_if_content:
                    if not match_on_this_level:
                        if evaluate_if_condition(line, dou, table_line, parent_table_line):
                            process_this_level = 1
                            match_on_this_level = 1
                    else:
                        process_this_level = 0
        
            elif line.startswith("@@END_IF@@"):
                # Found the end of this clause, return
                if_level -= 1
                return
            
            # Skip comment
            #elif process_this_level:
            #    if (line.startswith("@@--") or line.startswith("@@ ") or line == "@@\n"): continue
                
            elif line.startswith("@@TABLE@@"):
                parse_table_clause(line, file, dou, process_this_level, table_line)

        elif process_this_level:
            # line does not start with @

            # Special parsing for parameters section
            if CURRENT_SECTION == "Parameters": 
                parse_parameters(line)
            
            elif line.find("@_") != -1:
                res = process_at_line(line, dou, table_line, parent_table_line, False)
                write_to_file(res)
                
            else:
                # Plain string, just output
                write_to_file(line)
        # else we skip the line

def write_to_file(s):
    global CURRENT_GENERATED_FILENAME
    global CURRENT_GENERATED_FILE
    
    if CURRENT_GENERATED_FILE is None: 
        CURRENT_GENERATED_FILE = codecs.open(CURRENT_GENERATED_FILENAME, "w", encoding="utf-8") 
        if CURRENT_GENERATED_FILE is None:
            print("** ERROR - could not open output file!", CURRENT_GENERATED_FILENAME, file=sys.stderr)
            sys.exit(1)
            
    # TODO - wierd way that the original parses the .dod, decide later if we should remove this
    if s != "\n" and s.strip() == "": return 

    # Make the line endings native
    s = s.replace("\n", os.linesep)
    
    CURRENT_GENERATED_FILE.write(s)
    #CURRENT_GENERATED_FILE.write(s.encode('utf8'))
    if loglevel >=4 : print(s, end='', file=sys.stderr)
    

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

# Faster file reader, buffers all in memory (good since we loop through same file multiple times)
class FileReader(object):
    def __init__(self, filename, preprocess):
        file = codecs.open(filename, "r", encoding="utf-8")
        self.index = 0
        # reads up all the lines of the file in memory
        self.lines = file.readlines()
        file.close()
        
        # preprocess
        if preprocess :            
            eol_string = ""
            for i in range(len(self.lines)):
                # Replace the files EOL with plain \n. This is converted to native EOL style when we write the output files
                if i == 0: 
                    if self.lines[0].endswith("\r\n"): eol_string = "\r\n"
                    elif self.lines[0].endswith("\n"): eol_string = "\n"
                    elif self.lines[0].endswith("\r"): eol_string = "\r"
                    else:
                        print("** ERROR, file", filename, "has unknown EOL characters", file=sys.stderr)
                        sys.exit(1)
                
                if eol_string != "\n":
                    self.lines[i] = self.lines[i].replace(eol_string, "\n")
                
                if preprocess:
                    # TODO - wierd way that the original parses the .dod, decide later if we should remove this
                    # All lines with spaces before the newline marker are skipped in the output by the old parser...
                    line = self.lines[i]
                    if line != "\n" and line.strip() == "": continue
        
                    # Removed trailing whitespaces, we add the linebreak again to distinguish this from EOF
                    self.lines[i] = line.rstrip() + "\n"
    
    def tell(self):
        return self.index

    def seek(self, index):
        self.index = index
        
    def get_next_line(self):
        if self.index < len(self.lines): 
            self.index += 1
            if loglevel >= 5: print(self.lines[self.index-1], end='')
            return self.lines[self.index-1]
        return ""
            
#def get_next_line(dou_file):
#    line = dou_file.readline()
#    if not line: 
        # EOF
#        return ""
    
    # TODO - wierd way that the original parses the .dod, decide later if we should remove this
    # All lines with spaces before the newline marker are skipped in the output by the old parser...
#    if line != "\n" and line.strip() == "": return line
        
    # Removed trailing whitespaces, we add the linebreak again to distinguish this from EOF
#    return line.rstrip() + "\n"

def parse_dod(dod_file, dou):
    while 1:
        line = dod_file.get_next_line()
        if not line: break
            
        # Check for @@IF@@
        if line.startswith("@@IF@@"): parse_if_clause(line, dod_file, dou, 1, -1, -1)

        # Skip everything else at this top level
        # Skip comment lines
        #if line[0] == '@' and (line.startswith("@@--") or line.startswith("@@ ") or line == "@@\n"): continue
        
def dod_init(dod_filename):
    global CURRENT_SECTION
    global dod_parameters
    global dod_exceptions
    global dod_types
    dod_parameters.clear()
    dod_exceptions.clear()
    dod_types.clear()

    dod_file = FileReader(dod_filename, True)
    CURRENT_SECTION = "Parameters"
    
    parse_dod(dod_file, None)
    if not are_dod_parameters_complete(dod_parameters, dod_parameter_names):
        print("** ERROR - missing mandatory parameters, check ", dod_file, file=sys.stderr)
        sys.exit(1)
    
    dod_file.seek(0)
    return dod_file
    
def generator_main(dod_file, dou_filename, gen_src_output_path):
    ##global dod_parameter_names
    global CURRENT_GENERATED_FILENAME
    global CURRENT_GENERATED_FILE
    global CURRENT_SECTION
    global PARENT_NAMESPACE
    
    parent_namespaces_completed = []
    
    dou = parse_dou(dou_filename)

    # Parameters section is already handled before generator_main is called
    sections = ["Parent", "Code"]
    
    # One pass through the file per section
    for section in sections:
        CURRENT_SECTION = section
        dod_file.seek(0)
        
        if section == "Parent":
            # Generate a "Parent" file for each pass (only used for ADA and tags)
            PARENT_NAMESPACE = ""
            for n in dou.namespaces:
                if len(PARENT_NAMESPACE) > 0: PARENT_NAMESPACE += "."
                PARENT_NAMESPACE += n
                if PARENT_NAMESPACE in parent_namespaces_completed: 
                    # This file has already been generated (for this dod)
                    continue
                    
                empty_dou = Dou()
                output_path = gen_src_output_path + dod_parameters["Output_Directory"]
                parent_name = filename_formatter(PARENT_NAMESPACE) + dod_parameters["File_Suffix"]
                CURRENT_GENERATED_FILENAME = os.path.join(output_path, parent_name)
                mkdir(output_path)

                parse_dod(dod_file, empty_dou)
                dod_file.seek(0)
                if CURRENT_GENERATED_FILE is not None:
                    # Write final newline and close
                    CURRENT_GENERATED_FILE.write(os.linesep)
                    CURRENT_GENERATED_FILE.close()
                    CURRENT_GENERATED_FILE = None
                
                parent_namespaces_completed.append(PARENT_NAMESPACE)
    
        else: #Code
            output_path = gen_src_output_path + dod_parameters["Output_Directory"]
            
            # Namespace prefix files? (for java)
            namespaces = ".".join(dou.namespaces)
            if dod_parameters['Namespace_Prefix_File_Suffix'] != "":
                namespace_prefix_init()
                namespaces = namespace_prefixer(namespaces)
                dou.namespaces = namespaces.split(".")

            if dod_parameters["Filename_Separator"] == "/":
                output_path += directory_name_formatter(namespaces) + os.sep
                filename = filename_formatter(dou.classname) + dod_parameters["File_Suffix"]
            else:
                filename = filename_formatter(namespaces) + dod_parameters["Filename_Separator"] + filename_formatter(dou.classname) + dod_parameters["File_Suffix"]
                
            mkdir(output_path)
            CURRENT_GENERATED_FILENAME = os.path.join(output_path, filename)
            
            parse_dod(dod_file, dou)
            if CURRENT_GENERATED_FILE is not None:
                # Write final newline and close
                CURRENT_GENERATED_FILE.write(os.linesep)
                CURRENT_GENERATED_FILE.close()
                CURRENT_GENERATED_FILE = None
                
    # end for
    
    
def main():
    global dou_file_root
    global loglevel

    # Argument parsing is Python version dependent
    use_argparse = False
    if sys.version_info[0] == 2 and sys.version_info[1] >= 7:
        # Was added in 2.7
        use_argparse = True
    elif sys.version_info[0] <= 2 and sys.version_info[1] < 6:
        print("Update your Python version, 2.6 or higher required by this module", file=sys.stderr)
        sys.exit(1)
    elif sys.version_info[0] == 3 and sys.version_info[1] >= 2:
        # Was added in 3.2, all 3.x version support optparse
        use_argparse = True
    
    arguments = None
    if use_argparse:
        import argparse

        parser = argparse.ArgumentParser(description='Source code generator tool for Safir SDK Core. Processes .dou files into source code for all supported languages. Files are generated in language specific subdirectories of root path of processed dou files.')
    
        parser.add_argument('dou_files', metavar='DOU_FILE(S)', help='.dou file(s) to process. Accepts wildcards (*). If a directory is specified, all .dou files in the directory are processed (recursive)')
        parser.add_argument('-dod', '--dod', '--dod_files', dest='dod_files', metavar='DOD_FILE(S)', required=True, help='Specifies .dod files to use as templates for the processing. Accepts wildcards (*). If it is a directory, all .dod files in that directory are used.')
        parser.add_argument('-xdir', '--xdir', '--dou_root', dest='dou_root', metavar='DOU_ROOT', required=True, help='Path to the top directory containing the .dou files')
        parser.add_argument('-o', '--output_path', dest='output_path', metavar='OUTPUT_PATH', required=False, default='', help='Directory where the generated file structure starts. Defaults to current working directory.')
        parser.add_argument('-i', '-info', '--info', '--show_files', dest='show_files', required=False, default=False, action='store_true', help='Prints out the dod and dou filenames for each parsing')
        parser.add_argument('-v', '-verbose', '--verbose', '--show_parsing', dest='show_parsing', required=False, default=False, action='store_true', help='Prints debugging info from the parsing to stderr')
    
        arguments = parser.parse_args()
    else:
        import optparse
        ## Preparse option names, optparse does not support single dash options with long names
        ## This is only supported because the old dots_v supports it
        for i in range(len(sys.argv)):
            if i == 0: continue
            if sys.argv[i] == "-dod" or sys.argv[i].startswith("-dod="): sys.argv[i] = sys.argv[i].replace("-dod", "--dod")            
            if sys.argv[i] == "-xdir" or sys.argv[i].startswith("-xdir="): sys.argv[i] = sys.argv[i].replace("-xdir", "--xdir")
            if sys.argv[i] == "-info" : sys.argv[i] = "--info"
            if sys.argv[i] == "-verbose" : sys.argv[i] = "--verbose"
        
        parser = optparse.OptionParser(usage='usage: %prog [options] DOU_FILE(S)', description='Source code generator tool for Safir SDK Core. Processes .dou files into source code for all supported languages. Files are generated in language specific subdirectories of root path of processed dou files. DOU_FILE(S): .dou file(s) to process. Accepts wildcards (*). If a directory is specified, all .dou files in the directory are processed (recursive)')
    
        parser.add_option('--dod', '--dod_files', dest='dod_files', metavar='DOD_FILE(S)', default='-', help='Specifies .dod files to use as templates for the processing. Accepts wildcards (*). If it is a directory, all .dod files in that directory are used.')
        parser.add_option('--xdir', '--dou_root', dest='dou_root', metavar='DOU_ROOT', default='-', help='Path to the top directory containing the .dou files')
        parser.add_option('--output_path', '-o', dest='output_path', metavar='OUTPUT_PATH', default='', help='Directory where the generated file structure starts. Defaults to current working directory.')
        parser.add_option( '-i', '--info', '--show_files', dest='show_files', default=False, action='store_true', help='Prints out the dod and dou filenames for each parsing')
        parser.add_option('-v', '--verbose', '--show_parsing', dest='show_parsing', default=False, action='store_true', help='Prints debugging info from the parsing to stderr')
    
        ## Check existense
    
        (arguments, args) = parser.parse_args()
        if len(args) == 0: arguments.dou_files
        else: arguments.dou_files = args[0]

    dod_files = []
    if os.path.isdir(arguments.dod_files):
        normalized_path = os.path.abspath(arguments.dod_files) + os.sep    
        for dod_file in os.listdir(normalized_path + "."):
            if dod_file.endswith(".dod"):
                dod_files.append(normalized_path + dod_file)
    elif os.path.isfile(arguments.dod_files):
        dod_files.append(arguments.dod_files)
    elif arguments.dod_files.find("*") != -1:
        # Has wildcard
        dod_files = glob(arguments.dod_files)
    
    if len(dod_files) == 0 or not os.path.isfile(dod_files[0]):
        print("Invalid argument for dod files.", file=sys.stderr)
        sys.exit(1)
    
    dou_file_root = os.path.abspath(arguments.dou_root) + os.sep
    if not os.path.isdir(dou_file_root):
        print("Invalid argument for dou root.", file=sys.stderr)
        sys.exit(1)

    gen_src_output_path = arguments.output_path
    if gen_src_output_path == "":
        gen_src_output_path = os.getcwd()
    else:
        if not os.path.isdir(gen_src_output_path):
            mkdir(gen_src_output_path)
            if not os.path.isdir(gen_src_output_path):
                print("Invalid argument for output path.", file=sys.stderr)
                sys.exit(1)
    gen_src_output_path = os.path.abspath(gen_src_output_path) + os.sep
    
        
    dou_files = []    
    if os.path.isdir(arguments.dou_files):
        for path, dirs, files in os.walk(arguments.dou_files): # Walk directory tree
            for file in files:
                if file.endswith(".dou"):
                    dou_files.append(path + os.sep + file)
    elif os.path.isfile(arguments.dou_files):
        path, file = os.path.split(arguments.dou_files)
        dou_files.append(arguments.dou_files)
    elif arguments.dou_files.find("*") != -1:
        # Has wildcard
        path, file = os.path.split(arguments.dou_files)
        dou_files = glob(arguments.dou_files)
    
    if len(dou_files) == 0:
        print("No valid dou files to process.", file=sys.stderr)
        sys.exit(1)

    if arguments.show_parsing: loglevel = 4
    
    # Prepare dou uniform lookup table
    dou_uniform_lookup_init()
    
    for dod_filename in dod_files:
        dod_file = dod_init(dod_filename)
        ## Execute in threads?
        for dou_file in dou_files:
            if arguments.show_files: print(">>Processing", dod_filename, "  ", dou_file, file=sys.stderr)
            generator_main(dod_file, dou_file, gen_src_output_path)
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
