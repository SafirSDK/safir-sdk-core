#!/usr/bin/env python
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2005-2014 (http://safir.sourceforge.net)
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
import sys, os, re, hashlib, subprocess
import xml.etree.ElementTree as ET
from glob import glob
import codecs
import argparse

try:
    import ConfigParser
except ImportError:
    import configparser as ConfigParser

try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO

## Fix for unicode cross compatibility
if sys.version < '3':
    import codecs
    def u(x):
        return codecs.unicode_escape_decode(x)[0]
else:
    def u(x):
        return x

dod_parameter_names = [\
        "File_Suffix", "Filename_Separator", "Output_Directory", "Namespace_Separator", \
        "Namespace_Prefix_File_Suffix", "Parent_Filename", "Namespace_Underscore_Style", "Filename_Underscore_Style", \
        "Classname_Underscore_Style", "Membername_Underscore_Style", "Enum_Value_Underscore_Style", "Namespace_Case_Style", \
        "Filename_Case_Style", "Classname_Case_Style", "Membername_Case_Style", "Enum_Value_Case_Style", \
        "Object_Type", "Index_Type" \
        ]

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
    def __init__(self, summary, name, type, maxLength, array, arraySize, arraySizeRef, sequence, dictionary_type):
        self.summary = summary
        self.name = name
        self.type = type
        self.maxLength = maxLength
        self.array = array
        self.arraySize = arraySize
        self.arraySizeRef = arraySizeRef
        self.sequence = sequence
        self.dictionary_type = dictionary_type

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

    def __init__(self, member, member_type, parameter, inline, parameter_index, array, arraySize):
        self.member = member
        self.member_type = member_type
        self.parameter = parameter
        self.inline = inline
        self.parameter_index = parameter_index
        self.array = array
        self.arraySize = arraySize

class DouParameter(object):
    def __init__(self, summary, name, type, array, dictionary_type):
        self.summary = summary
        self.name = name
        self.type = type
        self.array = array
        self.dictionary_type = dictionary_type

def readTextPropery(xml_root, element):
    if (element.find("/") != -1):
        # Has subtags, prefix all of them
        element = element.replace("/", "/{urn:safir-dots-unit}");

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

def dou_uniform_lookup_add(dou_uniform_lookup_cache,
                           dou_file_lookup_cache,
                           dou_xml_lookup_cache,
                           file,
                           path):
    if file in dou_file_lookup_cache:
        if os.path.join(path,file) == dou_file_lookup_cache[file]:
            #we've already got this dou file
            return
        print ("Duplicate dou file", os.path.join(path,file), file=sys.stderr)
        sys.exit(1)

    dou_xml = ET.parse(os.path.join(path, file))
    xml_root = dou_xml.getroot()
    dou_type = xml_root.tag.split("}")[1]
    # Make first char uppercase
    dou_type = dou_uniform_translate(dou_type)
    dou_uniform_lookup_cache[os.path.splitext(file)[0]] = dou_type
    dou_file_lookup_cache[file] = os.path.join(path, file)
    dou_xml_lookup_cache[os.path.join(path, file)] = dou_xml


def dou_uniform_lookup_init(dou_uniform_lookup_cache,
                            dou_file_lookup_cache,
                            dou_xml_lookup_cache,
                            dependency_paths,
                            dou_files):
    for dep in dependency_paths:
        for path, dirs, files in os.walk(dep): # Walk directory tree
            for file in files:
                if file.endswith(".dou"):
                    dou_uniform_lookup_add(dou_uniform_lookup_cache,
                                           dou_file_lookup_cache,
                                           dou_xml_lookup_cache,
                                           file,
                                           path)
    for dou_file in dou_files:
        (path, file) = os.path.split(dou_file)
        dou_uniform_lookup_add(dou_uniform_lookup_cache,
                               dou_file_lookup_cache,
                               dou_xml_lookup_cache,
                               file,
                               path)

def dou_uniform_lookup(gSession, typename):
    if typename in gSession.dou_uniform_lookup_cache: return gSession.dou_uniform_lookup_cache[typename]
    print("** ERROR - Cannot match dou type", typename, file=sys.stderr)
    sys.exit(1)

def parse_namespace_file(namespace_prefixes, path, file, file_suffix):
    prefix_file = codecs.open(os.path.join(path, file), "r", encoding="utf-8")
    found = False
    line = prefix_file.readline()
    while len(line) > 0 and not found:
        if not line.startswith("#") and not line.startswith("--") and line.rstrip() != "":
            found = True
            namespace = file[:file.find(file_suffix)]
            if namespace not in namespace_prefixes:
                namespace_prefixes[namespace] = line.rstrip()
            elif namespace_prefixes[namespace] != line.rstrip():
                print("Conflicting namespace prefix definition found.", namespace_prefixes[namespace], "is different from", line.rstrip(), file=sys.stderr)
                sys.exit(1)
        line = prefix_file.readline()

# We only need to call this once per dod file (if Namespace prefixes are used)
# since it does not differ between dou files
def namespace_prefix_init(gSession):

    file_suffix = gSession.dod_parameters['Namespace_Prefix_File_Suffix']
    if file_suffix == "" : return
    if len(gSession.namespace_prefixes) != 0 and \
            (gSession.namespace_prefixes["¤¤Namespace_Prefix_File_Suffix¤¤"] == \
            file_suffix):
        # The dict has already been initialized for this file suffix, leave it
        return

    gSession.namespace_prefixes.clear()
    gSession.namespace_prefixes["¤¤Namespace_Prefix_File_Suffix¤¤"] = file_suffix

    for dep in gSession.dependency_paths:
        for path, dirs, files in os.walk(dep): # Walk directory tree
            for file in files:
                if file.endswith(file_suffix):
                    parse_namespace_file(gSession.namespace_prefixes,
                                         path,
                                         file,
                                         file_suffix)
    for prefix_file in gSession.namespace_prefix_files:
        if prefix_file.endswith(file_suffix):
            (path,file) = os.path.split(prefix_file)
            parse_namespace_file(gSession.namespace_prefixes,
                                 path,
                                 file,
                                 file_suffix)

def namespace_prefixer(gSession, typename):
    # global namespace_prefixes   Read Only
    bestmatch_prefix = ""

    if gSession.dod_parameters['Namespace_Prefix_File_Suffix'] == "": return typename

    ts = typename.split(".")
    if gSession.namespace_prefixes is not None:
        # check for namespace prefix files, best match is longest match
        ns = ""
        for n in ts:
            if len(ns) == 0: ns = n
            else: ns = ns + "." + n

            if ns in gSession.namespace_prefixes:
                bestmatch_prefix = gSession.namespace_prefixes[ns]

    if bestmatch_prefix == "": return typename
    return bestmatch_prefix + "." + typename

seek_member_in_base_class_cache = {}
def seek_member_in_base_class(gSession, seek_name, baseClass):
    if baseClass is None:
        return None

    if (seek_name, baseClass) in seek_member_in_base_class_cache:
        return seek_member_in_base_class_cache[(seek_name, baseClass)]

    parent = None
    baseClassFile = baseClass + ".dou"

    if baseClassFile in gSession.dou_file_lookup_cache:
        dou_xml = gSession.dou_xml_lookup_cache[gSession.dou_file_lookup_cache[baseClassFile]]
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
                                m.find("{urn:safir-dots-unit}arraySizeRef") is not None, False, "")
                    seek_member_in_base_class_cache[(seek_name, baseClass)] = found_member
                    return found_member
    else:
        print("** Unexpected, baseclassfile not found", baseClassFile, file=sys.stderr)
    # No match, try next level
    return seek_member_in_base_class(gSession, seek_name, parent)

def parse_dou(gSession, dou_xmlfile):
    parsed = Dou()
    dou_xml = gSession.dou_xml_lookup_cache[dou_xmlfile] #ET.parse(dou_xmlfile)
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

    parsed.baseClass = readTextPropery(xml_root, "baseClass")
    parsed.baseClassOrg = parsed.baseClass

    if not parsed.baseClass is None:
        # Check for exceptions
        if parsed.baseClass in gSession.dod_exceptions:
            baseClass = parsed.baseClass
            parsed.baseClass = gSession.dod_exceptions[baseClass].generated
            parsed.unique_dependencies.append(dependency_formatter(gSession, gSession.dod_exceptions[baseClass].dependency))
        elif parsed.baseClass in gSession.dod_types:
            baseClass = parsed.baseClass
            parsed.baseClass = gSession.dod_types[baseClass].generated
            parsed.unique_dependencies.append(gSession.dod_types[baseClass].dependency)
        else:
            parsed.baseClass = type_formatter(gSession, parsed.baseClass)
            parsed.unique_dependencies.append(dependency_formatter(gSession, parsed.baseClass))

    parsed.summary = summary_formatter(readTextPropery(xml_root, "summary"))

    m_type = parsed.name
    gSession.dod_types[m_type] = DodType(m_type, m_type, dou_uniform_translate(parsed.type), type_formatter(gSession, m_type), dependency_formatter(gSession, m_type))

    if parsed.type == "property":
        # For unknown reasons, the old dots_v adds a dependency to Object for all property dous
        parsed.unique_dependencies.append(type_formatter(gSession, gSession.dod_types["Object"].dependency))

    member_name_to_type_lookup = {}
    member_name_to_is_array_lookup = {}

    members = xml_root.find("{urn:safir-dots-unit}members")
    if members is not None:
        for m in members:
            m_type = readTextPropery(m, "type")
            m_name = readTextPropery(m, "name")
            m_array = readTextPropery(m, "array")
            m_arraySize = readTextPropery(m, "arraySize")
            m_arraySizeRef = m.find("{urn:safir-dots-unit}arraySizeRef") is not None
            m_sequence = readTextPropery(m, "sequence") is not None
            m_dictionary = None
            is_dict = m.find("{urn:safir-dots-unit}dictionary")
            if is_dict is not None:
              m_dictionary = is_dict.attrib["keyType"]

            parsed.members.append( DouMember( summary_formatter(readTextPropery(m, "summary")), \
                                        m_name, \
                                        m_type, \
                                        readTextPropery(m, "maxLength"), \
                                        m_array, \
                                        m_arraySize,
                                        m_arraySizeRef,
                                        m_sequence,
                                        m_dictionary) )
            member_name_to_type_lookup[m_name] = m_type
            member_name_to_is_array_lookup[m_name] = (m_arraySize is not None or m_array is not None or m_arraySizeRef)

            if not (m_type in gSession.dod_types):
                # This is a dou defined object
                uniform_type = dou_uniform_lookup(gSession, m_type)
                gSession.dod_types[m_type] = DodType(m_type, m_type, uniform_type, type_formatter(gSession, m_type), dependency_formatter(gSession, m_type))

                parsed.unique_dependencies.append(dependency_formatter(gSession, m_type))
            elif len(gSession.dod_types[m_type].dependency) > 0:
                parsed.unique_dependencies.append(gSession.dod_types[m_type].dependency)

            if (m_type == "String" or member_name_to_is_array_lookup[m_name]):
                parsed.unique_dependencies.append(gSession.dod_types[gSession.dod_parameters["Index_Type"]].dependency)

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
                            found_member = seek_member_in_base_class(gSession, p.text, parsed.baseClassOrg)
                            if found_member is not None:
                                parameters.append(DouCreateRoutineParameter( summary_formatter(found_member.summary), \
                                                                found_member.name, \
                                                                found_member.type, \
                                                                found_member.maxLength, \
                                                                found_member.arraySize) )
                                member_name_to_type_lookup[found_member.name] = found_member.type
                                member_name_to_is_array_lookup[found_member.name] = (found_member.arraySize is not None or found_member.array is not None or found_member.arraySizeRef)

                                if not found_member.type in gSession.dod_types:
                                    gSession.dod_types[found_member.type] = DodType(found_member.type, \
                                                                        found_member.type, \
                                                                        dou_uniform_lookup(gSession, found_member.type), \
                                                                        type_formatter(gSession, found_member.type), \
                                                                        dependency_formatter(gSession, found_member.type))
                                    parsed.unique_dependencies.append(dependency_formatter(gSession, found_member.type))
                                elif len(gSession.dod_types[found_member.type].dependency) > 0:
                                    parsed.unique_dependencies.append(gSession.dod_types[found_member.type].dependency)
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
                            if not (m_type in dod_types):
                                # This is a dou defined object
                                uniform_type = dou_uniform_lookup(gSession, m_type)
                                gSession.dod_types[m_type] = DodType(m_type, m_type, uniform_type, type_formatter(gSession, m_type), dependency_formatter(gSession, m_type))
                                parsed.unique_dependencies.append(dependency_formatter(gSession, m_type))
                            elif len(gSession.dod_types[m_type].dependency) > 0:
                                parsed.unique_dependencies.append(gSession.dod_types[m_type].dependency)

                        if (m_type == "String" or m_arraySize is not None):
                            parsed.unique_dependencies.append(gSession.dod_types[gSession.dod_parameters["Index_Type"]].dependency)


                    else:
                        print("** ERROR unknown CreateRoutineParameter subtag", parameter_tag, file=sys.stderr)
                        sys.exit(1)

            values = []
            vs = cr.find("{urn:safir-dots-unit}values")
            if vs is not None:
                for v in vs:
                    m_member = readTextPropery(v, "member")
                    m_type = member_name_to_type_lookup[m_member]
                    m_array = None
                    if member_name_to_is_array_lookup[m_member]: m_array = True

                    m_parameter = ""
                    m_p_index = None
                    m_p_inline = False

                    #m_value = readTextPropery(v, "value")
                    m_parameter_new = readTextPropery(v, "parameter/name")
                    m_parameter_old = readTextPropery(v, "parameter")
                    if m_parameter_new is not None:
                        # New <parameter> syntax!
                        m_parameter = m_parameter_new
                        m_p_index = readTextPropery(v, "parameter/index")
                    elif m_parameter_old is not None:
                        # Old <parameter> syntax!
                        m_parameter = m_parameter_old
                        m_p_index = readTextPropery(v, "index")
                    else:
                        # New syntax, direct parameter from dots_internal
                        # This parameter shall be generated inline to hide it from external use
                        # Parameter name: member@signature -> MyClassMember@MyNamespace.MyClass.MyCreateRoutine#param1#...#paramN
                        m_p_inline = True

                        m_parameter = m_member + "@" + parsed.name + "." + readTextPropery(cr, "name")
                        for cr_member in parameters:
                            m_parameter += "#" + cr_member.name

                    values.append( DouCreateRoutineValue( m_member, \
                                                            m_type, \
                                                            m_parameter, \
                                                            m_p_inline, \
                                                            m_p_index, \
                                                            m_array, \
                                                            readTextPropery(v, "arraySize") ) )
                    parameter_class = m_parameter[:m_parameter.rfind(".")]
                    if not m_p_inline and parameter_class != parsed.name:
                        parsed.unique_dependencies.append(dependency_formatter(gSession, parameter_class))

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
            m_type = readTextPropery(p, "type")
            is_array = readTextPropery(p, "arrayElements") is not None or readTextPropery(p, "array") is not None
            is_dict = p.find("{urn:safir-dots-unit}dictionary")
            dict_type = None
            if is_dict is not None:
              dict_type = is_dict.attrib["keyType"]

            parsed.parameters.append( DouParameter( summary_formatter(readTextPropery(p, "summary")), \
                                        readTextPropery(p, "name"), \
                                        m_type, \
                                        is_array, dict_type) )

            if is_array:
                parsed.unique_dependencies.append(gSession.dod_types[gSession.dod_parameters["Index_Type"]].dependency)

            if not (m_type in gSession.dod_types):
                # This is a dou defined object
                uniform_type = dou_uniform_lookup(gSession, m_type)
                gSession.dod_types[m_type] = DodType(m_type, m_type, uniform_type, type_formatter(gSession,
                    m_type), dependency_formatter(gSession, m_type))
                parsed.unique_dependencies.append(dependency_formatter(gSession, m_type))
            elif len(gSession.dod_types[m_type].dependency) > 0:
                parsed.unique_dependencies.append(gSession.dod_types[m_type].dependency)

    # Remove duplicates from dependencies and membertypes
    parsed.unique_dependencies = filter_duplicates(parsed.unique_dependencies)
    # Sort dependencies by path
    parsed.unique_dependencies.sort()
    # For some reason, the "ValueContainers" dependency is put last by the ADA dots_v, so do the same here
    vc_dependency = gSession.dod_types[gSession.dod_parameters["Index_Type"]].dependency
    if vc_dependency in parsed.unique_dependencies:
        parsed.unique_dependencies.remove(vc_dependency)
        parsed.unique_dependencies.append(vc_dependency)

    # remove empty dependency
    empty_dependency = ""
    if empty_dependency in parsed.unique_dependencies:
        parsed.unique_dependencies.remove(empty_dependency)

    # remove recursive dependencies
    if gSession.dod_types[parsed.name].dependency in parsed.unique_dependencies:
        parsed.unique_dependencies.remove(gSession.dod_types[parsed.name].dependency)

    parsed.member_name_to_type_lookup = member_name_to_type_lookup

    # list all base classes for dependencies
    for dep in parsed.unique_dependencies:
        if dep.find(".") != -1:
            dep_base = dep[:dep.rfind(".")]
            if not dep_base in parsed.dependency_base:
                parsed.dependency_base.append(dep_base)
        elif dep.find(gSession.dod_parameters["Namespace_Separator"]) != -1:
            dep_base = dep[:dep.rfind(gSession.dod_parameters["Namespace_Separator"])]
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
        s1 = re.sub(u("([^\._:\u00A4])([A-Z][a-z]+)"), r'\1_\2', name)
        s2 = re.sub('([a-z])([A-Z])', r'\1_\2', s1)
        s3 = re.sub('([0-9])([A-Za-z])', r'\1_\2', s2)
        s4 = re.sub('([A-Za-z])([0-9])', r'\1_\2', s3)
        return s4
    else:
        print("** ERROR, unsupported underscore format", style, file=sys.stderr)
        sys.exit(1)

def case_formatter(name, style):
    # *_Case_Style is one of: "Upper", "Lower", "Camel", "Keep"
    if style == "Keep": return name
    elif style == "Upper": return name.upper()
    elif style == "Lower": return name.lower()
    else:
        print("** ERROR, unsupported case format", style, file=sys.stderr)
        sys.exit(1)

def separator_formatter(gSession, name, style):
    if style != gSession.dod_parameters["Namespace_Separator"]:
        name = name.replace(gSession.dod_parameters["Namespace_Separator"], style)

    return name.replace(".", style)

def directory_name_formatter(gSession, directory_name):
    directory_name = underscore_formatter(directory_name, gSession.dod_parameters["Filename_Underscore_Style"])
    directory_name = separator_formatter(gSession, directory_name, gSession.dod_parameters["Filename_Separator"])
    directory_name = case_formatter(directory_name, gSession.dod_parameters["Namespace_Case_Style"])
    return directory_name

def filename_formatter(gSession, filename):
    filename = underscore_formatter(filename, gSession.dod_parameters["Filename_Underscore_Style"])
    filename = separator_formatter(gSession, filename, gSession.dod_parameters["Filename_Separator"])
    filename = case_formatter(filename, gSession. dod_parameters["Filename_Case_Style"])
    return filename

def member_formatter(gSession, member):
    member = underscore_formatter(member, gSession.dod_parameters["Membername_Underscore_Style"])
    member = case_formatter(member, gSession.dod_parameters["Membername_Case_Style"])
    return member

def classname_formatter(gSession, classname):
    classname = underscore_formatter(classname, gSession.dod_parameters["Classname_Underscore_Style"])
    classname = case_formatter(classname, gSession.dod_parameters["Classname_Case_Style"])
    return classname

def namespace_formatter(gSession, namespace):
    if namespace is None: return namespace

    namespace = underscore_formatter(namespace, gSession.dod_parameters["Namespace_Underscore_Style"])
    namespace = separator_formatter(gSession, namespace, gSession.dod_parameters["Namespace_Separator"])
    namespace = case_formatter(namespace, gSession.dod_parameters["Namespace_Case_Style"])
    return namespace

def type_formatter(gSession, type):
    if type is None: return type

    if type.find(".") != -1:
        parts = type.split(".")
        classname = parts.pop()
        namespace = namespace_prefixer(gSession, ".".join(parts))
        return namespace_formatter(gSession, namespace) + gSession.dod_parameters["Namespace_Separator"] + classname_formatter(gSession, classname)
    elif type.find(gSession.dod_parameters["Namespace_Separator"]) != -1:
        parts = type.split(gSession.dod_parameters["Namespace_Separator"])
        classname = parts.pop()
        namespace = namespace_prefixer(gSession, ".".join(parts))
        return namespace_formatter(gSession, namespace) + gSession.dod_parameters["Namespace_Separator"] + classname_formatter(gSession, classname)
    else:
        # No namespace to format
        return classname_formatter(gSession, type)


def unit_formatter(gSession, unit):
    return dependency_formatter(gSession, unit)

def enum_formatter(gSession, enum):
    enum = underscore_formatter(enum, gSession.dod_parameters["Enum_Value_Underscore_Style"])
    enum = case_formatter(enum, gSession.dod_parameters["Enum_Value_Case_Style"])
    return enum

def dependency_formatter(gSession, dependency):
    return type_formatter(gSession, dependency).replace(gSession.dod_parameters["Namespace_Separator"], ".")

def dependencybase_formatter(gSession, dependency):
    return namespace_formatter(gSession, dependency).replace(gSession.dod_parameters["Namespace_Separator"], ".")

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

def process_at_variable_lookup(gSession, var, dou, table_line, parent_table_line):
    index = table_line - 1

    var = var.upper()

    if var == "SECTION": return gSession.current_section
    elif var == "UNIT": return unit_formatter(gSession, dou.name)
    elif var == "UNITTYPE": return dou.type
    elif var == "DEPENDENCY":
        # Special case - the ADA dod file uses an uniterated MATCH(..):DEPENDENCY, which means match to all strings in the iterator
        if index < 0:
            return dependency_formatter(gSession, u("\u00A4").join(dou.unique_dependencies))
        return dou.unique_dependencies[index]
    elif var == "DEPENDENCYBASE": return dou.dependency_base[index]
    elif var == "TABLE_LINE": return table_line
    elif var == "LIBRARY_NAME": return gSession.library_name
    elif var == "CLASS": return type_formatter(gSession, dou.classname)
    elif var == "NAMESPACEV": return namespace_formatter(gSession, dou.namespaces[index])
    elif var == "NAMESPACE":
        if gSession.current_section == "Parent":
            return dependencybase_formatter(gSession, gSession.parent_namespace)
        else:
            return namespace_formatter(gSession, ".".join(dou.namespaces))
    elif var == "REVNAMESPACE": return namespace_formatter(gSession, dou.namespaces[(len(dou.namespaces) - 1) - index])
    elif var == "CLASSSUMMARY" : return dou.summary
    elif var == "BASECLASS" : return dou.baseClass
    elif var == "CREATEROUTINESUMMARY" : return dou.createRoutines[index].summary
    elif var == "CREATEROUTINE" : return member_formatter(gSession, dou.createRoutines[index].name)
    elif var == "CREATEROUTINE'LENGTH" : return get_iterator_length("CREATEROUTINE", dou, 0, 0)
    elif var == "CREATEPARAMETERTYPE" :
        return gSession.dod_types[dou.createRoutines[parent_table_line - 1].parameters[index].type].generated
    elif var == "UNIFORM_CREATEPARAMETERTYPE" :
        return gSession.dod_types[dou.createRoutines[parent_table_line - 1].parameters[index].type].uniform_type
    elif var == "CREATEPARAMETER" :
        if parent_table_line == -1:
            # this only occurs when checking for existance and not iterating
            return len(dou.createRoutines[index].parameters) > 0
        else:
            return member_formatter(gSession, dou.createRoutines[parent_table_line - 1].parameters[index].name)
    elif var == "CREATEPARAMETERISARRAY" : return create_parameter_is_array(dou, table_line, parent_table_line)
    elif var == "CREATEPARAMETERISLAST" : return create_parameter_is_last(dou, table_line, parent_table_line)
    elif var == "UNIFORM_CREATEVALUETYPE" :
        member = dou.createRoutines[parent_table_line - 1].values[index].member
        m_type = dou.member_name_to_type_lookup[member]
        return gSession.dod_types[m_type].uniform_type
    elif var == "CREATEVALUEPARAMETER" :
        p1 = dou.createRoutines[parent_table_line - 1].values[index].parameter
        return member_formatter(gSession, p1[p1.rfind(".")+1:])
    elif var == "CREATEVALUEPARAMETERRAW" :
        p1 = dou.createRoutines[parent_table_line - 1].values[index].parameter
        return p1;
    elif var == "CREATEVALUEPARAMETERCLASS" :
        p1 = dou.createRoutines[parent_table_line - 1].values[index].parameter
        return type_formatter(gSession, p1[:p1.rfind(".")])
    elif var == "CREATEVALUEMEMBERTYPE" :
        return gSession.dod_types[dou.createRoutines[parent_table_line - 1].values[index].member_type].generated
    elif var == "CREATEVALUEPARAMETERINDEX":
        return type_formatter(gSession, dou.createRoutines[parent_table_line - 1].values[table_line - 1].parameter_index);
    elif var == "CREATEVALUE" :
        return member_formatter(gSession, dou.createRoutines[parent_table_line - 1].values[index].member)
    elif var == "CREATEVALUETYPE" :
        member = dou.createRoutines[parent_table_line - 1].values[index].member
        m_type = dou.member_name_to_type_lookup[member]
        return gSession.dod_types[m_type].generated
    elif var == "CREATEVALUEISARRAY": return create_value_is_array(dou, table_line, parent_table_line)
    elif var == "MEMBER" : return member_formatter(gSession, dou.members[index].name)
    elif var == "XMLMEMBER" : return dou.members[index].name
    elif var == "MEMBER'LENGTH" : return get_iterator_length("MEMBER", dou, 0, 0)
    elif var == "MEMBERCLASS" : return classname_formatter(gSession, dou.classname)
    elif var == "MEMBERSUMMARY" : return dou.members[index].summary
    elif var == "UNIFORM_MEMBERTYPE" : return gSession.dod_types[dou.members[index].type].uniform_type
    elif var == "MEMBERTYPE" : return gSession.dod_types[dou.members[index].type].generated
    elif var == "MEMBERISSTRING" : return member_is_string(dou, table_line)
    elif var == "MEMBERISARRAY" : return member_is_array(dou, table_line)
    elif var == "MEMBERISSEQUENCE" : return member_is_sequence(dou, table_line)
    elif var == "MEMBERISDICTIONARY" : return member_is_dictionary(dou, table_line)
    elif var == "MEMBERDICTIONARYTYPE" : return gSession.dod_types[dou.members[table_line - 1].dictionary_type].generated
    elif var == "UNIFORM_MEMBERDICTIONARYTYPE" : return gSession.dod_types[dou.members[table_line - 1].dictionary_type].uniform_type
    elif var == "PARAMETER" : return member_formatter(gSession, dou.parameters[index].name)
    elif var == "PARAMETER'LENGTH" : return get_iterator_length("PARAMETER", dou, 0, 0)
    elif var == "XMLPARAMETER" : return dou.parameters[index].name
    elif var == "PARAMETERCLASS" : return namespace_formatter(gSession, dou.classname)
    elif var == "PARAMETERSUMMARY" : return dou.parameters[index].summary
    elif var == "UNIFORM_PARAMETERTYPE" : return gSession.dod_types[dou.parameters[index].type].uniform_type
    elif var == "PARAMETERTYPE" : return gSession.dod_types[dou.parameters[index].type].generated
    elif var == "PARAMETERISARRAY" : return parameter_is_array(dou, table_line)
    elif var == "PARAMETERISDICTIONARY" : return parameter_is_dictionary(dou, table_line)
    elif var == "PARAMETERDICTIONARYTYPE" : return gSession.dod_types[dou.parameters[table_line - 1].dictionary_type].generated
    elif var == "UNIFORM_PARAMETERDICTIONARYTYPE" : return gSession.dod_types[dou.parameters[table_line - 1].dictionary_type].uniform_type
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
            return enum_formatter(gSession, dou.values[0])
        else:
            return enum_formatter(gSession, dou.values[index])
    elif var == "ENUMVALUE'LENGTH": return get_iterator_length("ENUMVALUE", dou, 0, 0)
    print("** ERROR - invalid var lookup,", var, file=sys.stderr)
    sys.exit(1)

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
    elif var == "LIBRARY_NAME": return -1
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
    sys.exit(1)

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
    return dou.createRoutines[parent_table_line - 1].values[table_line - 1].parameter_index is not None

def create_value_parameter_inline(dou, table_line, parent_table_line):
    if len(dou.createRoutines) == 0: return False
    if len(dou.createRoutines[parent_table_line - 1].values) == 0 : return False
    return dou.createRoutines[parent_table_line - 1].values[table_line - 1].inline

def parameter_is_array(dou, table_line):
    return trim_false(len(dou.parameters) > 0 and dou.parameters[table_line - 1].array)

def parameter_is_dictionary(dou, table_line):
    return trim_false(len(dou.parameters) > 0 and (dou.parameters[table_line - 1].dictionary_type is not None))

def member_is_array(dou, table_line):
    return trim_false(len(dou.members) > 0 and ((dou.members[table_line - 1].arraySize is not None) or (dou.members[table_line - 1].array is not None) or dou.members[table_line - 1].arraySizeRef))

def member_is_sequence(dou, table_line):
    return trim_false(len(dou.members) > 0 and dou.members[table_line - 1].sequence)

def member_is_dictionary(dou, table_line):
    return trim_false(len(dou.members) > 0 and (dou.members[table_line - 1].dictionary_type is not None))

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
    elif var == "MEMBERISSEQUENCE" : return member_is_sequence(dou, table_line)
    elif var == "MEMBERISDICTIONARY" : return member_is_dictionary(dou, table_line)
    elif var == "MEMBERSUMMARY": return len(dou.members) > 0 and dou.members[table_line - 1].summary is not None and len(dou.members[table_line - 1].summary) > 0
    elif var == "CLASSSUMMARY": return dou.summary is not None and len(dou.summary) > 0
    elif var == "DEPENDENCY": return len(dou.unique_dependencies) > 0
    elif var == "PARAMETER": return len(dou.parameters) > 0
    elif var == "PARAMETERSUMMARY": return len(dou.parameters) > 0 and dou.parameters[table_line - 1].summary is not None and len(dou.parameters[table_line - 1].summary) > 0
    elif var == "PARAMETERISARRAY" : return parameter_is_array(dou, table_line)
    elif var == "PARAMETERISSEQUENCE" : return parameter_is_sequence(dou, table_line)
    elif var == "PARAMETERISDICTIONARY" : return parameter_is_dictionary(dou, table_line)
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
    elif var == "CREATEVALUEISINLINE" : return create_value_parameter_inline(dou, table_line, parent_table_line)
    else:
        print("process_at_exist: Missing", var, file=sys.stderr)
        return False

def process_at_line(gSession, line, dou, table_line, parent_table_line, strings_with_quotes):
    pre, at_string, post = extract_at_string(line)
    if len(at_string) > 0:
        # maybe there is another @ string
        post = process_at_line(gSession, post, dou, table_line, parent_table_line, strings_with_quotes)
        processed = process_at_str(gSession, at_string, dou, table_line, parent_table_line, strings_with_quotes)
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

def process_at_str(gSession, at_string, dou, table_line, parent_table_line, strings_with_quotes):
    result = ""

    if at_string.find("EXIST:") != -1 :
        # Check for existance, treat specially
        result = process_at_exist(at_string, dou, table_line, parent_table_line)
    elif at_string.find(":") == -1:
        # Simple case, only variable lookup
        result = process_at_variable_lookup(gSession, at_string, dou, table_line, parent_table_line)
    else:
        # Variable lookup + processing
        ls = safe_at_string_splitter(at_string)
        result = process_at_variable_lookup(gSession, ls.pop(), dou, table_line, parent_table_line)

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
                rc1 = rc1.replace("\/", u("\u00A4"))
                # Then split on the non-espaced slash
                ptn, repl = rc1.split("/",1)
                # now put the slashes back, unescaped
                ptn = ptn.replace(u("\u00A4"), "/")
                repl = repl.replace(u("\u00A4"), "/")
                result = re.sub(ptn, repl, result, 1)

            elif command.startswith("REPLACE_ALL("):
                # ensure we only remove leading and last parentheses in case
                # there are any in the expression
                rc1 = command[command.find("(")+1:-1]
                # first replace all escaped slashes with something else
                rc1 = rc1.replace("\/", u("\u00A4"))
                # Then split on the non-espaced slash
                ptn, repl = rc1.split("/",1)
                # now put the slashes back, unescaped
                ptn = ptn.replace(u("\u00A4"), "/")
                repl = repl.replace(u("\u00A4"), "/")
                result = re.sub(ptn, repl, result)

            else:
                print("** ERROR - Unknown @ command", command, file=sys.stderr)
                sys.exit(1)

    if isinstance(result, (bool, int)): result = str(result)
    elif strings_with_quotes and not result.isdigit(): result = '"' + result + '"'
    return result

def get_at_string_iterator_length(line, dou, table_line, parent_table_line):
    ## Now handled before calling this function

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


def parse_table_clause(gSession, current_line, file, dou, process_table_content, parent_table_line):
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
                parse_if_clause(gSession, line, file, dou, process_table_content, table_line, parent_table_line)
            elif line.startswith("@@TABLE@@"):
                parse_table_clause(gSession, line, file, dou, process_table_content, table_line)
            elif line.startswith("@@END_TABLE@@"):
                table_line += 1
                if iterator_len < table_line:
                    # Nothing more to iterate
                    return
                else:
                    # One more go
                    file.seek(table_start)
            # skip comment
        elif process_table_content:
            if line.find("@_") != -1:
                if iterator_len > 0:
                    res = process_at_line(gSession, line, dou, table_line, parent_table_line, False)
                    write_to_file(gSession, res)
            else:
                # Plain string, just output
                write_to_file(gSession, line)

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

def evaluate_if_condition(gSession, line, dou, table_line, parent_table_line):
    # substitution to allow python evaluation
    line = if_string_subst(line)
    # substitute all the @_ expressions
    line = process_at_line(gSession, line, dou, table_line, parent_table_line, True)
    # line may start with @@IF@@ or @@ELSIF@@
    start = line.find("IF@@")
    expression = line[start+4:]
    # then let python do the work!
    return eval(expression)

def parse_parameters(gSession, line):
    valid_parameter = 0
    if line.startswith("Exception"):
        read_dod_exception(line, gSession.dod_exceptions)
        valid_parameter = 1
    elif line.startswith("Type"):
        read_dod_type(line, gSession.dod_types)
        valid_parameter = 1
    else:
        for dod_param in dod_parameter_names:
            if read_dod_parameter(line, dod_param, gSession.dod_parameters):
                valid_parameter = 1
                break

    if not valid_parameter:
        print("** ERROR - Cannot parse parameter line:", line, file=sys.stderr)
        sys.exit(1)


def parse_if_clause(gSession, current_line, file, dou, process_if_content, table_line, parent_table_line):
    gSession.if_level += 1
    process_this_level = 0
    match_on_this_level = 0
    if process_if_content:
        if evaluate_if_condition(gSession, current_line, dou, table_line, parent_table_line):
            process_this_level = 1
            match_on_this_level = 1

    while 1:
        line = file.get_next_line()
        if not line: break

        if line[0] == '@':
            # watch out for next IF, ELSIF or ENDIF
            if line.startswith("@@IF@@"):
                # Another IF encountered, recurse with skip flag
                parse_if_clause(gSession, line, file, dou, process_this_level, table_line, parent_table_line)

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
                        if evaluate_if_condition(gSession, line, dou, table_line, parent_table_line):
                            process_this_level = 1
                            match_on_this_level = 1
                    else:
                        process_this_level = 0

            elif line.startswith("@@END_IF@@"):
                # Found the end of this clause, return
                gSession.if_level -= 1
                return

            # Skip comment
            #elif process_this_level:
            #    if (line.startswith("@@--") or line.startswith("@@ ") or line == "@@\n"): continue

            elif line.startswith("@@TABLE@@"):
                parse_table_clause(gSession, line, file, dou, process_this_level, table_line)

        elif process_this_level:
            # line does not start with @

            # Special parsing for parameters section
            if gSession.current_section == "Parameters":
                parse_parameters(gSession, line)

            elif line.find("@_") != -1:
                res = process_at_line(gSession, line, dou, table_line, parent_table_line, False)
                write_to_file(gSession, res)

            else:
                # Plain string, just output
                write_to_file(gSession, line)
        # else we skip the line

def write_to_file(gSession, s):
    if gSession.current_generated_file is None:
        gSession.current_generated_file = codecs.open(gSession.current_generated_filename, "w", encoding="utf-8")
        if gSession.current_generated_file is None:
            print("** ERROR - could not open output file!", gSession.current_generated_filename, file=sys.stderr)
            sys.exit(1)

    # wierd way that the original parses the .dod:
    if s != "\n" and s.strip() == "": return

    # Make the line endings native
    s = s.replace("\n", os.linesep)

    gSession.current_generated_file.write(s)
    if loglevel >=4 : print(s, end='', file=sys.stderr)


import multiprocessing

# Since these directories do not interfere, we can allow parallell processing
MKDIR_CPP_RLOCK = multiprocessing.RLock()
MKDIR_ADA_RLOCK = multiprocessing.RLock()
MKDIR_DOTNET_RLOCK = multiprocessing.RLock()
MKDIR_JAVA_RLOCK = multiprocessing.RLock()
MKDIR_TAGS_RLOCK = multiprocessing.RLock()
MKDIR_OTHER_RLOCK = multiprocessing.RLock()

def find_mkdir_rlock(newdir):
    if newdir.endswith("/cpp") or newdir.find("/cpp/"): return MKDIR_CPP_RLOCK
    elif newdir.endswith("/ada") or newdir.find("/ada/"): return MKDIR_ADA_RLOCK
    elif newdir.endswith("/dotnet") or newdir.find("/dotnet/"): return MKDIR_DOTNET_RLOCK
    elif newdir.endswith("/java") or newdir.find("/java/"): return MKDIR_JAVA_RLOCK
    elif newdir.endswith("/tags") or newdir.find("/tags/"): return MKDIR_TAGS_RLOCK
    return MKDIR_OTHER_RLOCK



def mkdir(gSession, newdir):
    """works the way a good mkdir should :)
        - already exists, silently complete
        - regular file in the way, raise an exception
        - parent directory(ies) does not exist, make them as well
    """
    def mkdir_internal(newdir):
        # Someone may have created the dir while we waited for the RLock
        if os.path.isdir(newdir):
            pass
        elif os.path.isfile(newdir):
            raise OSError("a file with the same name as the desired " \
                          "dir, '%s', already exists." % newdir)
        else:
            head, tail = os.path.split(newdir)
            if head and not os.path.isdir(head):
                mkdir(gSession, head)
            if tail:
                os.mkdir(newdir)


    if os.path.isdir(newdir): return

    if gSession is None:
        mkdir_internal(newdir)
    else:
        with gSession.mkdir_rlock:
            mkdir_internal(newdir)

def get_dou_directories():
    #we cache the output so that we only run the safir_show_config command once.
    #here we check for cached data
    if hasattr(get_dou_directories,"cache"):
        return get_dou_directories.cache

    proc = subprocess.Popen(("safir_show_config", "--dou-install-dirs"),
                            stdout = subprocess.PIPE,
                            stderr = subprocess.PIPE,
                            universal_newlines = True)
    output = proc.communicate()
    if proc.returncode != 0:
        print ("Failed to run safir_show_config. \nStdout:\n",output[0],"\nStderr:\n",output[1],sep='')
        sys.exit(1)
    if len(output[1]) != 0:
        print ("Spurious output from safir_show_config. \nStdout:\n",output[0],"\nStderr:\n",output[1],sep='')
        sys.exit(1)
    dou_directories = list()
    for line in output[0].splitlines():
        (module,path) = line.split("=")
        dou_directories.append((module,path))

    #cache the data
    get_dou_directories.cache=dou_directories
    return dou_directories

def resolve_typesystem_dependencies(unresolved_dependencies):
    result = list()

    #first we try to just split the list items, assuming that
    #there are no dependencies that need to be resolved using
    #typesystem.ini information

    try:
        for dependency in unresolved_dependencies:
            (module,path) = dependency.split("=")
            result.append(path)
        return result
    except:
        pass

    #if we get here we need to use try again, using typesystem.ini
    #information. First of all we need to reset the results list.
    result = list()

    dirs = set([pair[0] for pair in get_dou_directories()])

    for dependency in unresolved_dependencies:
        pair = dependency.split("=")
        if len(pair) == 1: #an unresolved dependency
            if pair[0] in dirs:
                for (module,path) in get_dou_directories():
                    result.append(path)
                    if module == pair[0]:
                        break
        elif len(pair) == 2:
            result.append(pair[1])
        else:
            print("Syntax error for dependencies argument!", dependency, file=sys.stderr)
            sys.exit(1)
    return result

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
                    # wierd way that the original parses the .dod
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

def parse_dod(gSession, dod_file, dou):
    while 1:
        line = dod_file.get_next_line()
        if not line: break

        # Check for @@IF@@
        if line.startswith("@@IF@@"): parse_if_clause(gSession, line, dod_file, dou, 1, -1, -1)

        # Skip everything else at this top level

def dod_init(gSession, dod_filename):
    gSession.dod_parameters.clear()
    gSession.dod_exceptions.clear()
    gSession.dod_types.clear()

    dod_file = FileReader(dod_filename, True)
    gSession.current_section = "Parameters"

    parse_dod(gSession, dod_file, None)
    if not are_dod_parameters_complete(gSession.dod_parameters, dod_parameter_names):
        print("** ERROR - missing mandatory parameters, check ", dod_file, file=sys.stderr)
        sys.exit(1)

    dod_file.seek(0)
    return dod_file

def generator_main(gSession, dod_file, dou_filename, gen_src_output_path):
    parent_namespaces_completed = []

    dou = parse_dou(gSession, dou_filename)

    # Parameters section is already handled before generator_main is called
    sections = ["Parent", "Code"]

    # One pass through the file per section
    for section in sections:
        gSession.current_section = section
        dod_file.seek(0)

        if section == "Parent":
            # Generate a "Parent" file for each pass (only used for ADA and tags)
            gSession.parent_namespace = ""
            for n in dou.namespaces:
                if len(gSession.parent_namespace) > 0: gSession.parent_namespace += "."
                gSession.parent_namespace += n
                if gSession.parent_namespace in parent_namespaces_completed:
                    # This file has already been generated (for this dod)
                    continue

                empty_dou = Dou()
                output_path = gen_src_output_path + gSession.dod_parameters["Output_Directory"]
                parent_name = filename_formatter(gSession, gSession.parent_namespace) + gSession.dod_parameters["File_Suffix"]
                gSession.current_generated_filename = os.path.join(output_path, parent_name)
                mkdir(gSession, output_path)

                parse_dod(gSession, dod_file, empty_dou)
                dod_file.seek(0)
                if gSession.current_generated_file is not None:
                    # Write final newline and close
                    gSession.current_generated_file.write(os.linesep)
                    gSession.current_generated_file.close()
                    gSession.current_generated_file = None

                parent_namespaces_completed.append(gSession.parent_namespace)

        else: #Code
            output_path = gen_src_output_path + gSession.dod_parameters["Output_Directory"]

            # Namespace prefix files? (for java)
            namespaces = ".".join(dou.namespaces)
            if gSession.dod_parameters['Namespace_Prefix_File_Suffix'] != "":
                namespaces = namespace_prefixer(gSession, namespaces)
                dou.namespaces = namespaces.split(".")

            if gSession.dod_parameters["Filename_Separator"] == "/":
                output_path += directory_name_formatter(gSession, namespaces)
                filename = filename_formatter(gSession, dou.classname) + gSession.dod_parameters["File_Suffix"]
            else:
                filename = filename_formatter(gSession, namespaces) + gSession.dod_parameters["Filename_Separator"] + filename_formatter(gSession, dou.classname) + gSession.dod_parameters["File_Suffix"]

            mkdir(gSession, output_path)
            gSession.current_generated_filename = os.path.join(output_path, filename)

            parse_dod(gSession, dod_file, dou)
            if gSession.current_generated_file is not None:
                # Write final newline and close
                gSession.current_generated_file.write(os.linesep)
                gSession.current_generated_file.close()
                gSession.current_generated_file = None

    # end for

class GeneratorSession(object):
    def __init__(self):
        self.current_section = None
        self.current_generated_filename = None
        self.current_generated_file = None
        self.parent_namespace = None
        self.dod_parameters = {}
        self.dod_exceptions = {}
        self.dod_types = {}
        self.namespace_prefixes = {}
        self.if_level = 0
        self.dou_uniform_lookup_cache = None
        self.dou_file_lookup_cache = None
        self.dou_xml_lookup_cache = None
        self.dependency_paths = None
        self.namespace_prefix_files = None
        self.library_name = None

def dod_thread_main(dod_filename,
                    dou_files,
                    gen_src_output_path,
                    show_files,
                    dou_uniform_lookup_cache,
                    dou_file_lookup_cache,
                    dou_xml_lookup_cache,
                    dependency_paths,
                    namespace_prefix_files,
                    library_name):
    t1 = os.times()[4]
    global loglevel

    gSession = GeneratorSession()
    gSession.dou_uniform_lookup_cache = dou_uniform_lookup_cache
    gSession.dou_file_lookup_cache = dou_file_lookup_cache
    gSession.dou_xml_lookup_cache = dou_xml_lookup_cache
    gSession.dependency_paths = dependency_paths
    gSession.namespace_prefix_files = namespace_prefix_files
    gSession.library_name = library_name

    dod_file = dod_init(gSession, dod_filename)
    output_dir = os.path.join(gen_src_output_path, gSession.dod_parameters["Output_Directory"])
    gSession.mkdir_rlock = find_mkdir_rlock(output_dir.replace(os.sep, "/"))

    namespace_prefix_init(gSession)

    for dou_file in dou_files:
        if show_files: print(">>Processing", dod_filename, "  ", dou_file, file=sys.stderr)
        generator_main(gSession, dod_file, dou_file, gen_src_output_path)

    t2 = os.times()[4]
    if loglevel >=4: print(".dod process duration", t2-t1, os.path.split(dod_filename)[1])

def main():
    global loglevel

    # version dependent features
    support_multicpu = True
    if sys.version_info[0] == 3:
        support_multicpu = False

        # if ((sys.version_info[1] == 3 and sys.version_info[2] >= 2)) or (sys.version_info[1] > 3) or (sys.version_info[1] == 2 and sys.version_info[2] >= 5):
        #    # This was introduced due to http://bugs.python.org/issue16076, which was resolved in 3.3.2
        #    support_multicpu = True

    arguments = None

    parser = argparse.ArgumentParser(description='Source code generator tool for Safir SDK Core. Processes .dou files into source code for all supported languages. Files are generated in language specific subdirectories of root path of processed dou files.')

    parser.add_argument('--dou-files',
                        metavar='DOU_FILE(S)',
                        nargs="*",
                        help='.dou file(s) to process.')
    parser.add_argument('--namespace-mappings',
                        metavar='NAMESPACE_MAPPING(S)',
                        nargs="*",
                        help='.namespace.txt files to process for namespaces.')
    parser.add_argument('--dod-files',
                        metavar='DOD_FILE(S)',
                        nargs="*",
                        required=True,
                        help='Specifies .dod files to use as templates for the processing.')
    parser.add_argument('--dependencies',
                        metavar='DEPENDENCIES',
                        required=False,
                        nargs="*",
                        help="Paths to dou file directory that this module depends on.")
    parser.add_argument('--library-name',
                        metavar='LIBRARY_NAME',
                        required=True,
                        help="Name of the generated library/module being built")
    parser.add_argument('--output-path',
                        metavar='OUTPUT_PATH',
                        required=False,
                        default='',
                        help='Directory where the generated file structure starts. Defaults to current working directory.')
    parser.add_argument('--show-files',
                        required=False,
                        default=False,
                        action='store_true',
                        help='Prints out the dod and dou filenames for each parsing')
    parser.add_argument('-v', '--verbose',
                        required=False,
                        default=False,
                        action='store_true',
                        help='Prints debugging info from the parsing to stderr')
    parser.add_argument('--multiprocess',
                        required=False,
                        default=False,
                        action='store_true',
                        help='Activates multiprocessing, will spawn 1 process per .dod file (normally 7 processes).')

    arguments = parser.parse_args()

    dod_files = [os.path.normpath(f) for f in arguments.dod_files]
    dou_files = [os.path.normpath(f) for f in arguments.dou_files]
    namespace_prefix_files = [os.path.normpath(f) for f in arguments.namespace_mappings]

    dependency_paths = resolve_typesystem_dependencies(arguments.dependencies)

    gen_src_output_path = arguments.output_path
    if gen_src_output_path == "":
        gen_src_output_path = os.getcwd()
    else:
        if not os.path.isdir(gen_src_output_path):
            mkdir(None, gen_src_output_path)
            if not os.path.isdir(gen_src_output_path):
                print("Invalid argument for output path.", file=sys.stderr)
                sys.exit(1)
    gen_src_output_path = os.path.abspath(gen_src_output_path) + os.sep

    if len(dou_files) == 0:
        print("No valid dou files to process.", file=sys.stderr)
        sys.exit(1)

    if arguments.verbose: loglevel = 4

    if support_multicpu and arguments.multiprocess:
        # Prepare dou lookup tables
        # We don't need to clear this between the dou or dod files, since the result is the same for all
        # Used shared memory dictionaries to avoid copying huge amounts of data for each process
        shared_mem_manager = multiprocessing.Manager()
        dou_uniform_lookup_cache = shared_mem_manager.dict()
        dou_file_lookup_cache = shared_mem_manager.dict()
        dou_xml_lookup_cache = shared_mem_manager.dict()
        dou_uniform_lookup_init(dou_uniform_lookup_cache, dou_file_lookup_cache, dou_xml_lookup_cache, dependency_paths, dou_files)

        pool = multiprocessing.Pool() # Defaults number of worker processes to number of CPUs

        for dod_filename in dod_files:
            pool.apply_async(dod_thread_main, args = (dod_filename, dou_files, gen_src_output_path, arguments.show_files, dou_uniform_lookup_cache, dou_file_lookup_cache, dou_xml_lookup_cache, dependency_paths, namespace_prefix_files, arguments.library_name))

        pool.close()
        pool.join()

    else:
        # Prepare dou lookup tables
        # We don't need to clear this between the dou or dod files, since the result is the same for all
        dou_uniform_lookup_cache = {}
        dou_file_lookup_cache = {}
        dou_xml_lookup_cache = {}
        dou_uniform_lookup_init(dou_uniform_lookup_cache, dou_file_lookup_cache, dou_xml_lookup_cache, dependency_paths, dou_files)

        for dod_filename in dod_files:
            dod_thread_main(dod_filename, dou_files, gen_src_output_path, arguments.show_files, dou_uniform_lookup_cache, dou_file_lookup_cache, dou_xml_lookup_cache, dependency_paths, namespace_prefix_files, arguments.library_name)

    return 0

if __name__ == "__main__":
    sys.exit(main())
