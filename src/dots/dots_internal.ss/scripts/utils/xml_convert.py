#!/usr/bin/env python2
# -*- coding: utf-8 -*-
######################################################################
# Created by: Joel Ottosson / joot
#
# Converts xml serialized objects in the old format into the new.
#
######################################################################

import os, sys, getopt
import xml.etree.ElementTree as ET

convert_mode=""
recursive=False
output_dir=""
input_path=""
number_of_converted=0
number_of_unchanged=0
number_of_failed=0
verbose=False
current_namespace=""
current_file=""
error_files=[]
no_array_conv=False


def print_help():
    """Print help message"""
    print("-h : print this help")
    print("-f <file> : convert single file")
    print("-d <directory> : convert files in directory")
    print("-r : convert directory recursive, must be combined with -d")
    print("-o <directory> : output directory, place converted files here. (Mandatory)")
    print("-v : verbose")
    print("--no-array-conversion : don't convert arrays to new format.")
    
def parse_commandline(argv):
    """Parse commandline"""
    global convert_mode    
    global recursive
    global output_dir
    global input_path
    global verbose
    global no_array_conv
    
    try:
        opts, args = getopt.getopt(argv,"hrvf:d:o:", ["no-array-conversion"])
    except getopt.GetoptError:
        print("Invalid commandline")
        print_help()
        sys.exit(1)

    for opt, arg in opts:        
        if opt=='-h':
            print_help()
            sys.exit(0)
        elif opt=='-o':
            output_dir=arg;
        elif opt=='-f':
            input_path=arg;
            convert_mode="file"
        elif opt=='-d':
            input_path=arg;
            convert_mode="dir"
        elif opt=='-r':
            recursive=True
        elif opt=='-v':
            verbose=True
        elif opt=='--no-array-conversion':
            no_array_conv=True
            
    if output_dir=="" or not os.path.isdir(output_dir):
        print("Output directory missing.")
        print_help()
        sys.exit(1)

    if input_path=="":
        print("Must specify either -f <file> or -d <directory>")
        print_help()
        sys.exit(1)
        
    if convert_mode=='file' and not os.path.isfile(input_path):
        print('File does not exist!')
        sys.exit(1)
    elif convert_mode=='dir' and not os.path.isdir(input_path):
        print('Directory does not exist!')
        sys.exit(1)
        
    if recursive and convert_mode=="file":
        print("Recursive flag -f has no effect when converting singel file")
        
    output("Mode="+convert_mode+" input="+input_path+", output="+output_dir+", recursive="+str(recursive))

def output(msg):
    if verbose:
        print(msg)

def set_current_namespace(tag):
    """get xml namespace """
    global current_namespace
    current_namespace=''
    if tag[0]=='{':
        current_namespace=tag[0:tag.find('}')+1]
        defns=current_namespace[1:len(current_namespace)-1]
        ET.register_namespace('', defns)
 
def ns(tag):
    return current_namespace+tag

def to_path(l):
    result=ns(l[0])
    for s in l[1:]:
        result=result+"/"+ns(s)
    return result
    
def indent(elem, level=0):
    """ pretty print xml """
    i = os.linesep + level*"  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "  "
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
        for elem in elem:
            indent(elem, level+1)
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i

def convert_array(src, dest, member_name):
    array_elements=src.find(ns('arrayElements'))
    if array_elements.find(ns('arrayElement'))==None:
        #empty array
        return
        
    member_element=ET.SubElement(dest, member_name)
    index=0
    for arr_el in array_elements.findall(ns('arrayElement')):
        index_el=arr_el.find(ns('index'))
        if index_el!=None:
            index=int(index_el.text)
        if convert_member_item(arr_el, member_element, ''):
            member_element[-1].attrib['index']=str(index)
        index=index+1

def convert_member_item(src, dest, member_name):    
    """ convert a member or array item """
    #1. <value> ---> <mem>value</mem>
    #2. <entityId> ---> <memEid> <name>Entity</name> <instanceId>3</instanceId> </memEid>
    #3. <object> ---> <memObj type="typeName"> ..... </memObj>
    #4. <valueRef><name>...</name><index>...</index></valueRef> ---> <mem valueRef="..." valueRefIndex="..."/>

    member_name=member_name.strip()
    value_element=src.find(ns('value'))
    if value_element!=None:
        if member_name=='':
            member_name='value'
        member_element=ET.SubElement(dest, member_name)
        if value_element.text!=None:
            member_element.text=value_element.text.strip()
        else:
            member_element.text=''
    else:
        eid_element=src.find(ns('entityId'))
        if eid_element!=None:
            if member_name=='':
                member_name='entityId'
            member_element=ET.SubElement(dest, member_name)
            typename_element=ET.SubElement(member_element, 'name')
            instance_element=ET.SubElement(member_element, 'instanceId')
            typename_element.text=eid_element.find(ns('name')).text.strip()
            instance_element.text=eid_element.find(ns('instanceId')).text.strip()
        else:
            obj_element=src.find(ns('object'))
            if obj_element!=None:
                inner_obj=convert_object(obj_element, member_name, '')
                dest.append(inner_obj)
                #insert_at=len(dest)
                #dest.insert(insert_at, inner_obj)
            else:
                value_ref_element=src.find(ns('valueRef'))
                if value_ref_element!=None:
                    if member_name=='':
                        member_name='value'
                    member_element=ET.SubElement(dest, member_name)
                    param_name_element=value_ref_element.find(ns('name'))
                    member_element.attrib['valueRef']=param_name_element.text.strip()
                    param_index_element=value_ref_element.find(ns('index'))
                    if param_index_element!=None:
                        member_element.attrib['valueRefIndex']=param_index_element.text.strip()
                else:
                    return False #null member, not inserted
    return True #somethin inserted

def convert_object(tree, root_name, base_type):
    """Convert object structure"""
    type_name=tree.find(ns('name')).text.strip()
    if root_name=='':
        root_name=type_name
            
    obj=ET.Element(root_name)
    
    if type_name!=base_type:
        obj.attrib['type']=type_name

    members=tree.find(ns('members'))
    if members!=None:        
        for member in members.findall(ns('member')):
            member_name=member.find(ns('name')).text.strip()
            if member.find(ns('arrayElements'))!=None:
                convert_array(member, obj, member_name)
            else:
                convert_member_item(member, obj, member_name)

    return obj

def traverse_xml(tree, base_type):
    """Traverse xml structure and converts object-elements"""
    changed=False
    index=0
    for child in tree:
        if child.tag==ns('object'):
            new_child=convert_object(child, base_type, base_type)
            tree.remove(child)
            tree.insert(index, new_child)
            changed=True
        else:
            if traverse_xml(child, base_type):
                changed=True
        index=index+1
    return changed

def convert_objects(tree):
    """Converts all old format objects in a xml tree"""
    root = tree.getroot()
    set_current_namespace(root.tag)    
    changed=False
    
    #Handle parameters first since we have a chance to reduce the use of type-attributes for parameters
    if root.tag==ns('class'):        
        for par in root.findall(to_path(["parameters", "parameter"])):
            par_name=par.find(ns("name")).text.strip()
            type_name=par.find(ns("type")).text.strip()            
            if traverse_xml(par, type_name):
                changed=True
    #handle other objects, can be plain serialized objects or in dom-files        
    if root.tag==ns('object'):
        root=convert_object(root, '', '')
        tree._setroot(root)
        changed=True
    elif traverse_xml(root, ''):
        changed=True
    
    return changed


def convert_create_routines(tree):
    """Converts all old format create routines in a xml tree"""
    root = tree.getroot()
    set_current_namespace(root.tag)    
    changed=False
    
    #Handle parameters first since we have a chance to reduce the use of type-attributes for parameters
    if root.tag==ns('class'):        
        for cr in root.findall(to_path(["createRoutines", "createRoutine", "values", "value", "parameter"])):
            if cr.find(ns("name"))==None:
                cr_name_element=ET.SubElement(cr, "name")
                cr_name_element.text=cr.text.strip()
                cr.text=""
                changed=True
    return changed
    
def convert_parameter_arrays(tree):
    """Converts all old format arrays in a xml tree"""
    if no_array_conv:
        return False
    
    root = tree.getroot()
    set_current_namespace(root.tag)    
    changed=False
    for arrayElements in root.findall(".//"+ns("arrayElements")):
        index=0
        for ae in arrayElements:
            index_element=ae.find(ns("index"))
            if index_element!=None:
                ae.remove(index_element)
            if len(ae)>0:
                child=ae[0]
                arrayElements.insert(index, child)
            arrayElements.remove(ae)
            index=index+1
        arrayElements.tag="array"
        changed=True
    return changed

def convert_file(file_path, out_dir):
    """Converts a single file and store result in out_dir. Only modified files are saved"""
    output('Handle file '+file_path)
    global current_file
    global number_of_converted
    global number_of_unchanged
    global number_of_failed
    global error_files
    current_file=file_path
    try:
        tree = ET.parse(file_path)
    except:
        output("*** Couldnt parse file: "+file_path+". Skipping file.")
        error_files.append(file_path)
        number_of_failed=number_of_failed+1
        return
    
    #Convert old format createRoutines
    converted_create_routines=convert_create_routines(tree)
    
    #Convert old format objects
    converted_objects=convert_objects(tree)

    #Convert old format arrays, i.e arrayElements
    converted_arrays=convert_parameter_arrays(tree)
    
    if  converted_objects or converted_arrays or converted_create_routines:        
        #save new file tree.write
        root = tree.getroot()
        set_current_namespace(root.tag)
        if converted_objects: output('                - Converted objects')
        if converted_arrays: output('                - Converted arrays')
        if converted_create_routines: output('                - Converted create routines')
        indent(root)
        new_file=os.path.join(out_dir, os.path.basename(file_path))
        
        #Since tree.write will add a unix lineending after <?xml version...?> we do this step manually instead.
        #tree.write(new_file, encoding='utf-8', xml_declaration=True, default_namespace=None, method='xml')
        xml_str='<?xml version="1.0" encoding="utf-8" ?>'+os.linesep+ET.tostring(root, encoding='utf-8', method='xml')
        outfile=open(new_file, "w")
        outfile.write(xml_str)
        outfile.close()
        
        number_of_converted=number_of_converted+1
    else:
        number_of_unchanged=number_of_unchanged+1
        output('                - No changes')
 
def convert_dir(path, out_dir):
    """Traverse a directory and convert every file."""
    output('Handle directory '+path)
    for root, dirs, files in os.walk(path):
        for f in files:
            convert_file(os.path.join(root, f), out_dir)
        if recursive:
            for d in dirs:
                od=os.path.join(out_dir, d)
                try:
                    os.makedirs(od)
                except:
                    pass 
                convert_dir(os.path.join(root, d), od)
                
                if not os.listdir(od):
                    #empty folder, remove it again
                    os.rmdir(od)
        break
    
def main(argv):
    """Main program"""
    parse_commandline(argv)
    ET.register_namespace('xsi', 'http://www.w3.org/2001/XMLSchema-instance')
    if convert_mode=="file":
        convert_file(input_path, output_dir)
    else:
        convert_dir(input_path, output_dir)
    print('--------------------------------')
    print('#converted: '+str(number_of_converted))
    print('#unchanged: '+str(number_of_unchanged))
    print('#failed:    '+str(number_of_failed))
    for failed in error_files:
        print('  Error: '+failed)

#------------------------------------------------
# If this is the main module, start the program
#------------------------------------------------
if __name__ == "__main__":
    main(sys.argv[1:])
    
