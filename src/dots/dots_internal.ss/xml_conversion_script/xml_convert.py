#!/usr/bin/python
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


def print_help():
    """Print help message"""
    print("-h : print this help")
    print("-f <file> : convert single file")
    print("-d <directory> : convert files in directory")
    print("-r : convert directory recursive, must be combined with -d")
    print("-o <directory> : output directory, place converted files here. (Mandatory)")
    print("-v verbose")
    
def parse_commandline(argv):
    """Parse commandline"""
    global convert_mode    
    global recursive
    global output_dir
    global input_path
    global verbose
    
    try:
        opts, args = getopt.getopt(argv,"hrvf:d:o:")
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

def indent(elem, level=0):
    """ pretty print xml """
    i = "\n" + level*"  "
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
    member_element=ET.SubElement(dest, member_name)
    array_elements=src.find(ns('arrayElements'))
    index=0
    for arr_el in array_elements.findall(ns('arrayElement')):
        index_el=arr_el.find(ns('index'))
        if index_el!=None:
            index=int(index_el.text)
        convert_member_item(arr_el, member_element, '')
        member_element[-1].attrib['index']=str(index)
        index=index+1
    pass

def convert_member_item(src, dest, member_name):    
    """ convert a member or array item """
    #1. <value> ---> <mem>value</mem>
    #2. <entityId> ---> <memEid> <name>Entity</name> <instanceId>3</instanceId> </memEid>
    #3. <object> ---> <memObj type="typeName"> ..... </memObj>

    value_element=src.find(ns('value'))
    if value_element!=None:
        if member_name=='':
            member_name='value'
        member_element=ET.SubElement(dest, member_name)
        member_element.text=value_element.text
    else:
        eid_element=src.find(ns('entityId'))
        if eid_element!=None:
            if member_name=='':
                member_name='entityId'
            member_element=ET.SubElement(dest, member_name)
            typename_element=ET.SubElement(member_element, 'name')
            instance_element=ET.SubElement(member_element, 'instanceId')
            typename_element.text=eid_element.find(ns('name')).text
            instance_element.text=eid_element.find(ns('instanceId')).text
        else:
            obj_element=src.find(ns('object'))
            if obj_element!=None:
                inner_obj=convert_object(obj_element, member_name)
                dest.append(inner_obj)
                #insert_at=len(dest)
                #dest.insert(insert_at, inner_obj)
            else:
                print('*** Unexpected member in '+current_file)
                for x in src:
                    print(x.tag)

def convert_object(tree, root_name=''):
    """Convert object structure"""
    type_name=tree.find(ns('name')).text
    if root_name=='':
        root_name=type_name
    obj=ET.Element(root_name)
    
    obj.attrib['type']=type_name
        

    members=tree.find(ns('members'))
    if members!=None:        
        for member in members.findall(ns('member')):
            member_name=member.find(ns('name')).text
            if member.find(ns('arrayElements'))!=None:
                convert_array(member, obj, member_name)
            else:
                convert_member_item(member, obj, member_name)

    return obj

def traverse_xml(tree):
    """Traverse xml structure and converts object-elements"""
    changed=False
    index=0
    for child in tree:
        if child.tag==ns('object'):
            new_child=convert_object(child)
            tree.remove(child)
            tree.insert(index, new_child)
            changed=True
        else:
            if traverse_xml(child):
                changed=True
        index=index+1
    return changed

def convert_file(file_path, out_dir):
    """Converts a single file and store result in out_dir. Only modified files are saved"""
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
    root = tree.getroot()
    set_current_namespace(root.tag)
    if root.tag==ns('object'):
        root=convert_object(root)
        tree._setroot(root)
        changed=True
    else:
        changed=traverse_xml(root)    

    if changed:        
        #save new file tree.write
        output('Converted file: '+file_path)
        indent(root)
        new_file=os.path.join(out_dir, os.path.basename(file_path))        
        tree.write(new_file, 'utf-8')        
        number_of_converted=number_of_converted+1
    else:
        output('No changes to: '+file_path)
        number_of_unchanged=number_of_unchanged+1
 
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
        break
    
def main(argv):
    """Main program"""
    parse_commandline(argv)
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
    
