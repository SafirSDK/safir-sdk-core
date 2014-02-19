#!/usr/bin/python
# -*- coding: utf-8 -*-
import os, sys, getopt

err=[]
tests=set([])

def collect_tests(path):
    global tests
    for root, dirs, files in os.walk(path):
        for d in dirs:
            try:            
                ix=d.rfind('-')+1
                num=int(d[ix:].strip())
                tests.add(num)
            except:
                pass

def inspect_line(line):
    global err
    if 'throw ParseError' in line:
        startIx=line.rfind(',')+1
        endIx=line.rfind(');')
        num=int(line[startIx:endIx].strip())
        err.append(num)

def inspect_file(file):
    f=open(file)
    line_number=1
    for line in f:
        try:
            inspect_line(line)
        except:
            print('At '+file+'('+str(line_number)+'): '+line)
        line_number=line_number+1
    f.close()

def main(argv):
    """Main program"""
    if len(argv)<2:
        print('usage: error_codes src_path test_path')
        return

    if len(argv)>=3:
        collect_tests(argv[2])

    global err
    global tests

    for root, dirs, files in os.walk(argv[1]):
        for file in files:
            inspect_file(os.path.join(root,file))
    err.sort()
    last=-1
    for i in err:
        msg=str(i)
        if i in tests:
            msg=msg+' t'
        if last==i:
            msg=msg+' d'
        if i>last+1:
            msg=msg+' h'
        last=i
        print(msg)        
        
#------------------------------------------------
# If this is the main module, start the program
#------------------------------------------------
if __name__ == "__main__":    
    main(sys.argv)
    
