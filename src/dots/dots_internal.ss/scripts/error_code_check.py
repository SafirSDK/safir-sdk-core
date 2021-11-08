#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2014 (http://safirsdkcore.com)
#
# Created by: Joel Ottosson (joel.ottosson@consoden.se)
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
import os
import sys
import getopt


err = []
tests = set([])


def collect_tests(path):
    global tests
    for root, dirs, files in os.walk(path):
        for d in dirs:
            try:
                ix = d.rfind('-') + 1
                num = int(d[ix:].strip())
                tests.add(num)
            except:
                pass


def inspect_line(line):
    global err
    if 'throw ParseError' in line:
        startIx = line.rfind(',') + 1
        endIx = line.rfind(');')
        num = int(line[startIx:endIx].strip())
        err.append(num)


def inspect_file(file):
    f = open(file)
    line_number = 1
    for line in f:
        try:
            inspect_line(line)
        except:
            print('At ' + file + '(' + str(line_number) + '): ' + line)
        line_number = line_number + 1
    f.close()


def main(argv):
    """Main program"""
    if len(argv) < 2:
        print('usage: error_codes src_path test_path')
        return

    if len(argv) >= 3:
        collect_tests(argv[2])

    global err
    global tests

    for root, dirs, files in os.walk(argv[1]):
        for file in files:
            inspect_file(os.path.join(root, file))
    err.sort()
    last = -1
    for i in err:
        msg = str(i)
        if i in tests:
            msg = msg + ' t'
        if last == i:
            msg = msg + ' d'
        if i > last + 1:
            msg = msg + ' h'
        last = i
        print(msg)


#------------------------------------------------
# If this is the main module, start the program
#------------------------------------------------
if __name__ == "__main__":
    main(sys.argv)
