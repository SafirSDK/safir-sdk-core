#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2015 (http://safirsdkcore.com)
#
# Created by: Lars Hagstrom / lars.hagstrom@consoden.se
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
import sys
import os
import fnmatch

#exact matches
ignore_directories = (".git", ".svn", ".hg", ".venv", "boost", "rapidjson",
                      "websocketpp", "RelWithDebInfo", "Release", "Debug", "bin")

#glob patterns for files to not check for tabs
ignore_files = ("*.xcf", "*.xsl", "*.bmp", "*.ico", "*.eap", "*.png", "Makefile", "rules", "*.snk", "*.dia", "*.resx", "*.ttf")


def log(*args, **kwargs):
    print(*args, **kwargs)
    sys.stdout.flush()


def check_tabs(filename):
    try:
        #log("checking file", filename)
        count = 0
        with open(filename, "r", encoding="utf-8") as f:
            for line in f:
                count += 1
                if "\t" in line:
                    log(f"{os.path.abspath(filename)}:{count}:0 error: Tab character on line")
                    return False
    except UnicodeDecodeError as e:
        log(f" !!! UnicodeDecodeError ({str(e)}) while reading '{filename}'.")
        return False
    return True


def main():
    ok = True
    rootDir = '.'
    for dirName, subdirList, fileList in os.walk(rootDir):
        for id in ignore_directories:
            if id in subdirList: subdirList.remove(id)

        for fname in fileList:
            check = True
            for ig in ignore_files:
                if fnmatch.fnmatch(fname, ig):
                    check = False
                    break
            if check:
                ok = check_tabs(os.path.join(dirName, fname)) and ok
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
