#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2009-2014 (http://safirsdkcore.com)
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
"""Command line build tool for user dou-projects (installed as 'dobmake-batch').

This is the non-GUI counterpart of the dobmake application: it builds the dou
project in the current directory using the same logic as the Safir SDK Core
build, but it deliberately does NOT offer the --package option. Packaging is a
concern of the SDK build itself (build.py) only, which keeps the SDK-specific
release machinery (including the dual-ABI fast path) out of reach of user
builds. The shared build logic lives in safir_build_common.py, a private,
import-only module (never run directly). In an installed tree it lives in the
shared private python dir lib/safir-sdk-core/python (deliberately out of bin/, so
it is not exposed on PATH); in the source tree it sits next to this script. We
add both candidate locations to sys.path so 'import safir_build_common' resolves
in either layout.

Run this from the directory containing your project's CMakeLists.txt.
"""
import os
import sys
import argparse

_script_dir = os.path.dirname(os.path.realpath(__file__))
for _candidate in (_script_dir,
                   os.path.join(_script_dir, "..", "lib", "safir-sdk-core", "python")):
    if _candidate not in sys.path:
        sys.path.insert(0, _candidate)

import safir_build_common as common


def parse_command_line():
    """parse the command line"""
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    action = parser.add_mutually_exclusive_group()

    action.add_argument("--install",
                        metavar="PATH",
                        help="Build the source in the current directory and install it to "
                        "PATH. If PATH is set to 'None' the install step will be run "
                        "without setting CMAKE_INSTALL_PREFIX, useful if your "
                        "CMakeLists.txt has absolute paths in the INSTALL directives.")

    action.add_argument("--clean",
                        action="store_true",
                        help="Remove previous build results instead of building.")

    common.add_common_arguments(parser)
    common.add_platform_options(parser)

    arguments = parser.parse_args()
    common.finalize_arguments(arguments)
    return arguments


sys.exit(common.execute(parse_command_line()))
