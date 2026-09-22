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
"""Build and package the safir-sdk-core source tree.

This is the entry point used by CI and by anyone producing the
installation packages. Building and packaging is the only thing it does - there
is no separate "build without packaging" mode here. To build an external user
dou-project use dobmake_batch.py instead (installed as dobmake-batch); to just
build the source tree as a developer, use cmake/ninja directly (see
BUILD.Linux.txt / BUILD.Windows.txt). The shared logic lives in
safir_build_common.py next to this file.
"""
import sys
import argparse

import safir_build_common as common


def parse_command_line():
    """parse the command line"""
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument("--noclean",
                        action="store_true",
                        help="Attempt to continue from a previous build instead of "
                        "building from scratch.")

    common.add_common_arguments(parser)
    common.add_platform_options(parser)

    arguments = parser.parse_args()

    # Packaging is the only mode this script has, so set the flags the shared
    # builder logic keys on. (dobmake_batch.py leaves these unset/false.)
    arguments.package = True
    arguments.package_noclean = arguments.noclean

    common.finalize_arguments(arguments)
    return arguments


sys.exit(common.execute(parse_command_line()))
