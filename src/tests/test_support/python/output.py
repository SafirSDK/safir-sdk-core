#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2026 (http://safirsdkcore.com)
#
# Created by: Lars Hagstrom / lars@foldspace.nu
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
"""Output utility functions for test scripts.

These functions handle stdout flushing and Unicode encoding errors that can
occur on some platforms (particularly Windows consoles).
"""
import datetime
import sys


def out(*args, **kwargs):
    """Print with flush and Unicode safety.

    Works like print() but always flushes stdout and handles Unicode
    encoding errors gracefully by replacing problematic characters.
    """
    try:
        print(*args, **kwargs)
    except UnicodeEncodeError:
        safe_args = [str(a).encode("ascii", errors="replace").decode() for a in args]
        print(*safe_args, **kwargs)
    sys.stdout.flush()


def log(*args, **kwargs):
    """Print with timestamp, flush and Unicode safety.

    Like out() but prepends an ISO format timestamp to the output.
    """
    timestamp = datetime.datetime.now().isoformat()
    try:
        print(timestamp, ":", *args, **kwargs)
    except UnicodeEncodeError:
        safe_args = [str(a).encode("ascii", errors="replace").decode() for a in args]
        print(timestamp, ":", *safe_args, **kwargs)
    sys.stdout.flush()
