# Copyright (c) 2008, OpenCog.org (http://opencog.org)
#
# Redistribution and use is allowed according to the terms of the BSD license.
# The license was originally included in a separate file, COPYING-CMAKE-SCRIPTS,
# but since this file has been taken out of that context the contents of that file
# are here:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# 1. Redistributions of source code must retain the copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# - Try to find the UnixODBC library; Once done this will define
#
# UnixODBC_FOUND - system has the UnixODBC library
# UnixODBC_INCLUDE_DIRS - the UnixODBC include directory
# UnixODBC_LIBRARIES - The libraries needed to use UnixODBC

# Look for the header file
FIND_PATH(UnixODBC_INCLUDE_DIR uodbc_stats.h /usr/include /usr/local/include /usr/include/odbc /usr/local/include/odbc /usr/include/libodbc /usr/local/include/libodbc)

# Look for the library
FIND_LIBRARY(UnixODBC_LIBRARY NAMES odbc PATH /usr/lib /usr/local/lib)

# Copy the results to the output variables.
IF (UnixODBC_INCLUDE_DIR AND UnixODBC_LIBRARY)
    SET(UnixODBC_FOUND 1)
    SET(UnixODBC_LIBRARIES ${UnixODBC_LIBRARY})
    SET(UnixODBC_INCLUDE_DIRS ${UnixODBC_INCLUDE_DIR})
ELSE (UnixODBC_INCLUDE_DIR AND UnixODBC_LIBRARY)
    SET(UnixODBC_FOUND 0)
    SET(UnixODBC_LIBRARIES)
    SET(UnixODBC_INCLUDE_DIRS)
ENDIF (UnixODBC_INCLUDE_DIR AND UnixODBC_LIBRARY)

# Report the results.
IF (NOT UnixODBC_FOUND)
    SET(UnixODBC_DIR_MESSAGE "UnixODBC was not found. Make sure UnixODBC_LIBRARY and UnixODBC_INCLUDE_DIR are set.")
    IF (NOT UnixODBC_FIND_QUIETLY)
        MESSAGE(STATUS "${UnixODBC_DIR_MESSAGE}")
    ELSE (NOT UnixODBC_FIND_QUIETLY)
        IF (UnixODBC_FIND_REQUIRED)
            MESSAGE(FATAL_ERROR "${UnixODBC_DIR_MESSAGE}")
        ENDIF (UnixODBC_FIND_REQUIRED)
    ENDIF (NOT UnixODBC_FIND_QUIETLY)
ENDIF (NOT UnixODBC_FOUND)

MARK_AS_ADVANCED(UnixODBC_INCLUDE_DIRS)
MARK_AS_ADVANCED(UnixODBC_LIBRARIES)
