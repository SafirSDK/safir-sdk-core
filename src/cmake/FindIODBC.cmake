# - Try to find the IODBC library
# Once done, this will define
#
# IODBC_FOUND        - system has IODBC
# IODBC_INCLUDE_DIRS - the IODBC include directories
# IODBC_LIBRARIES    - link these to use IODBC

INCLUDE(LibFindMacros)

# Include dir
FIND_PATH(IODBC_INCLUDE_DIR iodbcunix.h /usr/include /usr/local/include /usr/include/libiodbc /usr/local/include/libiodbc)

# Look for the library
FIND_LIBRARY(IODBC_LIBRARY NAMES iodbc)

# Set the include dir variables and the libraries and let LIBFIND_PROCESS do the rest.
SET(IODBC_PROCESS_INCLUDES IODBC_INCLUDE_DIR)
set(IODBC_PROCESS_LIBS IODBC_LIBRARY)
LIBFIND_PROCESS(IODBC)

MARK_AS_ADVANCED(IODBC_INCLUDE_DIRS)
MARK_AS_ADVANCED(IODBC_LIBRARIES)

