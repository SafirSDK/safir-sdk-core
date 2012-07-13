#
# Find the ACE client includes and library
# 

# This module defines
# ACE_INCLUDE_DIRECTORIES, where to find ace.h
# ACE_LIBRARY, the libraries to link against
# ACE_FOUND, if false, you cannot build anything that requires ACE

# also defined, but not for general use are
# ACE_LIBRARY, where to find the ACE library.

set( ACE_FOUND 0 )

FIND_PATH( ACE_INCLUDE_DIRECTORIES
  NAMES
    ace/ACE.h
  PATHS
    /usr/include
    /usr/include/ace
    /usr/local/include
    /usr/local/include/ace
    $ENV{ACE_ROOT}
    $ENV{ACE_ROOT}/include
    $ENV{SAFIR_SDK}/include
DOC
  "Specify include-directories that might contain ace.h here."
)
FIND_LIBRARY( ACE_LIBRARY 
  NAMES
    ace ACE
  PATHS
    /usr/lib
    /usr/lib/ace
    /usr/local/lib
    /usr/local/lib/ace
    /usr/local/ace/lib
    $ENV{SAFIR_SDK}/lib
    $ENV{ACE_ROOT}/lib
    $ENV{ACE_ROOT}
  DOC "Specify library-locations that might contain the ACE library here."
)
if (MSVC)
  FIND_LIBRARY( ACE_DEBUG_LIBRARY 
    NAMES
      ACEd
    PATHS
      $ENV{ACE_ROOT}/lib
      $ENV{ACE_ROOT}
      $ENV{SAFIR_SDK}/lib
    DOC "Specify library-locations that might contain the ACE debug library here."
  )
endif()

if ( ACE_LIBRARY )
  if ( ACE_INCLUDE_DIRECTORIES )
    set( ACE_FOUND 1 )
    message( STATUS "Found ACE library: ${ACE_LIBRARY}")
    message( STATUS "Found ACE headers: ${ACE_INCLUDE_DIRECTORIES}")
    if (ACE_DEBUG_LIBRARY)
      set(ACE_LIBRARY debug ${ACE_DEBUG_LIBRARY} optimized ${ACE_LIBRARY})
    endif()
  endif ()
endif ()

mark_as_advanced( ACE_FOUND ACE_LIBRARY ACE_INCLUDE_DIRECTORIES )
message("ACE_LIBRARY is ${ACE_LIBRARY}")