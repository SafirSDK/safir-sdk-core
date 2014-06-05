set(SAFIR_SDK_CORE_FOUND False)

FIND_PATH(SAFIR_SDK_CORE_INCLUDE_DIRECTORIES
  NAMES
    Safir/Dob/Connection.h
  PATHS
    /usr/include
    /usr/local/include
DOC
  "Specify include-directories that might contain Safir/Dob/Connection.h here."
)

if (NOT MSVC)
  FIND_LIBRARY(SAFIR_DOB_LIBRARY
    NAMES dose_cpp
    PATHS
      /usr/lib
      /usr/local/lib
    DOC "Specify library-locations that might contain the dose_cpp library here."
)
endif()

if ( SAFIR_DOB_LIBRARY )
  if ( SAFIR_SDK_CORE_INCLUDE_DIRECTORIES )
    set( SAFIR_SDK_CORE_FOUND True )
    message( STATUS "Found Safir Dob library: ${SAFIR_DOB_LIBRARY}")
    message( STATUS "Found Safir SDK Core headers: ${SAFIR_SDK_CORE_INCLUDE_DIRECTORIES}")
  endif ()
endif ()

mark_as_advanced(SAFIR_SDK_CORE_FOUND SAFIR_DOB_LIBRARY SAFIR_SDK_CORE_INCLUDE_DIRECTORIES)
