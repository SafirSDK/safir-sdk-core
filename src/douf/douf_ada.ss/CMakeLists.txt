cmake_minimum_required(VERSION 2.8)

PROJECT (douf_time_ada NONE)

if (NOT SAFIR_ADA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_target ALL
                       COMMENT "SAFIR_ADA_SUPPORT set to false, douf_time_ada will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_ADA_SUPPORT set to false, douf_time_ada will not be installed.\")")
     return()
endif ()

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

SET(build_directories
    program/lib
    program/obj
    program/ali
    program/interface)

FILE(MAKE_DIRECTORY ${build_directories})

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})
ADD_CUSTOM_TARGET(douf_time_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P douf_time_ada_library.gpr 
                  WORKING_DIRECTORY program
                  COMMENT "Building douf_time_ada using gnatmake"
                  VERBATIM)

# INSTALL(DIRECTORY program/interface/
#         DESTINATION ${SAFIR_SDK}/ada/douf_time_ada/interface)

# INSTALL(DIRECTORY program/ali/
#         DESTINATION ${SAFIR_SDK}/ada/douf_time_ada/ali)
        
# if (WIN32)
#    INSTALL(FILES program/lib/libdouf_time_ada.dll
#            DESTINATION ${SAFIR_RUNTIME}/bin/)
# elseif (UNIX)
#    INSTALL(FILES program/lib/libdouf_time_ada.so
#            DESTINATION ${SAFIR_RUNTIME}/lib/)
# endif(WIN32)

# INSTALL(FILES data/douf_time_ada.gpr
#         DESTINATION ${SAFIR_SDK}/ada/)

