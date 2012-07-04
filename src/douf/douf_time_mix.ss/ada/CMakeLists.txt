cmake_minimum_required(VERSION 2.6)

PROJECT (douf_time_ada NONE)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

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

INSTALL(DIRECTORY program/interface/
        DESTINATION ${SAFIR_SDK}/ada/douf_time_ada/interface)

INSTALL(DIRECTORY program/ali/
        DESTINATION ${SAFIR_SDK}/ada/douf_time_ada/ali)
        
if (WIN32)
   INSTALL(FILES program/lib/libdouf_time_ada.dll
           DESTINATION ${SAFIR_RUNTIME}/bin/)
elseif (UNIX)
   INSTALL(FILES program/lib/libdouf_time_ada.so
           DESTINATION ${SAFIR_RUNTIME}/lib/)
endif(WIN32)

INSTALL(FILES data/douf_time_ada.gpr
        DESTINATION ${SAFIR_SDK}/ada/)

