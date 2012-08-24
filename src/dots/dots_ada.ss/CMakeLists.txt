cmake_minimum_required(VERSION 2.6)

if (NOT SAFIR_ADA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_target ALL
                       COMMENT "SAFIR_ADA_SUPPORT set to false, dots_ada will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_ADA_SUPPORT set to false, dots_ada will not be installed.\")")
     return()
endif (NOT SAFIR_ADA_SUPPORT)
   
PROJECT (dots_ada NONE)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

SET(build_directories
      program/lib
      program/obj
      program/ali
      program/interface)

FILE(MAKE_DIRECTORY ${build_directories})

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})  

ADD_CUSTOM_TARGET(dots_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P dots_ada_library.gpr 
                  WORKING_DIRECTORY program
                  COMMENT "Building dots_ada using gnatmake"
                  VERBATIM)

INSTALL(DIRECTORY program/interface/
        DESTINATION ${SAFIR_SDK}/ada/dots_ada/interface)

INSTALL(DIRECTORY program/ali/
        DESTINATION ${SAFIR_SDK}/ada/dots_ada/ali)
if (WIN32)
   INSTALL(FILES program/lib/libdots_ada.dll
           DESTINATION ${SAFIR_RUNTIME}/bin/)
elseif (UNIX)
   INSTALL(FILES program/lib/libdots_ada.so
           DESTINATION ${SAFIR_RUNTIME}/lib/)
endif(WIN32)

INSTALL(FILES data/dots_ada.gpr
        DESTINATION ${SAFIR_SDK}/ada/)
