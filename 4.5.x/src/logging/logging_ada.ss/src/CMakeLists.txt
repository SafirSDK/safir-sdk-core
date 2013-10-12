cmake_minimum_required(VERSION 2.8)

PROJECT (logging_ada NONE)

if (NOT SAFIR_ADA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_target ALL
                       COMMENT "SAFIR_ADA_SUPPORT set to false, logging_ada will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_ADA_SUPPORT set to false, logging_ada will not be installed.\")")
     return()
endif ()

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

SET(build_directories
  lib
  obj
  ali)

FILE(MAKE_DIRECTORY ${build_directories})

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})
ADD_CUSTOM_TARGET(logging_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P logging_ada_library.gpr 
                  WORKING_DIRECTORY .
                  COMMENT "Building logging_ada using gnatmake"
                  VERBATIM)

FILE(GLOB ads_files *.ads)
INSTALL(FILES ${ads_files}
        DESTINATION ${SAFIR_SDK}/ada/logging_ada/interface)

INSTALL(DIRECTORY ali/
        DESTINATION ${SAFIR_SDK}/ada/logging_ada/ali)
if (WIN32)
   INSTALL(FILES lib/liblogging_ada.dll
           DESTINATION ${SAFIR_RUNTIME}/bin/)
elseif (UNIX)
   INSTALL(FILES lib/liblogging_ada.so
           DESTINATION ${SAFIR_RUNTIME}/lib/)
endif(WIN32)

INSTALL(FILES ../data/logging_ada.gpr
        DESTINATION ${SAFIR_SDK}/ada/)
