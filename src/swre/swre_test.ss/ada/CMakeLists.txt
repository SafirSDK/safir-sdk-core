cmake_minimum_required(VERSION 2.6)

if (NOT SAFIR_ADA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_target ALL
                       COMMENT "SAFIR_ADA_SUPPORT set to false, swre_test_ada will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_ADA_SUPPORT set to false, swre_test_ada will not be installed.\")")
     return()
endif (NOT SAFIR_ADA_SUPPORT)

PROJECT (swre_test_ada)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

SET(build_directories
       obj)
#      src/lib
#      src/obj
#      src/ali)

FILE(MAKE_DIRECTORY ${build_directories})

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})
ADD_CUSTOM_TARGET(swre_test_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P swre_test_ada.gpr 
                  COMMENT "Building swre_interface_ada using gnatmake"
                  VERBATIM)

if (WIN32)
   INSTALL(PROGRAMS obj/swre_test_ada.exe
           DESTINATION ${SAFIR_RUNTIME}/bin/)
elseif (UNIX)
   INSTALL(PROGRAMS obj/swre_test_ada
           DESTINATION ${SAFIR_RUNTIME}/bin/)
endif(WIN32)
