cmake_minimum_required(VERSION 2.6)

PROJECT (swre_interface_ada)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

SET(build_directories
  src/lib
  src/obj
  src/ali)

FILE(MAKE_DIRECTORY ${build_directories})

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})
ADD_CUSTOM_TARGET(swre_interface_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P swre_interface_ada_library.gpr 
                  WORKING_DIRECTORY src
                  COMMENT "Building swre_interface_ada using gnatmake"
                  VERBATIM)

FILE(GLOB ads_files src/*.ads)
INSTALL(FILES ${ads_files}
        DESTINATION ${SAFIR_SDK}/ada/swre_interface_ada/interface)

INSTALL(DIRECTORY src/ali/
        DESTINATION ${SAFIR_SDK}/ada/swre_interface_ada/ali)
if (WIN32)
   INSTALL(FILES src/lib/libswre_interface_ada.dll
           DESTINATION ${SAFIR_RUNTIME}/bin/)
elseif (UNIX)
   INSTALL(FILES src/lib/libswre_interface_ada.so
           DESTINATION ${SAFIR_RUNTIME}/lib/)
endif(WIN32)

INSTALL(FILES data/swre_interface_ada.gpr
        DESTINATION ${SAFIR_SDK}/ada/)
