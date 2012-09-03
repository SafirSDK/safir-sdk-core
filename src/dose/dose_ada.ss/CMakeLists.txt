cmake_minimum_required(VERSION 2.8)

if (NOT SAFIR_ADA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_target ALL
                       COMMENT "SAFIR_ADA_SUPPORT set to false, dose_ada will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_ADA_SUPPORT set to false, dose_ada will not be installed.\")")
     return()
endif (NOT SAFIR_ADA_SUPPORT)


PROJECT (dose_ada NONE)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

SET(build_directories
      src/lib
      src/obj
      src/ali
      src/interface)

FILE(MAKE_DIRECTORY ${build_directories})

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})
ADD_CUSTOM_TARGET(dose_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P dose_ada_library.gpr 
                  WORKING_DIRECTORY src
                  COMMENT "Building dose_ada using gnatmake"
                  VERBATIM)

INSTALL(DIRECTORY src/interface/
        DESTINATION ${SAFIR_SDK}/ada/dose_ada/interface)

INSTALL(DIRECTORY src/ali/
        DESTINATION ${SAFIR_SDK}/ada/dose_ada/ali)
if (WIN32)
   INSTALL(FILES src/lib/libdose_ada.dll
           DESTINATION ${SAFIR_RUNTIME}/bin/)
elseif (UNIX)
   INSTALL(FILES src/lib/libdose_ada.so
           DESTINATION ${SAFIR_RUNTIME}/lib/)
endif(WIN32)

INSTALL(FILES data/dose_ada.gpr
        DESTINATION ${SAFIR_SDK}/ada/)
