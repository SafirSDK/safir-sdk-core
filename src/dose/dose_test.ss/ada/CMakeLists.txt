cmake_minimum_required(VERSION 2.8)

PROJECT (dose_test_ada NONE)

FILE(MAKE_DIRECTORY obj)

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})

ADD_CUSTOM_TARGET(dose_test_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P dose_test_ada.gpr 
                  COMMENT "Building dose_test_ada using gnatmake"
                  VERBATIM)

if (WIN32)
   INSTALL(PROGRAMS obj/dose_test_ada.exe
           DESTINATION ${SAFIR_RUNTIME}/bin/)
elseif (UNIX)
   INSTALL(PROGRAMS obj/dose_test_ada
           DESTINATION ${SAFIR_RUNTIME}/bin/)
endif(WIN32)

