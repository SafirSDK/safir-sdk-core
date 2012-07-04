cmake_minimum_required(VERSION 2.6)

PROJECT (dose_test_ada NONE)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(MAKE_DIRECTORY obj)

#SET(gnatmake_arguments -L/home/lars/safir_sdk_core/src/dose/dose_test.ss/util/dose_test_util/ -aP/home/lars/safir_sdk_core/src/dose/dose_test.ss/util/dose_test_util/ -q -XBUILD=${CUSTOM_BUILD_TYPE})
SET(gnatmake_arguments -L${dose_test_util_BINARY_DIR} -aP${dose_test_util_SOURCE_DIR} -q -XBUILD=${CUSTOM_BUILD_TYPE})

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

