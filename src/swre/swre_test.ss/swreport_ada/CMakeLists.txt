FILE(MAKE_DIRECTORY obj)

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})

ADD_CUSTOM_TARGET(swreport_sender_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P sender.gpr 
                  COMMENT "Building sender using gnatmake"
                  VERBATIM)

ADD_TEST(NAME swreport_ada COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST swreport_ada TIMEOUT 120)
