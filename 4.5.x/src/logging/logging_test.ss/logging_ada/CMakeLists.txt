FILE(MAKE_DIRECTORY obj)

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})

ADD_CUSTOM_TARGET(log_sender_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P sender.gpr 
                  COMMENT "Building sender using gnatmake"
                  VERBATIM)

ADD_TEST(logging_ada ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(logging_ada 120)
