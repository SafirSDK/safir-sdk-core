FILE(MAKE_DIRECTORY obj)

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})

ADD_CUSTOM_TARGET(douf_test_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P douf_test_ada.gpr 
                  COMMENT "Building douf_test_ada using gnatmake"
                  VERBATIM)

ADD_TEST(TimeProvider_ada_test obj/douf_test_ada)
SET_TESTS_PROPERTIES(TimeProvider_ada_test PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
