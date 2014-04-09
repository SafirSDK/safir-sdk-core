FILE(MAKE_DIRECTORY obj)

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})

ADD_CUSTOM_TARGET(douf_test_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P douf_test_ada.gpr 
                  COMMENT "Building douf_test_ada using gnatmake"
                  VERBATIM)

ADD_TEST(TimeProvider_ada_test obj/douf_test_ada)
SET_SAFIR_TEST_PROPERTIES(TEST TimeProvider_ada_test)
