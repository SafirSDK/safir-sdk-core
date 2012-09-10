FILE(MAKE_DIRECTORY obj)

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})

ADD_CUSTOM_TARGET(dots_test_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P dots_test_ada.gpr 
                  COMMENT "Building dots_test_ada using gnatmake"
                  VERBATIM)

