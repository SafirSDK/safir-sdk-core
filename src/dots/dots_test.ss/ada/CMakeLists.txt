if (NOT SAFIR_ADA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_dots_test_ada ALL
                       COMMENT "SAFIR_ADA_SUPPORT set to false, dots_test_ada will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_ADA_SUPPORT set to false, dots_test_ada will not be installed.\")")
     return()
endif (NOT SAFIR_ADA_SUPPORT)

FILE(MAKE_DIRECTORY obj)

SET(gnatmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE})

ADD_CUSTOM_TARGET(dots_test_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P dots_test_ada.gpr 
                  COMMENT "Building dots_test_ada using gnatmake"
                  VERBATIM)

