PROJECT (swre_interface_ada)

SET(build_directories program/lib-obj/release program/lib-obj/release-lib program/lib-obj/debug program/lib-obj/debug-lib)

FILE(MAKE_DIRECTORY ${build_directories})

SET(gnatmake_arguments -q -XBUILD=release -XWarning_Level=Default)
ADD_CUSTOM_TARGET(swre_interface_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P program/swre_interface_ada_library.gpr 
                  COMMENT "Building swre_interface_ada using gnatmake"
                  VERBATIM)

SET(gnatmake_arguments_dbg -q -XBUILD=debug -XWarning_Level=Default)
ADD_CUSTOM_TARGET(swre_interface_adad ALL
                  COMMAND gnatmake ${gnatmake_arguments_dbg} -P program/swre_interface_ada_library.gpr 
                  COMMENT "Building swre_interface_adad using gnatmake"
                  VERBATIM)

INSTALL(DIRECTORY shared/
        DESTINATION $ENV{SAFIR_SDK}/ada/swre_interface_ada/
        PATTERN ".svn" EXCLUDE
        PATTERN "*.adb" EXCLUDE)

INSTALL(DIRECTORY program/lib-obj/release-lib/
        DESTINATION $ENV{SAFIR_SDK}/ada/swre_interface_ada/lib-obj/release/
        PATTERN "*.so" EXCLUDE)

INSTALL(DIRECTORY program/lib-obj/debug-lib/
        DESTINATION $ENV{SAFIR_SDK}/ada/swre_interface_ada/lib-obj/debug/
        PATTERN "*.so" EXCLUDE)

INSTALL(FILES program/lib-obj/release-lib/libswre_interface_ada.so
              program/lib-obj/debug-lib/libswre_interface_adad.so
        DESTINATION $ENV{SAFIR_RUNTIME}/lib/)

INSTALL(FILES data/swre_interface_ada.gpr
        DESTINATION $ENV{SAFIR_SDK}/ada/)
