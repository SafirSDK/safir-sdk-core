PROJECT (douf_time_ada)

SET(build_directories program/lib-obj/release program/lib-obj/release-lib program/lib-obj/debug program/lib-obj/debug-lib)

FILE(MAKE_DIRECTORY ${build_directories})

SET(gnatmake_arguments -q -XBUILD=release -XWarning_Level=Default)
ADD_CUSTOM_TARGET(douf_time_ada ALL
                  COMMAND gnatmake ${gnatmake_arguments} -P program/douf_time_ada_library.gpr 
                  COMMENT "Building douf_time_ada using gnatmake"
                  VERBATIM)

SET(gnatmake_arguments_dbg -q -XBUILD=debug -XWarning_Level=Default)
ADD_CUSTOM_TARGET(douf_time_adad ALL
                  COMMAND gnatmake ${gnatmake_arguments_dbg} -P program/douf_time_ada_library.gpr 
                  COMMENT "Building douf_time_adad using gnatmake"
                  VERBATIM)

INSTALL(DIRECTORY shared/
        DESTINATION $ENV{SAFIR_SDK}/ada/douf_time_ada/
        PATTERN ".svn" EXCLUDE
        PATTERN "*.adb" EXCLUDE)

INSTALL(DIRECTORY program/lib-obj/release-lib/
        DESTINATION $ENV{SAFIR_SDK}/ada/douf_time_ada/lib-obj/release/
        PATTERN "*.so" EXCLUDE)

INSTALL(DIRECTORY program/lib-obj/debug-lib/
        DESTINATION $ENV{SAFIR_SDK}/ada/douf_time_ada/lib-obj/debug/
        PATTERN "*.so" EXCLUDE)

INSTALL(FILES program/lib-obj/release-lib/libdouf_time_ada.so
              program/lib-obj/debug-lib/libdouf_time_adad.so
        DESTINATION $ENV{SAFIR_RUNTIME}/lib/)

INSTALL(FILES data/douf_time_ada.gpr
        DESTINATION $ENV{SAFIR_SDK}/ada/)
