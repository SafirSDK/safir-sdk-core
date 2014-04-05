#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(swre_library SHARED ${sources} ${headers})

target_include_directories(swre_library PUBLIC .)

TARGET_LINK_LIBRARIES(swre_library douf_time_cpp dose_cpp dots_generated-swre_library-cpp logging_cpp lluf_utils lluf_crash_reporter)

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS swre_library
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES ../data/swre_library.gpr
        DESTINATION ${SAFIR_SDK}/ada)


INSTALL_DEBUG_INFO(swre_library)
