#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(logging_library SHARED ${sources} ${headers})

target_include_directories(logging_library PUBLIC .)

TARGET_LINK_LIBRARIES(logging_library PRIVATE lluf_utils)

SAFIR_INSTALL(TARGETS logging_library INCLUDE_DIR Safir)

#TODO
# INSTALL(FILES ../data/logging_library.gpr
#         DESTINATION ${SAFIR_SDK}/ada)


# INSTALL_DEBUG_INFO(logging_library)
