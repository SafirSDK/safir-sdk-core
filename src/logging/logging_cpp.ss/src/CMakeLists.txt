FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(logging_cpp SHARED ${sources} ${headers})

target_include_directories(logging_cpp PUBLIC .)

TARGET_LINK_LIBRARIES(logging_cpp PRIVATE logging_library lluf_utils)

SAFIR_INSTALL(TARGETS logging_cpp INCLUDE_DIR Safir)
