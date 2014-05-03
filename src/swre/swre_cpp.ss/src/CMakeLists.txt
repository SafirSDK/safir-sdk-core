FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(swre_interface_cpp SHARED ${sources} ${headers})

target_include_directories(swre_interface_cpp PUBLIC .)

TARGET_LINK_LIBRARIES(swre_interface_cpp PRIVATE 
  swre_library
  dose_internal
  dose_cpp
  dots_generated-swre_library-cpp
  dots_cpp
  dots_internal
  logging_cpp
  lluf_utils
  ${Boost_REGEX_LIBRARY})

SAFIR_INSTALL(TARGETS swre_interface_cpp INCLUDE_DIR Safir)
