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
  lluf_utils)

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS swre_interface_cpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(swre_interface_cpp)
