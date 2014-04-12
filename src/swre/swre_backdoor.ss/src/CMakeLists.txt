ADD_EXECUTABLE(backdoor main.cpp)

TARGET_LINK_LIBRARIES(backdoor PRIVATE
  douf_time_cpp
  dose_internal
  dose_cpp
  dots_generated-swre_library-cpp
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

INSTALL(TARGETS backdoor
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(backdoor)
