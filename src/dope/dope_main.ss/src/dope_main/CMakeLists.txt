FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dope_main ${sources} ${headers})

TARGET_LINK_LIBRARIES(dope_main PRIVATE 
  swre_interface_cpp 
  olib
  douf_time_cpp
  dose_internal
  dose_cpp
  dots_generated-olib-cpp
  dots_generated-Safir-cpp
  dots_cpp
  logging_cpp
  ${ODBC_LIBRARIES}
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})


# INSTALL(TARGETS dope_main
#         RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
#         LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
#         ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

# INSTALL_DEBUG_INFO(dope_main)
