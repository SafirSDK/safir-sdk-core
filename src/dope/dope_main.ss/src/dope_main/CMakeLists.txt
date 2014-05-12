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
  ${SAFIR_ODBC_LIBRARIES}
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS dope_main)
