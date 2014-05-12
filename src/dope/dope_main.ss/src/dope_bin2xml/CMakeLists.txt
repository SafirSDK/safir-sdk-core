ADD_EXECUTABLE(dope_bin2xml dope_bin2xml.cpp)

TARGET_LINK_LIBRARIES(dope_bin2xml PRIVATE
  olib
  douf_time_cpp
  dots_generated-olib-cpp
  dots_generated-Safir-cpp
  dots_cpp
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${SAFIR_ODBC_LIBRARIES})

SAFIR_INSTALL(TARGETS dope_bin2xml)
