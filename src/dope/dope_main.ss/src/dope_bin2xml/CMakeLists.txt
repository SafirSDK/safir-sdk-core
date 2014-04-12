ADD_EXECUTABLE(dope_bin2xml dope_bin2xml.cpp)

TARGET_LINK_LIBRARIES(dope_bin2xml PRIVATE
  olib
  douf_time_cpp
  dots_generated-olib-cpp
  dots_generated-Safir-cpp
  dots_cpp
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${ODBC_LIBRARIES})

INSTALL(TARGETS dope_bin2xml
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dope_bin2xml)
