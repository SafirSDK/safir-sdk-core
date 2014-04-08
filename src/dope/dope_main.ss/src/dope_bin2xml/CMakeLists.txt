ADD_EXECUTABLE(dope_bin2xml dope_bin2xml.cpp)

FIND_PACKAGE(Boost COMPONENTS program_options filesystem system)

TARGET_LINK_LIBRARIES(dope_bin2xml dots_cpp dots_generated-olib-cpp dots_generated-Safir-cpp olib ${Boost_LIBRARIES} ${ODBC_LIBRARIES})

INSTALL(TARGETS dope_bin2xml
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dope_bin2xml)
