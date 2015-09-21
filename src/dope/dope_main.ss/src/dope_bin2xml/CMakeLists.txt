ADD_EXECUTABLE(dope_bin2xml dope_bin2xml.cpp)

FIND_PACKAGE(Boost COMPONENTS program_options filesystem system)

if (MSVC)
    TARGET_LINK_LIBRARIES(dope_bin2xml odbc32)
else()
    TARGET_LINK_LIBRARIES(dope_bin2xml dots_cpp dots_generated-Safir-cpp dots_generated-cpp ${Boost_LIBRARIES} ${ODBC_LIBRARIES})
endif()

INSTALL(TARGETS dope_bin2xml
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dope_bin2xml)
