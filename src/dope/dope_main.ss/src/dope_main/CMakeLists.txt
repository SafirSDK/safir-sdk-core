FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dope_main ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS filesystem system thread)

TARGET_LINK_LIBRARIES(dope_main swre_interface_cpp olib  dose_cpp dots_generated-olib-cpp dots_generated-Safir-cpp dots_cpp logging_cpp ${Boost_LIBRARIES} ${ODBC_LIBRARIES})

INSTALL(TARGETS dope_main
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dope_main)
