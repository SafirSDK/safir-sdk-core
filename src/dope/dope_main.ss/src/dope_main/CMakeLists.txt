FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dope_main ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS filesystem system thread)

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dope_main dots_cpp dots_generated-Safir-cpp dots_generated-cpp dose_cpp swre_interface_cpp olib ${Boost_LIBRARIES} ${ODBC_LIBRARIES})
endif()

INSTALL(TARGETS dope_main
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dope_main)
