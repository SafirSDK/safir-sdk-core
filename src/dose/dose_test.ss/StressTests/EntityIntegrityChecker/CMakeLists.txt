FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(EntityIntegrityChecker ${sources} ${headers})

TARGET_LINK_LIBRARIES(EntityIntegrityChecker ${COMMON_LIBRARIES})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(EntityIntegrityChecker dose_cpp dots_generated-DoseIntegrityCheck-cpp dots_generated-Safir-cpp dots_cpp lluf_utils ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS EntityIntegrityChecker
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(EntityIntegrityChecker)
