INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_test_cpp ${sources})

FIND_PACKAGE(Boost COMPONENTS filesystem system)

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dots_test_cpp dots_cpp dots_generated-DotsTest-cpp dots_generated-cpp ${Boost_LIBRARIES})
endif()
