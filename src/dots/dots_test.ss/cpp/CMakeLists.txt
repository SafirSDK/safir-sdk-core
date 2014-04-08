FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_test_cpp ${sources})

FIND_PACKAGE(Boost COMPONENTS filesystem system)

TARGET_LINK_LIBRARIES(dots_test_cpp dots_generated-DotsTest-cpp dots_cpp ${Boost_LIBRARIES})
