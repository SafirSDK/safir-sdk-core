FILE(GLOB sources *.cpp)

#Container tests have a lot of unused values...
if (UNIX)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-value")
endif()

ADD_EXECUTABLE(dots_test_cpp ${sources})

TARGET_LINK_LIBRARIES(dots_test_cpp PRIVATE
  safir_generated-DotsTest-cpp
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${Boost_FILESYSTEM_LIBRARY}
  )

ADD_DEPENDENCIES(dots_test_cpp safir_generated-DotsTestExtra-cpp)
