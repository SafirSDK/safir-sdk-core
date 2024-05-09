FILE(GLOB sources *.cpp)

#Container tests have a lot of unused values, and gcc gives a spurious warning about array bounds.
if (UNIX)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-value -Wno-array-bounds")
endif()

ADD_EXECUTABLE(dots_test_cpp ${sources})

TARGET_LINK_LIBRARIES(dots_test_cpp PRIVATE
  safir_generated-DotsTest-cpp
  Boost::unit_test_framework
  Boost::filesystem
)

ADD_DEPENDENCIES(dots_test_cpp safir_generated-DotsTestExtra-cpp)
