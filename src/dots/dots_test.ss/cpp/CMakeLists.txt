FILE(GLOB sources *.cpp)

#Container tests have a lot of unused values...
if (UNIX)
  SET(CMAKE_CXX_FLAGS "-Wno-unused-value")
endif()

ADD_EXECUTABLE(dots_test_cpp ${sources})

TARGET_LINK_LIBRARIES(dots_test_cpp PRIVATE 
  dots_generated-DotsTest-cpp
  dots_cpp 
  ${Boost_FILESYSTEM_LIBRARY})
