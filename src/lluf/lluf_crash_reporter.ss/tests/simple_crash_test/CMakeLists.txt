#we need to disable some warnings, since the crasher does some wierd things.
if (NOT MSVC)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-pedantic")
endif()


ADD_EXECUTABLE(crasher crasher.cpp)

TARGET_LINK_LIBRARIES (crasher PRIVATE lluf_crash_reporter)

ADD_TEST(NAME CrashReporter_simple COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py --crasher-exe $<TARGET_FILE:crasher>)

SET_SAFIR_TEST_PROPERTIES(TEST CrashReporter_simple)
