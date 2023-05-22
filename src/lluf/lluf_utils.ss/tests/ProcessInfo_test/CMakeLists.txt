FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ProcessInfo_test ProcessInfo_test.cpp)
ADD_EXECUTABLE(ProcessInfoSleeper ProcessInfoSleeper.cpp)

TARGET_LINK_LIBRARIES (ProcessInfo_test PRIVATE lluf_utils)

TARGET_LINK_LIBRARIES (ProcessInfoSleeper PRIVATE Boost::thread)

ADD_TEST(NAME ProcessInfo 
         COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
                   --test-exe $<TARGET_FILE:ProcessInfo_test>
                   --sleeper-exe $<TARGET_FILE:ProcessInfoSleeper>)

SET_SAFIR_TEST_PROPERTIES(TEST ProcessInfo)
