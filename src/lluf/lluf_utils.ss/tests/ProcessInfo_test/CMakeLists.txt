FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ProcessInfo_test ProcessInfo_test.cpp)
ADD_EXECUTABLE(ProcessInfoSleeper ProcessInfoSleeper.cpp)

TARGET_LINK_LIBRARIES (ProcessInfo_test PRIVATE lluf_utils)

TARGET_LINK_LIBRARIES (ProcessInfoSleeper PRIVATE ${Boost_THREAD_LIBRARY} ${Boost_SYSTEM_LIBRARY})

ADD_TEST(NAME ProcessInfo COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
SET_SAFIR_TEST_PROPERTIES(TEST ProcessInfo)
