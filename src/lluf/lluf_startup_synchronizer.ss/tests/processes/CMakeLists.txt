FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ss_test ${sources})

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(ss_test lluf_startup_synchronizer ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(StartupSynchronizer_processes ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
SET_SAFIR_TEST_PROPERTIES(StartupSynchronizer_processes 120)
