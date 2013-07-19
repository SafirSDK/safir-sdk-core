FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(swreport_send_cpp_test ${sources})

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(swreport_sending_cpp swre_interface_cpp ${Boost_LIBRARIES})
ENDIF ()

ADD_TEST(swreport_send_cpp ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ${CMAKE_CURRENT_SOURCE_DIR}/testconf)
