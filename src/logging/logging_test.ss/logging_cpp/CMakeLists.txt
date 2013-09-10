add_executable(log_sender sender.cpp)

if (NOT MSVC)
  target_link_libraries(log_sender logging_cpp)
endif()

ADD_TEST(logging_cpp ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(logging_cpp 120)
