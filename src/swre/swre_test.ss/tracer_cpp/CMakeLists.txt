if (NOT MSVC OR MSVC_VERSION GREATER_EQUAL 1930)
  set(CMAKE_CXX_STANDARD 20)
endif()

add_library(tracer_helper_dll SHARED tracer_helper_dll.cpp)
target_link_libraries(tracer_helper_dll PRIVATE swre_interface_cpp)

add_executable(tracer_sender_cpp sender.cpp)

target_link_libraries(tracer_sender_cpp PRIVATE
  swre_interface_cpp
  tracer_helper_dll)

ADD_TEST(NAME tracer_cpp
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --binary $<TARGET_FILE:tracer_sender_cpp>
  --safir-show-config $<TARGET_FILE:safir_show_config>)


SET_SAFIR_TEST_PROPERTIES(TEST tracer_cpp)
