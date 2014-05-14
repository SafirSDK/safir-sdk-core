add_executable(tracer_backdoor_sender sender.cpp)

target_link_libraries(tracer_backdoor_sender PRIVATE
  dots_generated-Safir-cpp
  swre_interface_cpp 
  douf_time_cpp
  dose_cpp 
  dose_internal
  dots_cpp
  lluf_utils
  ${Boost_SYSTEM_LIBRARY})

#Needed on windows
if (MSVC)
  TARGET_LINK_LIBRARIES(tracer_backdoor_sender PRIVATE ${Boost_DATE_TIME_LIBRARY})
endif()


get_property(DOTS_GENERATED_PATHS GLOBAL PROPERTY DOTS_GENERATED_PATHS)

ADD_TEST(NAME tracer_backdoor
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --sender $<TARGET_FILE:tracer_backdoor_sender>
  --backdoor $<TARGET_FILE:backdoor>
  --dose-main $<TARGET_FILE:dose_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --dots-generated-paths "${DOTS_GENERATED_PATHS}")

SET_SAFIR_TEST_PROPERTIES(TEST tracer_backdoor TIMEOUT 120
                          CONFIG_OVERRIDE "${CMAKE_CURRENT_SOURCE_DIR}/test_config")
