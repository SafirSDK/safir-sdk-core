ADD_EXECUTABLE(producer producer/main.cpp)
TARGET_LINK_LIBRARIES(producer PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  logging_cpp
  Boost::headers)
SAFIR_INSTALL(TARGETS producer COMPONENT TestSuite)

ADD_EXECUTABLE(consumer consumer/main.cpp)
TARGET_LINK_LIBRARIES(consumer PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  logging_cpp
  Boost::headers)
SAFIR_INSTALL(TARGETS consumer COMPONENT TestSuite)

ADD_TEST(NAME performance_test
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/performance_test.py
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --producer $<TARGET_FILE:producer>
  --consumer $<TARGET_FILE:consumer>
  --num-instances 100
  --num-updates 1000)

SET_SAFIR_TEST_PROPERTIES(TEST performance_test
  TIMEOUT 300
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)
