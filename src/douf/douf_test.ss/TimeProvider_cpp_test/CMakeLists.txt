ADD_EXECUTABLE(TimeProvider_cpp_test test.cpp)

TARGET_LINK_LIBRARIES(TimeProvider_cpp_test PRIVATE
  douf_time_cpp
  dots_cpp
  dots_internal
  lluf_utils)

#Needed on windows
if (MSVC)
  TARGET_LINK_LIBRARIES(TimeProvider_cpp_test PRIVATE ${Boost_DATE_TIME_LIBRARY})
endif()


ADD_TEST(TimeProvider_cpp_test TimeProvider_cpp_test)

SET_SAFIR_TEST_PROPERTIES(TEST TimeProvider_cpp_test 
                          CONFIG_OVERRIDE "${CMAKE_CURRENT_SOURCE_DIR}/../config"
                          ENVIRONMENT
                          "SAFIR_SOURCE_ROOT=${safir_sdk_core_SOURCE_DIR}/../")
