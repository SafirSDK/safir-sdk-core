
add_library(test_library SHARED test_library.cpp)

ADD_EXECUTABLE(DynamicLibraryLoader_test1 test1.cpp)
ADD_EXECUTABLE(DynamicLibraryLoader_test2 test2.cpp)
ADD_EXECUTABLE(DynamicLibraryLoader_test3 test3.cpp)
ADD_EXECUTABLE(DynamicLibraryLoader_test4 test4.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (DynamicLibraryLoader_test1 lluf_utils)
  TARGET_LINK_LIBRARIES (DynamicLibraryLoader_test2 lluf_utils)
  TARGET_LINK_LIBRARIES (DynamicLibraryLoader_test3 lluf_utils)
  TARGET_LINK_LIBRARIES (DynamicLibraryLoader_test4 lluf_utils)
ENDIF()

ADD_TEST(DynamicLibraryLoader ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
SET_TESTS_PROPERTIES(DynamicLibraryLoader PROPERTIES TIMEOUT 40)
SET_TESTS_PROPERTIES(DynamicLibraryLoader PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
