
add_library(test_library SHARED test_library.cpp)

ADD_EXECUTABLE(DynamicLibraryLoader_test1 test1.cpp)
ADD_EXECUTABLE(DynamicLibraryLoader_test2 test2.cpp)
ADD_EXECUTABLE(DynamicLibraryLoader_test3 test3.cpp)
ADD_EXECUTABLE(DynamicLibraryLoader_test4 test4.cpp)

TARGET_LINK_LIBRARIES (DynamicLibraryLoader_test1 lluf_utils)
TARGET_LINK_LIBRARIES (DynamicLibraryLoader_test2 lluf_utils)
TARGET_LINK_LIBRARIES (DynamicLibraryLoader_test3 lluf_utils)
TARGET_LINK_LIBRARIES (DynamicLibraryLoader_test4 lluf_utils)

ADD_TEST(NAME DynamicLibraryLoader
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --test-exe-1 $<TARGET_FILE:DynamicLibraryLoader_test1>
  --test-exe-2 $<TARGET_FILE:DynamicLibraryLoader_test2>
  --test-exe-3 $<TARGET_FILE:DynamicLibraryLoader_test3>
  --test-exe-4 $<TARGET_FILE:DynamicLibraryLoader_test4>)

SET_SAFIR_TEST_PROPERTIES(DynamicLibraryLoader 40)
