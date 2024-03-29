#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

#we need to disable some warnings, since these things contain wierdness
if (NOT MSVC)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-pedantic")
endif()


add_library(test_library SHARED test_library.cpp)

ADD_EXECUTABLE(DynamicLibraryLoader_test1 test1.cpp)
ADD_EXECUTABLE(DynamicLibraryLoader_test2 test2.cpp)
ADD_EXECUTABLE(DynamicLibraryLoader_test3 test3.cpp)
ADD_EXECUTABLE(DynamicLibraryLoader_test4 test4.cpp)

TARGET_LINK_LIBRARIES (DynamicLibraryLoader_test1 PRIVATE lluf_utils)
TARGET_LINK_LIBRARIES (DynamicLibraryLoader_test2 PRIVATE lluf_utils)
TARGET_LINK_LIBRARIES (DynamicLibraryLoader_test3 PRIVATE lluf_utils)
TARGET_LINK_LIBRARIES (DynamicLibraryLoader_test4 PRIVATE lluf_utils)

ADD_TEST(NAME DynamicLibraryLoader
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --test-exe-1 $<TARGET_FILE:DynamicLibraryLoader_test1>
  --test-exe-2 $<TARGET_FILE:DynamicLibraryLoader_test2>
  --test-exe-3 $<TARGET_FILE:DynamicLibraryLoader_test3>
  --test-exe-4 $<TARGET_FILE:DynamicLibraryLoader_test4>)

SET_SAFIR_TEST_PROPERTIES(TEST DynamicLibraryLoader)
