include(CheckCXXSourceCompiles)
FIND_PACKAGE(Boost COMPONENTS thread)

if (NOT MSVC)
  set(CMAKE_REQUIRED_LIBRARIES ${Boost_LIBRARIES})
else()
  set(ENV{LIB} "$ENV{LIB};${Boost_LIBRARY_DIRS}")
endif()


CHECK_CXX_SOURCE_COMPILES("#include \"${CMAKE_CURRENT_SOURCE_DIR}/test.cpp\"" locks_patched)

if(NOT locks_patched)
message (FATAL_ERROR 
    "${Boost_INCLUDE_DIRS}/boost/thread/locks.hpp does not appear to be patched.\n"
    "Please follow the included instructions for patching your Boost headers.\n"
    "Then remove CMakeCache.txt from the build/patch_check directory.")
endif()

