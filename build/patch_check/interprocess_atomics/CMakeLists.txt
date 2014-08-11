set(atomic_path ${Boost_INCLUDE_DIRS}/boost/interprocess/detail/atomic.hpp)

file(STRINGS ${atomic_path} FOUND_COMMENTS
  REGEX "//Patched for Safir SDK Core")

list(LENGTH FOUND_COMMENTS NUM_COMMENTS)

if(NOT NUM_COMMENTS EQUAL 2)
  message (FATAL_ERROR 
    "${atomic_path} does not appear to be patched.\n"
    "Please follow the included instructions for patching your Boost headers.\n"
    "Then remove CMakeCache.txt from the build/patch_check directory.")
endif()
