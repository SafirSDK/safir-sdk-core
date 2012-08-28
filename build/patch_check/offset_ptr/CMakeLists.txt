set(offset_ptr_path ${Boost_INCLUDE_DIRS}/boost/interprocess/offset_ptr.hpp)

file(STRINGS ${offset_ptr_path} FOUND_ATTRIBUTE_NOINLINE
  REGEX "__attribute__.*noinline")

if("${FOUND_ATTRIBUTE_NOINLINE}" STREQUAL "")
  message (FATAL_ERROR 
    "${offset_ptr_path} does not appear to be patched.\n"
    "Please follow the included instructions for patching your Boost headers."
    "Then remove CMakeCache.txt from the build/patch_check directory.")
endif()
