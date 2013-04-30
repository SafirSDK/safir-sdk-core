set(offset_ptr_path ${Boost_INCLUDE_DIRS}/boost/interprocess/offset_ptr.hpp)

file(STRINGS ${offset_ptr_path} FOUND_ATTRIBUTE_NOINLINE
  REGEX "__attribute__.*noinline")

file(STRINGS ${offset_ptr_path} FOUND_NEVER_INLINE
  REGEX "BOOST_INTERPROCESS_NEVER_INLINE")

if(NOT FOUND_ATTRIBUTE_NOINLINE AND NOT FOUND_NEVER_INLINE)
  message (FATAL_ERROR 
    "${offset_ptr_path} does not appear to be patched.\n"
    "Please follow the included instructions for patching your Boost headers.\n"
    "Then remove CMakeCache.txt from the build/patch_check directory.")
endif()
