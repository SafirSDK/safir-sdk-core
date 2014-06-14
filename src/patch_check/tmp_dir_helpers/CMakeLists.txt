include(CheckCXXSourceCompiles)

unset(tmp_dir_helpers_patched CACHE)
CHECK_CXX_SOURCE_COMPILES("#include \"${CMAKE_CURRENT_SOURCE_DIR}/test.cpp\"" tmp_dir_helpers_patched)
  
if(NOT tmp_dir_helpers_patched)
message (FATAL_ERROR 
    "${Boost_INCLUDE_DIRS}/boost/interprocess/detail/tmp_dir_helpers.hpp does not appear to be patched.\n"
    "Please follow the included instructions for patching your Boost headers.")
endif()
