include(CheckCXXSourceCompiles)

if (CMAKE_COMPILER_IS_GNUCXX)
  SET(CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS} -Werror)
endif()

CHECK_CXX_SOURCE_COMPILES("#include \"${CMAKE_CURRENT_SOURCE_DIR}/test.cpp\"" arm_swp_deprecated)

if(NOT arm_swp_deprecated)
message (FATAL_ERROR 
    "${Boost_INCLUDE_DIRS}/boost/smart_ptr/detail/sp_has_sync.hpp or "
    "${Boost_INCLUDE_DIRS}/boost/asio/detail/fenced_block.hpp do not appear to be patched.\n"
    "Please follow the included instructions for patching your Boost headers."
    "Then remove CMakeCache.txt from the build/patch_check directory.")
endif()
