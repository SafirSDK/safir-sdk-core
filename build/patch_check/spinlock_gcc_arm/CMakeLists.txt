include(CheckCXXSourceCompiles)

if (CMAKE_COMPILER_IS_GNUCXX)
  SET(CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS} -Werror)
endif()

CHECK_CXX_SOURCE_COMPILES("#include \"${CMAKE_CURRENT_SOURCE_DIR}/test.cpp\"" spinlock_gcc_arm_patched)

if(NOT spinlock_gcc_arm_patched)
message (FATAL_ERROR 
    "${Boost_INCLUDE_DIRS}/boost/smart_ptr/detail/spinlock_gcc_arm.hpp does not appear to be patched.\n"
    "Please follow the included instructions for patching your Boost headers."
    "Then remove CMakeCache.txt from the build/patch_check directory.")
endif()
