include(CheckCXXSourceRuns)

#we make the assembler generate errors instead of warnings
#for gcc. Other compilers are not supported by this check, currently.
if (NOT CMAKE_COMPILER_IS_GNUCXX)
  return()
endif()

if (CMAKE_COMPILER_IS_GNUCXX)
  SET(CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS} -Wa,--fatal-warnings -pthread)
endif()

FIND_PACKAGE(Boost COMPONENTS system)

if (NOT MSVC)
  set(CMAKE_REQUIRED_LIBRARIES ${Boost_LIBRARIES})
endif()

find_path(HAVE_FENCED_BLOCK_HPP boost/asio/detail/fenced_block.hpp)

if (HAVE_FENCED_BLOCK_HPP)
  file(WRITE config.h "#define CMAKE_HAVE_FENCED_BLOCK_HPP\n")
else()
  file(WRITE config.h "\n")
endif()

CHECK_CXX_SOURCE_RUNS("#include \"${CMAKE_CURRENT_SOURCE_DIR}/test.cpp\"" arm_swp_deprecated)

if(NOT arm_swp_deprecated)
message (FATAL_ERROR 
    "${Boost_INCLUDE_DIRS}/boost/smart_ptr/detail/sp_has_sync.hpp or "
    "${Boost_INCLUDE_DIRS}/boost/asio/detail/fenced_block.hpp do not appear to be patched.\n"
    "Please follow the included instructions for patching your Boost headers.\n"
    "Then remove CMakeCache.txt from the build/patch_check directory.")
endif()
