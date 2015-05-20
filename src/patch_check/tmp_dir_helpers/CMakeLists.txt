#it seems very difficult to test the change made in 1_56 in a proper way.
#we would need to actually check the type of a private member :(
#so we just check boost version...
if (Boost_VERSION LESS 105600) #1.56
  include(CheckCXXSourceCompiles)

  CHECK_CXX_SOURCE_COMPILES("#include \"${CMAKE_CURRENT_SOURCE_DIR}/test.cpp\"" tmp_dir_helpers_patched)

  if(NOT tmp_dir_helpers_patched)
    unset(tmp_dir_helpers_patched CACHE)
    message (FATAL_ERROR
      "${Boost_INCLUDE_DIRS}/boost/interprocess/detail/tmp_dir_helpers.hpp does not appear to be patched.\n"
      "Please use a supported Boost version!")
  endif()
endif()
