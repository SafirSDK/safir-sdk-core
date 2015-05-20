include(CheckCXXSourceCompiles)

CHECK_CXX_SOURCE_COMPILES("#include \"${CMAKE_CURRENT_SOURCE_DIR}/test.cpp\"" has_member_function_callable_with_patched)

if(NOT has_member_function_callable_with_patched)
  unset(has_member_function_callable_with_patched CACHE)
  message (FATAL_ERROR 
    "${Boost_INCLUDE_DIRS}/boost/intrusive/detail/has_member_function_callable_with.hpp does not appear to be patched.\n"
    "Please use a supported Boost version!")
endif()

