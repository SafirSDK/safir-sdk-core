#This bug is fixed in 1.57
if (Boost_VERSION LESS 105700) #1.57
  find_path(atomic_path "boost/interprocess/detail/atomic.hpp")
  message(STATUS "Checking ${atomic_path}/boost/interprocess/detail/atomic.hpp")
  file(STRINGS ${atomic_path} FOUND_COMMENTS
    REGEX "//Patched for Safir SDK Core")

  list(LENGTH FOUND_COMMENTS NUM_COMMENTS)

  if(NOT NUM_COMMENTS EQUAL 5)
    message (FATAL_ERROR
      "${atomic_path} does not appear to be patched.\n"
      "Please follow the included instructions for patching your Boost headers.")
  endif()
endif()
