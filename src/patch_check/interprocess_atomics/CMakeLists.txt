#This bug is fixed in 1.57
if (Boost_VERSION LESS 105700) #1.57
  set(atomic_name "boost/interprocess/detail/atomic.hpp")
  find_path(atomic_path "boost/interprocess/detail/atomic.hpp")
  set (full_path "${atomic_path}/${atomic_name}")

  message(STATUS "Checking ${full_path}")

  file(STRINGS ${full_path} FOUND_COMMENTS
    REGEX "//Patched for Safir SDK Core")

  list(LENGTH FOUND_COMMENTS NUM_COMMENTS)

  if(NOT NUM_COMMENTS EQUAL 5)
    message (FATAL_ERROR
      "${full_path} does not appear to be patched.\n"
      "Please follow the included instructions for patching your Boost headers.")
  endif()
endif()
