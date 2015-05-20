#This bug is fixed in 1.57
if (Boost_VERSION LESS 105700) #1.57
  set(atomic_name "boost/interprocess/detail/atomic.hpp")
  find_file(atomic_path "boost/interprocess/detail/atomic.hpp" HINTS ${CMAKE_REQUIRED_INCLUDES})

  message(STATUS "Checking ${atomic_path}")

  file(STRINGS ${atomic_path} FOUND_COMMENTS
    REGEX "//Patched for Safir SDK Core")

  list(LENGTH FOUND_COMMENTS NUM_COMMENTS)

  if(NOT NUM_COMMENTS EQUAL 5)
    message (FATAL_ERROR
      "${atomic_path} does not appear to be patched.\n"
      "Please use a supported Boost version!")
  endif()
endif()
