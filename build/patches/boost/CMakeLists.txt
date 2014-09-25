set(Boost_NO_BOOST_CMAKE ON)

find_package(Boost)

if (NOT Boost_FOUND)
   message (FATAL_ERROR "Boost was not found, cannot install boost patches!")
endif()

add_subdirectory("container_destroyers")
add_subdirectory("tmp_dir_helpers")
add_subdirectory("has_member_function_callable_with")
add_subdirectory("interprocess_atomics")
