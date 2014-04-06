FIND_PACKAGE(Boost COMPONENTS thread system REQUIRED)
set(CMAKE_REQUIRED_LIBRARIES ${Boost_LIBRARIES})

set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${Boost_LIBRARIES})

add_subdirectory(boost_iostreams_flushing)
add_subdirectory(offset_ptr)
add_subdirectory(tmp_dir_helpers)
add_subdirectory(win32_api)
add_subdirectory(locks)
add_subdirectory(arm_swp_deprecated)
add_subdirectory(has_member_function_callable_with)
