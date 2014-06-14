set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} 
  ${Boost_SYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY})

add_subdirectory(tmp_dir_helpers)
add_subdirectory(has_member_function_callable_with)
