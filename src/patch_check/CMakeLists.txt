set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} 
  Boost::thread)

add_subdirectory(container_destroyers)
add_subdirectory(tmp_dir_helpers)
add_subdirectory(interprocess_atomics)
