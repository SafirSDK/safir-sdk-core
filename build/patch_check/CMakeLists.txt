cmake_minimum_required(VERSION 2.8)

project(patch_check CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS thread system REQUIRED)
set(CMAKE_REQUIRED_LIBRARIES ${Boost_LIBRARIES})

#on some boost versions we need to link chrono as well, so we
#check for chrono quietly (it doesnt exist in all versions).
set (Boost_FIND_QUIETLY 1)
FIND_PACKAGE(Boost COMPONENTS chrono)
set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${Boost_LIBRARIES})

add_subdirectory(boost_iostreams_flushing)
add_subdirectory(offset_ptr)
add_subdirectory(tmp_dir_helpers)
add_subdirectory(win32_api)
add_subdirectory(locks)
add_subdirectory(arm_swp_deprecated)
add_subdirectory(has_member_function_callable_with)

#dummy install to get an install target in the makefiles
INSTALL (FILES
  DESTINATION dummy)
