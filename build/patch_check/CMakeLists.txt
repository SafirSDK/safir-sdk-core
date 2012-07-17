cmake_minimum_required(VERSION 2.6)

project(patch_check)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

#add_subdirectory(boost_iostreams_flushing)
#add_subdirectory(offset_ptr)
#add_subdirectory(tmp_dir_helpers)
#add_subdirectory(win32_api)
#add_subdirectory(locks)
add_subdirectory(arm_swp_deprecated)

#dummy install to get an install target in the makefiles
INSTALL (FILES
  DESTINATION dummy)
