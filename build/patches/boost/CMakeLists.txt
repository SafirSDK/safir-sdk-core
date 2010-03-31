project(ubuntu_patches)

cmake_minimum_required(VERSION 2.4)

#Add some more boost library versions that we want to be able to use,
# just to try to be "future safe"
set (Boost_ADDITIONAL_VERSIONS "1.37" "1.37.0" "1.38" "1.38.0" "1.39" "1.39.0" "1.40" "1.40.0" "1.41" "1.41.0" "1.42" "1.42.0" "1.43" "1.43.0" "1.44" "1.44.0" "1.45" "1.45.0" "1.46" "1.46.0" "1.47" "1.47.0" "1.48" "1.48.0" "1.49" "1.49.0")

find_package(Boost)

if (NOT Boost_FOUND)
   message (FATAL_ERROR "Boost was not found, cannot install boost patches!")
endif()

add_subdirectory("${Boost_LIB_VERSION}")
