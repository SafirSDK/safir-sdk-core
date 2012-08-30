include(CheckCXXSourceRuns)

FIND_PACKAGE(Boost COMPONENTS date_time thread system)

#if (MSVC)
  set(CMAKE_REQUIRED_LIBRARIES ${Boost_LIBRARIES})
#else()
#  set(ENV{LIB} "$ENV{LIB};${Boost_LIBRARY_DIRS}")
#endif()


CHECK_CXX_SOURCE_RUNS("#include \"${CMAKE_CURRENT_SOURCE_DIR}/test.cpp\"" boost_iostreams_flushing)

if(NOT boost_iostreams_flushing)
message (FATAL_ERROR 
    "Boost.Iostreams does not appear to be patched.\n"
    "Please follow the included instructions for patching your Boost headers.\n"
    "Then remove CMakeCache.txt from the build/patch_check directory.")
endif()
