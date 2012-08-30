include(CheckCXXSourceRuns)

CHECK_CXX_SOURCE_RUNS("#include \"${CMAKE_CURRENT_SOURCE_DIR}/test.cpp\"" boost_iostreams_flushing)

if(NOT boost_iostreams_flushing)
message (FATAL_ERROR 
    "Boost.Iostreams does not appear to be patched.\n"
    "Please follow the included instructions for patching your Boost headers.\n"
    "Then remove CMakeCache.txt from the build/patch_check directory.")
endif()
