include(CheckCXXSourceRuns)

CHECK_CXX_SOURCE_RUNS("#include \"${CMAKE_CURRENT_SOURCE_DIR}/test.cpp\"" container_destroyers)

if(NOT container_destroyers)
  unset(container_destroyers CACHE)
  message (FATAL_ERROR
    "Boost.Containers does not appear to be patched.\n"
    "Please follow the included instructions for patching your Boost headers.")
endif()
