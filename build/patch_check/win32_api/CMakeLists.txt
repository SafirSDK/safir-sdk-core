if (Boost_VERSION LESS 104100) #1.41
  set(win32_api_path ${Boost_INCLUDE_DIRS}/boost/interprocess/detail/win32_api.hpp)
  
  file(STRINGS ${win32_api_path} FOUND_SHARING_VIOLATION
    REGEX "GetLastError.*32.*sharing_violation")
  
  if("${FOUND_SHARING_VIOLATION}" STREQUAL "")
    message (FATAL_ERROR 
      "${win32_api_path} does not appear to be patched.\n"
      "Please follow the included instructions for patching your Boost headers."
      "Then remove CMakeCache.txt from the build/patch_check directory.")
  endif()
endif()
