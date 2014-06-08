FILE(GLOB_RECURSE headers
  RELATIVE ${safir_sdk_core_SOURCE_DIR}/include
  ${safir_sdk_core_SOURCE_DIR}/include/*.h
  )

set(EXCLUDE_LIST
  AutoLink\\.h       #Special file, but manually checked to not include windows.h
  AceTimeConverter\\.h #these are kind of meant to 
  AceDispatcher\\.h    # include ace, which includes windows.h...
  AsioDispatcher\\.h   #includes asio.hpp, which appears to include windows.h

  /Typesystem/ToolSupport #property_tree appears to have some windows.h stuff in it

  #here are the headers that include windows.h and should not!
  #/Odbc/
  #/DoseComAux/
  )

foreach(header ${headers})
  set(SKIP FALSE)
  foreach(regex ${EXCLUDE_LIST})
    if(header MATCHES ${regex})
      set(SKIP TRUE)
    endif()
  endforeach()
  
  if(NOT SKIP)
    string(REGEX REPLACE "[^a-zA-Z]" "_" name_base ${header})
    set(generated_name ${CMAKE_CURRENT_BINARY_DIR}/check_${name_base}_generated.cpp)
    set(sources ${sources} ${generated_name})
    file(WRITE ${generated_name} "")
    file(APPEND ${generated_name} "#ifdef SAFIR_NO_AUTOLINK\n#  undef SAFIR_NO_AUTOLINK\n#endif\n")
    file(APPEND ${generated_name} "#include <${header}>\n")
    file(APPEND ${generated_name} "#ifdef GetMessage\n#  error windows.h appears to be included by ${header}\n#endif\n\n")
    file(APPEND ${generated_name} "#ifdef _INC_WINDOWS\n#  error windows.h appears to be included by ${header}\n#endif\n\n")
  endif()
endforeach()

add_executable(check_correct_public_headers main.cpp ${sources})

target_link_libraries(check_correct_public_headers
  dots_generated-olib-cpp
  dots_generated-Safir-cpp)
