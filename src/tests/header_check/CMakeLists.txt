if (NOT CMAKE_BUILD_TYPE)
   set(CMAKE_BUILD_TYPE "RelWithDebInfo")
 endif()

FILE(GLOB_RECURSE headers
  RELATIVE ${safir_sdk_core_SOURCE_DIR}/include
  ${safir_sdk_core_SOURCE_DIR}/include/*.h
  )

file(WRITE all_headers_included.cpp  "")

set(EXCLUDE_LIST
  #AutoLink\\.h #ok, needs a define, but manually checked to not include windows.h
  #^boost/ #dont check third party library
  #/Breakpad/
  #AceTimeConverter\\.h #these are kind of meant to 
  #AceDispatcher\\.h    # include ace, which includes windows.h...
  #AsioDispatcher\\.h   #includes asio.hpp, which appears to include windows.h

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
    file(APPEND all_headers_included.cpp "#include <${header}>\n")
    file(APPEND all_headers_included.cpp "#ifdef GetMessage\n#error windows.h appears to be included by ${header}\n#endif\n\n")
    file(APPEND all_headers_included.cpp "#ifdef _INC_WINDOWS\n#error windows.h appears to be included by ${header}\n#endif\n\n")
  endif()
endforeach()

file(APPEND all_headers_included.cpp "int main() {return 0;}\n")

add_executable(check_correct_public_headers all_headers_included.cpp)

target_link_libraries(check_correct_public_headers
  dots_generated-Safir-cpp)

#execute_process(COMMAND ${CMAKE_COMMAND} -G ${CMAKE_GENERATOR} -DCMAKE_BUILD_TYPE:string=${CMAKE_BUILD_TYPE} -DDIRECTORY_TO_CHECK:string=${safir_sdk_core_SOURCE_DIR} .
#  WORKING_DIRECTORY correct_include_files)

#add_test(NAME correct_include_files 
#  COMMAND ${CMAKE_COMMAND} --build correct_include_files)

