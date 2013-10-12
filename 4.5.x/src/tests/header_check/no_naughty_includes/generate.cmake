cmake_minimum_required(VERSION 2.6)

FILE(GLOB_RECURSE headers
  RELATIVE $ENV{SAFIR_SDK}/include
  $ENV{SAFIR_SDK}/include/*.h
  )

file(WRITE all_headers_included.cpp  "")

set(EXCLUDE_LIST
  AutoLink\\.h #ok, needs a define, but manually checked to not include windows.h
  ^ace/ #dont check third party library
  ^boost/ #dont check third party library
  /Breakpad/
  AceTimeConverter\\.h #these are kind of meant to 
  AceDispatcher\\.h    # include ace, which includes windows.h...
  AsioDispatcher\\.h   #includes asio.hpp, which appears to include windows.h
 
  #here are the headers that include windows.h and should not!
  /Odbc/
  /DoseComAux/
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
