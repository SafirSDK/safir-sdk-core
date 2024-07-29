#This file sets up some of the compiler flags we want for GCC and MSVC.
cmake_minimum_required(VERSION 3.13)

if (MSVC_VERSION EQUAL 1900)
  set(CMAKE_CXX_STANDARD 14)
else()
  set(CMAKE_CXX_STANDARD 17)
endif()

if (UNIX)

  add_compile_options($<$<CXX_COMPILER_ID:GNU>:-fstack-protector-strong>)

  #turn on more warnings, set up use of threads, and set symbol visibility to hide as much as possible
  add_compile_options(-Wall -Wextra -Wpedantic -Winvalid-pch -pthread -fvisibility=hidden)
  add_compile_options($<$<COMPILE_LANGUAGE:CXX>:-Wsuggest-override>)
  add_compile_options($<$<COMPILE_LANGUAGE:CXX>:-fvisibility-inlines-hidden>)
  add_compile_options($<$<COMPILE_LANGUAGE:C>:-Bsymbolic>)

  add_link_options(-Wl,--exclude-libs=ALL -Wl,--warn-common -Wl,--fatal-warnings -pthread -latomic)

  #make sure we get the correct posix version
  add_compile_definitions(_POSIX_C_SOURCE=200809L)

  #this is defined by -pthread (above) in most cases, but at least when creating precompiled headers
  #it is not, so we define it here as well.
  add_compile_definitions(_REENTRANT)
endif ()

if (MSVC)
   add_compile_definitions(NOMINMAX)
   add_compile_definitions(_CRT_SECURE_NO_DEPRECATE _SCL_SECURE_NO_DEPRECATE _CRT_NONSTDC_NO_DEPRECATE)
   add_compile_definitions(_SILENCE_ALL_CXX17_DEPRECATION_WARNINGS)
   add_compile_definitions(_WINSOCK_DEPRECATED_NO_WARNINGS)
   add_compile_definitions(_UNICODE UNICODE)
   add_compile_definitions(_WIN32_WINNT=0x0600)
   add_compile_definitions(WINVER=0x0600)
   add_compile_definitions(WIN32_LEAN_AND_MEAN)

   add_compile_options(/wd4503) #decorated name length exceeded
   add_compile_options(/wd4512) #assignment operator could not be generated

   #Disable some warnings from Visual Studio 2015, since we're close to deprecating support for that compiler.
   if (MSVC_VERSION EQUAL 1900)
     add_compile_options(/wd4714) #marked as __forceinline not inlined
     add_compile_options(/wd4913) #user defined binary operator ',' exists but no overload could convert all operands, default built-in binary operator ',' used
     add_compile_options(/wd4800) #forcing value to bool 'true' or 'false' (performance warning)
   endif()

   # increase warning level
   # Use the highest warning level for visual studio.
   # This ought to work both for cases where CMP0092 is set or not.
   IF(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
     STRING(REGEX REPLACE "/W[0-4]" "/W4"
       CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
   ELSE()
     add_compile_options(/W4)
   ENDIF()

   #generated libraries sometimes get very large, so we need to use bigobj compiler flag
   add_compile_options(/bigobj)

   #enable auto-inlining for RelWithDebInfo builds
   IF(CMAKE_CXX_FLAGS_RELWITHDEBINFO MATCHES "/Ob[0-4]")
     STRING(REGEX REPLACE "/Ob[0-4]" "/Ob2"
       CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")
   ELSE()
     SET(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} /Ob2")
   ENDIF()

   #Set linker flag /OPT:REF (eliminates functions and/or data that are never referenced)
   #reduces size of executable to approx the same size as in Release mode.
   #Also disable incremental linking to avoid warning.
   add_link_options($<$<CONFIG:RelWithDebInfo>:/OPT:REF>)
   add_link_options($<$<CONFIG:RelWithDebInfo>:/INCREMENTAL:NO>)

   #disable spurious linker warnings for third party libraries.
   add_link_options($<$<CONFIG:Debug>:/ignore:4099>)
endif ()
