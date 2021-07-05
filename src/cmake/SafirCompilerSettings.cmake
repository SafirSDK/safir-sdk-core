#This file sets up some of the compiler flags we want for GCC and MSVC.

if (UNIX)

   if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
     if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.9")
       set(stack_protector_option "-fstack-protector")
     else()
       set(stack_protector_option "-fstack-protector-strong")
     endif()
   endif()

   #turn on more warnings, set up use of threads, and set symbol visibility to hide as much as possible
   SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wpedantic -Winvalid-pch -pthread -fvisibility=hidden -fvisibility-inlines-hidden ${stack_protector_option} -std=c++20")
   SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -Wpedantic -Winvalid-pch -pthread -fvisibility=hidden -Bsymbolic ${stack_protector_option}")

   SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--exclude-libs=ALL -Wl,--warn-common -Wl,--fatal-warnings")
   SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--exclude-libs=ALL -Wl,--warn-common -Wl,--fatal-warnings")

   SET (CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -DNDEBUG")

   #make sure we get the correct posix version
   ADD_DEFINITIONS(-D_POSIX_C_SOURCE=200809L)

   #this is defined by -pthread (above) in most cases, but at least when creating precompiled headers
   #it is not, so we define it here as well.
   ADD_DEFINITIONS(-D_REENTRANT)
endif ()

if (MSVC)
   ADD_DEFINITIONS(-DNOMINMAX)
   ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE -D_SCL_SECURE_NO_DEPRECATE -D_CRT_NONSTDC_NO_DEPRECATE)
   ADD_DEFINITIONS(-D_WINSOCK_DEPRECATED_NO_WARNINGS)
   ADD_DEFINITIONS(-D_UNICODE -DUNICODE)
   ADD_DEFINITIONS(-D_WIN32_WINNT=0x0501)
   ADD_DEFINITIONS(-DWIN32_LEAN_AND_MEAN)
   ADD_DEFINITIONS(/wd4503) #decorated name length exceeded
   ADD_DEFINITIONS(/wd4512) #assignment operator could not be generated

   if (MSVC_VERSION EQUAL 1600)
     ADD_DEFINITIONS(/wd4481) #nonstandard extension used, removed due to vs2010 not having full c++11 support
   endif()

   # increase warning level
   # Use the highest warning level for visual studio.
   IF(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
     STRING(REGEX REPLACE "/W[0-4]" "/W4"
       CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
   ELSE()
     SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
   ENDIF()

   #generated libraries sometimes get very large, so we need to use bigobj compiler flag
   set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /bigobj")

   #enable auto-inlining for RelWithDebInfo builds
   set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} /Ob2")

   #Set linker flag /OPT:REF (eliminates functions and/or data that are never referenced)
   #reduces size of executable to approx the same size as in Release mode.
   #Also disable incremental linking to avoid warning.
   set(CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO  "${CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO} /OPT:REF /INCREMENTAL:NO")
   set(CMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO  "${CMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO} /OPT:REF /INCREMENTAL:NO")
endif ()
