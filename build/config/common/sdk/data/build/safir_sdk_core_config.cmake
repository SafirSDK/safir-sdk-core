# This file contains common settings for all Safir SDK Core components.
# Do NOT use this file unless you are writing a component that is part
# of the _Safir SDK Core_. We reserve the right to change this file 
# without maintaining any sort of backward compatibility whatsoever.
#
# But please feel free to make a copy of your own and modify to your
# needs (according to below license). 
#
# Copyright (c) 2006-2013, Saab AB.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of the <organization> nor the
#       names of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#Set SAFIR_RUNTIME and SAFIR_SDK variables to paths that cmake likes.
FILE(TO_CMAKE_PATH "$ENV{SAFIR_SDK}" SAFIR_SDK)
FILE(TO_CMAKE_PATH "$ENV{SAFIR_RUNTIME}" SAFIR_RUNTIME)

#Default build release
if (NOT CMAKE_BUILD_TYPE)
   set(CMAKE_BUILD_TYPE "Release")
endif()

#add include path
INCLUDE_DIRECTORIES(${SAFIR_SDK}/include)

#if we're using gcc we need to set up some things
if (UNIX)
   #link directory for libraries (will this work with gcc under windows?)
   LINK_DIRECTORIES(${SAFIR_RUNTIME}/lib)

   #turn on more warnings and set up use of threads etc
   SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pthread")
   SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pthread")
   SET (CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -DNDEBUG")

   #make sure we get the correct posix version
   ADD_DEFINITIONS(-D_POSIX_C_SOURCE=200809L)
endif ()

if (MSVC)
   LINK_DIRECTORIES(${SAFIR_SDK}/lib)
   ADD_DEFINITIONS(-DNOMINMAX)
   ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE -D_SCL_SECURE_NO_DEPRECATE -D_CRT_NONSTDC_NO_DEPRECATE)
   ADD_DEFINITIONS(-D_UNICODE -DUNICODE)
   ADD_DEFINITIONS(-D_WIN32_WINNT=0x0501)
   ADD_DEFINITIONS(-DWIN32_LEAN_AND_MEAN)
   ADD_DEFINITIONS(/wd4503) #decorated name length exceeded
   ADD_DEFINITIONS(/wd4512) #assignment operator could not be generated

   #increase warning level
   # Use the highest warning level for visual studio.
   SET(CMAKE_CXX_WARNING_LEVEL 4)
   IF(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
     STRING(REGEX REPLACE "/W[0-4]" "/W4"
       CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
   ELSE()
     SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
   ENDIF()

   #Set linker flag /OPT:REF (eliminates functions and/or data that are never referenced) reduces size of executable to approx the same size as in Release mode. Also disable incremental linking to avoid warning.
   set(CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO  "${CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO} /OPT:REF /INCREMENTAL:NO")
   set(CMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO  "${CMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO} /OPT:REF /INCREMENTAL:NO")
endif ()

# Add some more boost library versions that we want to be able to use,
# just to try to be "future safe". This does not actually mean that we
# support all these versions, see our release information for that info.
set (Boost_ADDITIONAL_VERSIONS 
  "1.40" "1.40.0" "1.41" "1.41.0" "1.42" "1.42.0" "1.43" "1.43.0" "1.44" "1.44.0" 
  "1.45" "1.45.0" "1.46" "1.46.0" "1.47" "1.47.0" "1.48" "1.48.0" "1.49" "1.49.0" 
  "1.50" "1.50.0" "1.51" "1.51.0" "1.52" "1.52.0" "1.53" "1.53.0" "1.54" "1.54.0" 
  "1.55" "1.55.0" "1.56" "1.56.0" "1.57" "1.57.0" "1.58" "1.58.0" "1.59" "1.59.0") 

set(Boost_NO_BOOST_CMAKE ON)
set(Boost_USE_MULTITHREADED ON)
set(Boost_FIND_QUIETLY 1)
# Use boost from tower if it exists, unless specifically set to something else
if (NOT BOOST_ROOT AND "$ENV{BOOST_ROOT}" STREQUAL "")
  set(BOOST_ROOT ${SAFIR_SDK})
endif()
find_package(Boost)
if(Boost_FOUND)
   include_directories(${Boost_INCLUDE_DIRS})
   link_directories(${Boost_LIBRARY_DIRS})
else()
   unset(BOOST_ROOT)
   find_package(Boost)
   if(Boost_FOUND)
      include_directories(${Boost_INCLUDE_DIRS})
      link_directories(${Boost_LIBRARY_DIRS})
    else()
        message(FATAL_ERROR "Failed to find a boost installation!")
    endif()
endif()
set (Boost_FIND_QUIETLY 0)
#use dynamic linking with boost
ADD_DEFINITIONS(-DBOOST_ALL_DYN_LINK)

#Note that we do not request a specific boost.filesystem version,
#so we may get either v2 or v3. This is due to the fact that
#we're still supporting versions where v3 does not exist.
#When we drop support for those old versions, uncomment the line below,
#and add the same define to the CMAKE_REQUIRED_DEFINITIONS below.
#ADD_DEFINITIONS(-DBOOST_FILESYSTEM_VERSION=3)

#disable all deprecated functionality in boost.filesystem.
ADD_DEFINITIONS(-DBOOST_FILESYSTEM_NO_DEPRECATED)

#Make sure we only use the header-only part of Boost.DateTime
ADD_DEFINITIONS(-DBOOST_DATE_TIME_NO_LIB)

#Set up boost for any test code (i.e. CheckCXXSourceCompiles stuff)
set(CMAKE_REQUIRED_INCLUDES ${Boost_INCLUDE_DIRS})
set(CMAKE_REQUIRED_DEFINITIONS -DBOOST_ALL_DYN_LINK -DBOOST_FILESYSTEM_NO_DEPRECATED -DBOOST_DATE_TIME_NO_LIB)

if(MSVC)
   #We have a weird issue which causes a buffer overrun error when using Visual Studio 2013
   #and Boost 1.55 in 64 bit and release builds. 
   #Don't know if this is a bug in our code or in the compiler or in boost.
   #The workaround below disables some optimizations and all inlining in release builds
   #which appears to resolve the problem.
   if(MSVC_VERSION EQUAL 1800 AND Boost_VERSION EQUAL 105500 AND CMAKE_SIZEOF_VOID_P EQUAL 8)
     STRING(REGEX REPLACE "/Ob1" "/Ob0" CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")
     STRING(REGEX REPLACE "/O2" "/O1" CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")
     STRING(REGEX REPLACE "/Ob1" "/Ob0" CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE}")
     STRING(REGEX REPLACE "/O2" "/O1" CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE}")
   endif()
endif()


#Let ctest output stdout on failure by default.
set(CTEST_OUTPUT_ON_FAILURE ON)

if (MSVC AND NOT NO_LIBRARY_POSTFIXES)
  SET(CMAKE_DEBUG_POSTFIX "d")
endif()

MACRO(INSTALL_DEBUG_INFO target)
  if(MSVC)
    #the problem here is to find out where the pdb file is located. It is located next to the binary
    #in some directory which either the nmake/jom builds create or that the studio creates.

    GET_TARGET_PROPERTY(debug_location ${target} LOCATION_Debug)
    GET_TARGET_PROPERTY(relwithdebinfo_location ${target} LOCATION_RelWithDebInfo)

    #replace binary's the extension with .pdb
    #.exe --> .pdb
    #.dll --> .pdb
    STRING(REPLACE .dll .pdb debug_location ${debug_location})
    STRING(REPLACE .exe .pdb debug_location ${debug_location})

    STRING(REPLACE .dll .pdb relwithdebinfo_location ${relwithdebinfo_location})
    STRING(REPLACE .exe .pdb relwithdebinfo_location ${relwithdebinfo_location})
    
    #Install the pdb files using the locations we just worked out.
    INSTALL(FILES ${debug_location} DESTINATION ${SAFIR_RUNTIME}/bin CONFIGURATIONS Debug)
    INSTALL(FILES ${relwithdebinfo_location} DESTINATION ${SAFIR_RUNTIME}/bin CONFIGURATIONS RelWithDebInfo)
    
    UNSET(debug_location)
    UNSET(relwithdebinfo_location)
  endif()
ENDMACRO()

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${SAFIR_SDK}/data/build/)

#MSVC variable is not set when using None as project languages
#as is done in the dotnet projects. So we check on WIN32 instead.
if (WIN32)
    SET(COMMON_CS_FLAGS "-warn:4" "-nologo" "-nowarn:1607")

    #Get platform and convert it to lowercase (vs2010 has it as X64 and vs2013 express as x64!)
    string(TOLOWER "$ENV{Platform}" PLATFORM)

    #make sure we set the arch of dotnet assemblies to be the same as the native code we build.
    if (PLATFORM STREQUAL "x64")
      SET(COMMON_CS_FLAGS ${COMMON_CS_FLAGS} "-platform:x64")
    else()
      SET(COMMON_CS_FLAGS ${COMMON_CS_FLAGS} "-platform:x86")
    endif()
else()
    SET(COMMON_CS_FLAGS "-warn:4" "-nowarn:1587")
endif()


#work out if we've got a configuration on the command line or if 
#we're running in an IDE.
if (CMAKE_CONFIGURATION_TYPES AND CMAKE_BUILD_TYPE)
    #MESSAGE("Both CMAKE_CONFIGURATION_TYPES and CMAKE_BUILD_TYPE are set! Using CMAKE_BUILD_TYPE as the CUSTOM_BUILD_TYPE")
    SET(CUSTOM_BUILD_TYPE ${CMAKE_BUILD_TYPE})
elseif(CMAKE_CONFIGURATION_TYPES)
    # fix for vs2010
    if (MSVC_VERSION EQUAL 1600)
       SET(CUSTOM_BUILD_TYPE "$(Configuration)")
    else()
       SET(CUSTOM_BUILD_TYPE "$(OutDir)")
    endif()
    
elseif(CMAKE_BUILD_TYPE)
    SET(CUSTOM_BUILD_TYPE ${CMAKE_BUILD_TYPE})
else()
    SET(CUSTOM_BUILD_TYPE "Release")
endif()

#just use these variables to avoid some cmake warnings 
#(they're set in the build script)
if(SAFIR_ADA_SUPPORT OR SAFIR_JAVA_SUPPORT)
endif()

if (UNIX)
  set(PATH_SEPARATOR ":")
else()
  set(PATH_SEPARATOR ";")
endif()

#This function traverses up from PROJECT_SOURCE_DIR to find the root of the 
#source code tree. It looks for some special files in that directory.
function (FIND_SAFIR_SDK_SOURCE_ROOT RESULT_VARIABLE)
  set (curdir ${PROJECT_SOURCE_DIR})
  while (NOT EXISTS "${curdir}/INSTALL.Linux.txt" 
      OR NOT EXISTS "${curdir}/INSTALL.Windows.txt")
    get_filename_component(parent ${curdir} PATH)
    if (parent STREQUAL curdir)
      set(${RESULT_VARIABLE} "SOURCE_ROOT-NOTFOUND")
      message (FATAL_ERROR "!!! Failed to find Safir SDK Core source code root.")
      return()
    endif()
    set(curdir ${parent})
  endwhile()
  set(${RESULT_VARIABLE} ${curdir} PARENT_SCOPE)
endfunction()

#also accepts an optional second argument TIMEOUT which will set test timeout
#in seconds
function (SET_SAFIR_TEST_PROPERTIES TEST_NAME) 
  FIND_SAFIR_SDK_SOURCE_ROOT(SAFIR_SOURCE_ROOT)
  if (ARGV1)
    SET_TESTS_PROPERTIES(${TEST_NAME} PROPERTIES TIMEOUT ${ARGV1})
  endif()

  set (pypath "$ENV{PYTHONPATH}${PATH_SEPARATOR}${SAFIR_SOURCE_ROOT}/src/tests/test_support/python")
  string(REGEX REPLACE "^${PATH_SEPARATOR}+" "" pypath ${pypath}) # remove any leading path separators

  SET_PROPERTY(TEST ${TEST_NAME}
    PROPERTY ENVIRONMENT 
    "SAFIR_TEST_CONFIG_OVERRIDE=${SAFIR_SOURCE_ROOT}/src/tests/test_support/test_config"
    "PYTHONPATH=${pypath}")
endfunction()
