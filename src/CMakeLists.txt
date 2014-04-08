cmake_minimum_required(VERSION 2.8.11)

PROJECT(safir_sdk_core CXX C)


#TODO: !!
set(SAFIR_SDK "/NO_THIS_IS_NOT_RIGHT")
set(SAFIR_RUNTIME "/NO_THIS_IS_NOT_RIGHT")

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

#if we're using gcc we need to set up some things
if (UNIX)
   #turn on more warnings, set up use of threads, and set symbol visibility to hide as much as possible
   SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pthread -fvisibility=hidden -fvisibility-inlines-hidden -Bsymbolic -Wl,--exclude-libs=ALL")
   SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pthread -fvisibility=hidden -Bsymbolic  -Wl,--exclude-libs=ALL")
   SET (CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -DNDEBUG")

   #make sure we get the correct posix version
   ADD_DEFINITIONS(-D_POSIX_C_SOURCE=200809L)
endif ()

if (MSVC)
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
  "1.50" "1.50.0" "1.51" "1.51.0" "1.52" "1.52.0" "1.53" "1.53.0" "1.54" "1.54.0" 
  "1.55" "1.55.0" "1.56" "1.56.0" "1.57" "1.57.0" "1.58" "1.58.0" "1.59" "1.59.0") 

set(Boost_NO_BOOST_CMAKE ON)
set(Boost_USE_MULTITHREADED ON)
find_package(Boost)
if(Boost_FOUND)
   include_directories(${Boost_INCLUDE_DIRS})
   link_directories(${Boost_LIBRARY_DIRS})
endif()

#use dynamic linking with boost
ADD_DEFINITIONS(-DBOOST_ALL_DYN_LINK)

#dont use autolinking with boost
ADD_DEFINITIONS(-DBOOST_ALL_NO_LIB)

#disable deprecated functionality that we don't want
ADD_DEFINITIONS(-DBOOST_FILESYSTEM_NO_DEPRECATED)
ADD_DEFINITIONS(-DBOOST_SYSTEM_NO_DEPRECATED)

#we want to use boost::chrono instead of std::chrono and date_time for threads and asio
ADD_DEFINITIONS(-DBOOST_ASIO_DISABLE_STD_CHRONO)
ADD_DEFINITIONS(-DBOOST_THREAD_DONT_USE_DATETIME)

#Make Boost.Chrono header-only
ADD_DEFINITIONS(-DBOOST_CHRONO_HEADER_ONLY)

#Make sure we only use the header-only part of Boost.DateTime
ADD_DEFINITIONS(-DBOOST_DATE_TIME_NO_LIB)

#Set up boost for any test code (i.e. CheckCXXSourceCompiles stuff)
set(CMAKE_REQUIRED_INCLUDES ${Boost_INCLUDE_DIRS})
set(CMAKE_REQUIRED_DEFINITIONS 
  -DBOOST_ALL_DYN_LINK
  -DBOOST_FILESYSTEM_NO_DEPRECATED
  -DBOOST_SYSTEM_NO_DEPRECATED
  -DBOOST_ASIO_DISABLE_STD_CHRONO
  -DBOOST_THREAD_DONT_USE_DATETIME
  -DBOOST_CHRONO_HEADER_ONLY
  -DBOOST_DATE_TIME_NO_LIB)

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

if (MSVC)
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
    
    #TODO
    #Install the pdb files using the locations we just worked out.
    #INSTALL(FILES ${debug_location} DESTINATION ${SAFIR_RUNTIME}/bin CONFIGURATIONS Debug)
    #INSTALL(FILES ${relwithdebinfo_location} DESTINATION ${SAFIR_RUNTIME}/bin CONFIGURATIONS RelWithDebInfo)
    
    UNSET(debug_location)
    UNSET(relwithdebinfo_location)
  endif()
ENDMACRO()

if (UNIX)
  set(PATH_SEPARATOR ":")
else()
  set(PATH_SEPARATOR ";")
endif()


#also accepts an optional second argument TIMEOUT which will set test timeout
#in seconds
function (SET_SAFIR_TEST_PROPERTIES TEST_NAME) 
  #FIND_SAFIR_SDK_SOURCE_ROOT(SAFIR_SOURCE_ROOT)
  if (ARGV1)
    SET_TESTS_PROPERTIES(${TEST_NAME} PROPERTIES TIMEOUT ${ARGV1})
  endif()

  set (pypath "$ENV{PYTHONPATH}${PATH_SEPARATOR}${safir_sdk_core_SOURCE_DIR}/tests/test_support/python")
  string(REGEX REPLACE "^${PATH_SEPARATOR}+" "" pypath ${pypath}) # remove any leading path separators

  SET_PROPERTY(TEST ${TEST_NAME}
    PROPERTY ENVIRONMENT 
    "SAFIR_TEST_CONFIG_OVERRIDE=${safir_sdk_core_SOURCE_DIR}/tests/test_support/test_config"
    "SAFIR_SOURCE_ROOT=${safir_sdk_core_SOURCE_DIR}/../"
    "PYTHONPATH=${pypath}")
endfunction()

INCLUDE(cmake/build_generated.cmake)



include_directories(lluf/lluf_build_helpers.ss/src)

ADD_DEFINITIONS(-DSAFIR_NO_AUTOLINK)


ENABLE_TESTING()
FIND_PACKAGE(PythonInterp)



add_subdirectory(patch_check)
add_subdirectory(lluf)
add_subdirectory(logging)
add_subdirectory(dots)
add_subdirectory(dose)
add_subdirectory(douf)
add_subdirectory(swre)
add_subdirectory(foreach)
