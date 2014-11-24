
#Date_time is required on windows, even though we build header-only...
if (MSVC)
  set (BOOST_REQUIRED_ON_WINDOWS date_time)
endif()

set(Boost_NO_BOOST_CMAKE ON)
set(Boost_USE_MULTITHREADED ON)
set(Boost_FIND_QUIETLY True)

find_package(Boost
  COMPONENTS
  atomic
  regex
  timer
  program_options
  filesystem
  thread
  system
  unit_test_framework
  ${BOOST_REQUIRED_ON_WINDOWS}
  REQUIRED)

include_directories(${Boost_INCLUDE_DIRS})
#link_directories(${Boost_LIBRARY_DIRS})

if (Boost_VERSION LESS 105300)
  MESSAGE(FATAL_ERROR "Boost >= 1.53 required! Sorry!")
endif()

#Date_time is needed by boost thread on windows, so we monkey-patch that in, rather than adding link dependencies everywhere.
if (MSVC)
  set (Boost_THREAD_LIBRARY ${Boost_DATE_TIME_LIBRARY} ${Boost_THREAD_LIBRARY})
endif()

#make sure all linking is explicit as to what parts of boost it needs, so we clear the variable
#that "contains all".
SET(Boost_LIBRARIES "DONT_USE_Boost_LIBRARIES_VARIABLE")

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
