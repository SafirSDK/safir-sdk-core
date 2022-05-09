set(Boost_USE_MULTITHREADED ON)
set(Boost_USE_STATIC_LIBS   ON)

find_package(Boost
  COMPONENTS
  regex
  timer
  program_options
  filesystem
  thread
  system
  random
  unit_test_framework
  REQUIRED)

include_directories(${Boost_INCLUDE_DIRS})

if (Boost_VERSION VERSION_LESS 1.70.0)
  MESSAGE(FATAL_ERROR "Boost >= 1.70 required! Found ${Boost_VERSION}")
endif()

#Boost Asio strands can share the same implementation instance which means that hanging or long running operations can potentially block
#other strands. This symbol will make the first 193 strands unique and therefor in practice avoid any shared instances. 
ADD_DEFINITIONS(-DBOOST_ASIO_ENABLE_SEQUENTIAL_STRAND_ALLOCATION)
#TODO laha ADD_DEFINITIONS(-DBOOST_ASIO_NO_DEPRECATED)
ADD_DEFINITIONS(-DBOOST_ALLOW_DEPRECATED_HEADERS) #TODO laha

#disable deprecated functionality that we don't want
ADD_DEFINITIONS(-DBOOST_FILESYSTEM_NO_DEPRECATED)
ADD_DEFINITIONS(-DBOOST_SYSTEM_NO_DEPRECATED)

#we want to use boost::chrono instead of std::chrono and date_time for threads and asio
ADD_DEFINITIONS(-DBOOST_ASIO_DISABLE_STD_CHRONO)
ADD_DEFINITIONS(-DBOOST_THREAD_DONT_USE_DATETIME)

#Make Boost.Chrono header-only
ADD_DEFINITIONS(-DBOOST_CHRONO_HEADER_ONLY)

#use Boost.Chrono v2
ADD_DEFINITIONS(-DBOOST_CHRONO_VERSION=2)

#Make sure we only use the header-only part of Boost.DateTime
ADD_DEFINITIONS(-DBOOST_DATE_TIME_NO_LIB)

#The xml parsing uses Boost.Spirit, which we may be using from
#multiple threads at the same time
ADD_DEFINITIONS(-DBOOST_SPIRIT_THREADSAFE)

#Set up boost for any test code (i.e. CheckCXXSourceCompiles stuff)
set(CMAKE_REQUIRED_INCLUDES ${Boost_INCLUDE_DIRS})
set(CMAKE_REQUIRED_DEFINITIONS
  -DBOOST_FILESYSTEM_NO_DEPRECATED
  -DBOOST_SYSTEM_NO_DEPRECATED
  -DBOOST_ASIO_DISABLE_STD_CHRONO
  -DBOOST_THREAD_DONT_USE_DATETIME
  -DBOOST_CHRONO_HEADER_ONLY
  -DBOOST_DATE_TIME_NO_LIB)

