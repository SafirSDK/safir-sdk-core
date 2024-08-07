cmake_minimum_required(VERSION 3.24.0)

if (DEFINED ENV{JENKINS_HOME} OR SAFIR_EXAMPLES_USE_CONAN)
  set (CMAKE_PROJECT_TOP_LEVEL_INCLUDES ${CMAKE_SOURCE_DIR}/../src/cmake/conan_provider.cmake)
endif()

PROJECT(safir_sdk_core_examples C CXX) #Conan stuff requires C, and it does not break anything else.

#You can use later standards as well, but this is how we have to set it up for vs2015 compatibility.
if (MSVC_VERSION EQUAL 1900)
  set(CMAKE_CXX_STANDARD 14)
else()
  set(CMAKE_CXX_STANDARD 17)
endif()

#
# !! If you are trying to compile the examples, please read this !!
#
# Background: The example apps have an optional dependency on Boost.
#
# If you're trying to compile the examples and have Boost installed the examples
# should find boost and use it for VehicleAppCpp.
#
# If you don't have Boost installed VehicleAppCpp will use an alternative method
# of achieving the same thing.
#
# When we build these examples through Jenkins - as a check that the SDK has been
# properly built and can actually be used to compile applications on top of - we need to
# get hold of a reasonable copy of Boost. In fact we want an old Boost, so that we can
# verify that the user can use whatever version of Boost they like. To get hold of Boost
# we use Conan. For more information on conan, visit https://conan.io.
#

if (DEFINED ENV{JENKINS_HOME} OR SAFIR_EXAMPLES_USE_CONAN)

  write_file(${CMAKE_SOURCE_DIR}/conanfile.txt
    "[requires]\n"
    "boost/1.79.0\n"
    "[options]\n"
    "boost*:header_only=True\n"
    "[generators]\n"
    "CMakeDeps\n")

  SET(REQUIRED_BOOST_VERSION 1.79.0 REQUIRED)

endif()

ADD_DEFINITIONS(-DBOOST_ALL_DYN_LINK)

find_package(Boost ${REQUIRED_BOOST_VERSION})

find_package(SafirSDKCore REQUIRED)

add_subdirectory(dou_files)

#the code below needs to be able to include the Safir SDK Core headers
include_directories(${SAFIR_SDK_CORE_INCLUDE_DIRS})

if (MSVC)
  ADD_DEFINITIONS(-D_WINSOCK_DEPRECATED_NO_WARNINGS)
  ADD_DEFINITIONS(-D_WIN32_WINNT=0x0600)
  ADD_DEFINITIONS(-DWINVER=0x0600)
  ADD_DEFINITIONS(-DWIN32_LEAN_AND_MEAN)
endif()

if (UNIX)
   #set up use of threads and turn on more warnings
   SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread -Wall -Wextra -Wpedantic -Wsuggest-override -Werror=suggest-override")
   SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--warn-common,--fatal-warnings")
endif ()


#load macros for building dotnet stuff with cmake
include(${SAFIR_SDK_CORE_DOTNET_SETTINGS})
include(${SAFIR_SDK_CORE_JAVA_SETTINGS})

set (CSHARP_LIBRARY_PATHS "${SAFIR_SDK_CORE_CSHARP_LIBRARY_PATH}")

add_subdirectory(vehicleapp/src)
add_subdirectory(vehiclemmi/src)
