cmake_minimum_required(VERSION 3.16.0)

PROJECT(safir_sdk_core_examples CXX)

set(CMAKE_CXX_STANDARD 17)

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
  if(NOT EXISTS "${CMAKE_BINARY_DIR}/conan.cmake")
    message(STATUS "Downloading conan.cmake from https://github.com/conan-io/cmake-conan")
    file(DOWNLOAD "https://raw.githubusercontent.com/conan-io/cmake-conan/0.17.0/conan.cmake"
                  "${CMAKE_BINARY_DIR}/conan.cmake"
                  EXPECTED_HASH SHA256=3bef79da16c2e031dc429e1dac87a08b9226418b300ce004cc125a82687baeef
                  TLS_VERIFY ON)
  endif()

  include(${CMAKE_BINARY_DIR}/conan.cmake)

  conan_cmake_configure(REQUIRES boost/1.70.0
                        OPTIONS boost:header_only=True
                        GENERATORS cmake_find_package)

  conan_cmake_autodetect(settings)

  conan_cmake_install(PATH_OR_REFERENCE .
                      BUILD missing
                      REMOTE conancenter
                      SETTINGS ${settings})
endif()

ADD_DEFINITIONS(-DBOOST_ALL_DYN_LINK)

find_package(Boost)

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
