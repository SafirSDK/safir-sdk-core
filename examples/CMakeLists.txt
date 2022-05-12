PROJECT(safir_sdk_core_examples CXX)

cmake_minimum_required(VERSION 3.16.0)

set(CMAKE_CXX_STANDARD 17)

find_package(SafirSDKCore REQUIRED)
find_package(Boost)

add_subdirectory(dou_files)

#the code below needs to be able to include the Safir SDK Core headers
include_directories(${SAFIR_SDK_CORE_INCLUDE_DIRS})

if (MSVC)
  ADD_DEFINITIONS(-D_WINSOCK_DEPRECATED_NO_WARNINGS)
  ADD_DEFINITIONS(-D_WIN32_WINNT=0x0501)
  ADD_DEFINITIONS(-DWIN32_LEAN_AND_MEAN)
endif()

if (UNIX)
   #set up use of threads and turn on more warnings
   SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread -Wall -Wextra -Wpedantic -Wsuggest-override -Werror=suggest-override")
endif ()


#load macros for building dotnet stuff with cmake
include(${SAFIR_SDK_CORE_DOTNET_SETTINGS})
include(${SAFIR_SDK_CORE_JAVA_SETTINGS})

set (CSHARP_LIBRARY_PATHS "${SAFIR_SDK_CORE_CSHARP_LIBRARY_PATH}")

add_subdirectory(vehicleapp/src)
add_subdirectory(vehiclemmi/src)
