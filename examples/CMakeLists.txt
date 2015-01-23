cmake_minimum_required(VERSION 2.8.12)

PROJECT(safir_sdk_core_examples CXX)

find_package(SafirSDKCore REQUIRED)

add_subdirectory(dou_files)

#the code below needs to be able to include the Safir SDK Core headers
include_directories(${SAFIR_SDK_CORE_INCLUDE_DIRS})

#we also need boost
FIND_PACKAGE(Boost REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIRS})

#use dynamic linking with boost
ADD_DEFINITIONS(-DBOOST_ALL_DYN_LINK)

if (MSVC)
  ADD_DEFINITIONS(-D_WINSOCK_DEPRECATED_NO_WARNINGS)
  ADD_DEFINITIONS(-D_WIN32_WINNT=0x0501)
  ADD_DEFINITIONS(-DWIN32_LEAN_AND_MEAN)
endif()

#load macros for building dotnet stuff with cmake
include(${SAFIR_SDK_CORE_DOTNET_SETTINGS})
include(${SAFIR_SDK_CORE_JAVA_SETTINGS})

set (CSHARP_LIBRARY_PATHS "${SAFIR_SDK_CORE_CSHARP_LIBRARY_PATH}")

add_subdirectory(vehicleapp/src)
add_subdirectory(vehicledb/src)
add_subdirectory(vehiclemmi/src)
