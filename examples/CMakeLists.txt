cmake_minimum_required(VERSION 2.8.12)

PROJECT(safir_sdk_core_examples CXX)

#TODO: !!
set(SAFIR_SDK "STOP_USING_SAFIR_SDK_VARIABLE" NOW PLEASE)
set(SAFIR_RUNTIME "STOP_USING_SAFIR_RUNTIME_VARIABLE" NOW PLEASE)

find_package(SafirSDKCore REQUIRED)

#TODO java & dotnet
add_subdirectory(dou_files)

#the code below needs to be able to include the Safir SDK Core headers
include_directories(${SAFIR_SDK_CORE_INCLUDE_DIRS})

#we also need boost
FIND_PACKAGE(Boost COMPONENTS system regex)
include_directories(${Boost_INCLUDE_DIRS})

#use dynamic linking with boost
ADD_DEFINITIONS(-DBOOST_ALL_DYN_LINK)

if (MSVC)
   ADD_DEFINITIONS(-D_WIN32_WINNT=0x0501)
endif()

add_subdirectory(vehicleapp/src)
add_subdirectory(vehicledb/src)
add_subdirectory(vehiclemmi/src)
