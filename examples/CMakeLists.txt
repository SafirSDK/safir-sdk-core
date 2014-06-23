cmake_minimum_required(VERSION 2.8.12)

PROJECT(safir_sdk_core_examples CXX)

#TODO: !!
set(SAFIR_SDK "STOP_USING_SAFIR_SDK_VARIABLE" NOW PLEASE)
set(SAFIR_RUNTIME "STOP_USING_SAFIR_RUNTIME_VARIABLE" NOW PLEASE)

#list(APPEND CMAKE_MODULE_PATH /home/lars/safir-logan/lib/cmake/safir_sdk_core/)
set (SafirSDKCore_DIR /home/lars/safir-logan/lib/cmake/SafirSDKCore/)

find_package(SafirSDKCore REQUIRED)


#TODO java & dotnet
add_subdirectory(dou_files)

#the code below needs to be able to include the Safir SDK Core headers
include_directories(${SAFIR_SDK_CORE_INCLUDE_DIRS})

#we also need boost
FIND_PACKAGE(Boost COMPONENTS system)

add_subdirectory(vehicleapp/src)
add_subdirectory(vehicledb/src)
add_subdirectory(vehiclemmi/src)
