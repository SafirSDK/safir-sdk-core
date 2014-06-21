cmake_minimum_required(VERSION 2.8.12)

PROJECT(safir_sdk_core_examples CXX)

#TODO: !!
set(SAFIR_SDK "STOP_USING_SAFIR_SDK_VARIABLE" NOW PLEASE)
set(SAFIR_RUNTIME "STOP_USING_SAFIR_RUNTIME_VARIABLE" NOW PLEASE)

#list(APPEND CMAKE_MODULE_PATH /home/lars/safir-logan/lib/cmake/safir_sdk_core/)
set (SafirSDKCore_DIR /home/lars/safir-logan/lib/cmake/SafirSDKCore/)

find_package(SafirSDKCore REQUIRED)
#include(safir_sdk_core)

add_subdirectory(dou_files)
