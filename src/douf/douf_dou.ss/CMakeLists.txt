cmake_minimum_required(VERSION 2.8)

PROJECT (douf_time NONE)

INCLUDE(../../../build/config/sdk/data/generated/build_generated.cmake)

SET(DEPENDENCIES Safir)

BUILD_GENERATED_LIBRARY(${PROJECT_NAME} ${DEPENDENCIES})


