project(build_files NONE)

cmake_minimum_required(VERSION 2.6)

FILE(TO_CMAKE_PATH $ENV{SAFIR_SDK} SAFIR_SDK)
FILE(TO_CMAKE_PATH $ENV{SAFIR_RUNTIME} SAFIR_RUNTIME)

SET(runtime_dirs "common/runtime")
SET(sdk_dirs "common/sdk")

#We need to make sure that syst_default.gpr directories exist
#since they need to be installed for ada stuff to build.
#This is needed for version control systems that don't handle
#empty directories.
FILE(MAKE_DIRECTORY 
     "${sdk_dirs}/ada/lib-obj/Release"
     "${sdk_dirs}/ada/lib-obj/Debug")


IF (WIN32)
  SET(runtime_dirs ${runtime_dirs} windows/runtime)
  SET(sdk_dirs ${sdk_dirs} windows/sdk)
ENDIF(WIN32)

IF (LINUX)
  SET(runtime_dirs ${runtime_dirs} linux/runtime)
  SET(sdk_dirs ${sdk_dirs} linux/sdk)
ENDIF(LINUX)

INSTALL(DIRECTORY ${runtime_dirs}
        DESTINATION ${SAFIR_RUNTIME}/..
        PATTERN ".svn" EXCLUDE)

INSTALL(DIRECTORY ${sdk_dirs}
        DESTINATION ${SAFIR_SDK}/..
        PATTERN ".svn" EXCLUDE
        ${additional_exclude})
