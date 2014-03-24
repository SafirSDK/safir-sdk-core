project(build_files NONE)

cmake_minimum_required(VERSION 2.8)

#just use these two variables to avoid some cmake warnings 
#(they're set in the build script)
if(SAFIR_ADA_SUPPORT OR SAFIR_JAVA_SUPPORT)
endif()

FILE(TO_CMAKE_PATH $ENV{SAFIR_SDK} SAFIR_SDK)
FILE(TO_CMAKE_PATH $ENV{SAFIR_RUNTIME} SAFIR_RUNTIME)

SET(runtime_dir "runtime")
SET(sdk_dir "sdk")

#We need to make sure that syst_default.gpr directories exist
#since they need to be installed for ada stuff to build.
#This is needed for version control systems that don't handle
#empty directories.
FILE(MAKE_DIRECTORY 
     "${sdk_dir}/ada/lib-obj/RelWithDebInfo"
     "${sdk_dir}/ada/lib-obj/Debug")

INSTALL(DIRECTORY ${runtime_dir}
        DESTINATION ${SAFIR_RUNTIME}/..
        PATTERN ".svn" EXCLUDE)

INSTALL(DIRECTORY ${sdk_dir}
        DESTINATION ${SAFIR_SDK}/..
        PATTERN ".svn" EXCLUDE
        ${additional_exclude})
