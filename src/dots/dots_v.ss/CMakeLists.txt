cmake_minimum_required(VERSION 2.6)

PROJECT (dots_v)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

SET(gprmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE} -XWarning_Level=Default)

SET(build_directories 
      program/lib-obj/Release 
      program/lib-obj/Debug 
      lib-obj/Release 
      lib-obj/Debug)

FILE(MAKE_DIRECTORY ${build_directories})

ADD_CUSTOM_TARGET(dots_v ALL
                  COMMAND gnatmake ${gprmake_arguments} -P dots_v-program.gpr 
                  WORKING_DIRECTORY program
                  COMMENT "Building dots_v using gprmake"
                  VERBATIM)

if (WIN32)
   set (exe_name program/dots_v.exe)
elseif (UNIX)
   set (exe_name program/dots_v)
endif (WIN32)

INSTALL(PROGRAMS ${exe_name}
        DESTINATION ${SAFIR_RUNTIME}/bin)

FILE(GLOB dod_files data/*.dod)
INSTALL(FILES 
          ${dod_files} 
          data/dots_unit.xsd
        DESTINATION ${SAFIR_RUNTIME}/data/text/dots/config/)

if (SAFIR_BUILD_SYSTEM_VERSION OR UNIX)
    INSTALL(FILES 
            data/AssemblyInfo.cs
            data/dots_generated-dotnet.snk
            data/CMakeLists.txt 
            data/Manifest.java.txt
            data/precompiled_header_for_cpp.h
            data/Safir-java.namespace.txt
            data/dots_generated_ada_library.gpr_template
            data/dots_generated_ada.gpr
           DESTINATION ${SAFIR_SDK}/dots/dots_generated)
   INSTALL(FILES
            data/gen/CMakeLists.txt
           DESTINATION ${SAFIR_SDK}/dots/dots_generated/gen)
   INSTALL(PROGRAMS
            data/dobmake.py
           DESTINATION ${SAFIR_RUNTIME}/bin)
elseif(WIN32)
    INSTALL(FILES 
            data/AssemblyInfo.cs
            data/dots_generated-dotnet.snk
            data/Makefile
            data/Makefile_cpp
            data/Manifest.java.txt
            data/precompiled_header_for_cpp.h
            data/Safir-java.namespace.txt
            data/dots_generated_ada.gpr
        DESTINATION ${SAFIR_SDK}/dots/dots_generated)
else()
    MESSAGE(FATAL_ERROR "I don't know how to install myself on this platform!")
endif()
