cmake_minimum_required(VERSION 2.6)

PROJECT (dots_v NONE)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

execute_process(COMMAND xmlada-config -v
  OUTPUT_VARIABLE output)
if ("${output}" STREQUAL "")
  message("Failed to execute xmlada-config, using environment variable XML_ADA_VER to determine xmlada version")
  if ("$ENV{XML_ADA_VER}" STREQUAL "")
    message(FATAL_ERROR "Environment variable XML_ADA_VER is not set. Please set it to '3' or '4', depending on what version you have installed")
  endif()
  set(XML_ADA_VER $ENV{XML_ADA_VER})
else()
  string(REPLACE "XmlAda " "" XML_ADA_VER ${output})
  string(STRIP ${XML_ADA_VER} XML_ADA_VER)
endif()

string(SUBSTRING ${XML_ADA_VER} 0 1 XML_ADA_VER)

#message("Using XmlAda version : '${XML_ADA_VER}'") 

SET(gprmake_arguments -q -XBUILD=${CUSTOM_BUILD_TYPE} -XWarning_Level=Default)

SET(build_directories 
      program/lib-obj/Release 
      program/lib-obj/Debug 
      lib-obj/Release 
      lib-obj/Debug)

FILE(MAKE_DIRECTORY ${build_directories})

ADD_CUSTOM_TARGET(dots_v ALL
                  COMMAND gnatmake ${gprmake_arguments} -P dots_v-program.gpr -XXML_ADA_VER=${XML_ADA_VER}
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


INSTALL(FILES 
        data/AssemblyInfo.cs
        data/dots_generated-dotnet.snk
        data/CMakeLists.txt 
        data/Manifest.java.txt
        data/precompiled_header_for_cpp.h
        data/dots_generated_ada.gpr_template
        data/dots_generated_ada_library.gpr_template
        data/dots_generated_main_ada.gpr_template
        DESTINATION ${SAFIR_SDK}/dots/dots_generated)
INSTALL(FILES
        data/gen/CMakeLists.txt
       DESTINATION ${SAFIR_SDK}/dots/dots_generated/gen)
INSTALL(PROGRAMS
        data/dobmake.py
        DESTINATION ${SAFIR_RUNTIME}/bin)
