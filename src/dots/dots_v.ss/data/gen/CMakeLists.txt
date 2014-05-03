cmake_minimum_required(VERSION 2.8)

PROJECT (dots_generated-code NONE)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(PythonInterp)

SET(dod_directory ${SAFIR_RUNTIME}/data/text/dots/config/)
SET(dots_v_command ${PYTHON_EXECUTABLE} "${SAFIR_RUNTIME}/bin/dots_v.py" ARGS -dod=${dod_directory} -xdir=${dots_generated-code_SOURCE_DIR}/.. -j)

FILE(GLOB dod_files ${dod_directory} *.dod)

if (REBUILD)
   message("Rebuilding all dou files...")

   ADD_CUSTOM_COMMAND(OUTPUT tags
       COMMAND ${dots_v_command} ${dots_generated-code_SOURCE_DIR}/..
       DEPENDS ${dod_files}
       WORKING_DIRECTORY ..)
 
   ADD_CUSTOM_TARGET(generated_code ALL
                     DEPENDS tags)

else()
  message("Building only changed dou files...")
  FILE(GLOB_RECURSE dou_files ../*.dou)

  #Loop over all dou files and add rules to generate them
  FOREACH(dou_file ${dou_files})
      #we need some of the generated files to use as dependencies for the 
      #sub-targets
      get_filename_component(dou_name ${dou_file} NAME) 
      STRING(REPLACE "." "-" txt_file ${dou_name})
      SET(txt_file "${dots_generated-code_SOURCE_DIR}/../tags/${txt_file}")
      STRING(REPLACE "-dou" ".txt" txt_file ${txt_file})

      SET(dependencies ${dependencies} ${txt_file})

     ADD_CUSTOM_COMMAND(OUTPUT ${txt_file}
       COMMAND ${dots_v_command} ${dou_file}
       DEPENDS ${dod_files} ${dou_file}
       WORKING_DIRECTORY ..)
  ENDFOREACH()

  ADD_CUSTOM_TARGET(generated_code ALL
    DEPENDS ${dependencies})

endif()



###################################################
#    Installation rules for dou and dom files     #
###################################################
# INSTALL(CODE "FILE(REMOVE_RECURSE \"${SAFIR_RUNTIME}/data/text/dots/classes/\")")

# INSTALL(DIRECTORY ../ DESTINATION ${SAFIR_RUNTIME}/data/text/dots/classes/
#   FILES_MATCHING 
#   PATTERN "*.dou"
#   PATTERN "*.dom"
#   PATTERN "*.namespace.txt"

#   #below are directories to exclude
#   PATTERN "ada" EXCLUDE
#   PATTERN "cpp" EXCLUDE
#   PATTERN "dotnet" EXCLUDE
#   PATTERN "gen" EXCLUDE
#   PATTERN "java" EXCLUDE
#   PATTERN "tags" EXCLUDE
#   PATTERN "Release" EXCLUDE
#   PATTERN "Debug" EXCLUDE
#   PATTERN "RelWithDebInfo" EXCLUDE
#   PATTERN "MinSizeRel" EXCLUDE
#   )



