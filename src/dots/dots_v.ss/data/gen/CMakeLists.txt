cmake_minimum_required(VERSION 2.6)

PROJECT (dots_generated-code)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB dou_files RELATIVE ${dots_generated-code_SOURCE_DIR}/.. ../*.dou)

SET(dod_directory ${SAFIR_RUNTIME}/data/text/dots/config/)
FILE(GLOB dod_files ${dod_directory} *.dod)

#Loop over all dou files and add rules to generate them
FOREACH(dou_file ${dou_files})
    #we need some of the generated files to use as dependencies for the 
    #sub-targets
    STRING(REPLACE "." "-" txt_file ${dou_file})
    SET(txt_file "${dots_generated-code_SOURCE_DIR}/../tags/${txt_file}")
    STRING(REPLACE "-dou" ".txt" txt_file ${txt_file})

    SET(dependencies ${dependencies} ${txt_file})

   ADD_CUSTOM_COMMAND(OUTPUT  ${txt_file}
     COMMAND dots_v ARGS -dod=${dod_directory} ${dou_file}
     DEPENDS ${dod_files} ../${dou_file}
     WORKING_DIRECTORY ..)
ENDFOREACH()

ADD_CUSTOM_TARGET(generated_code ALL
  DEPENDS ${dependencies})


