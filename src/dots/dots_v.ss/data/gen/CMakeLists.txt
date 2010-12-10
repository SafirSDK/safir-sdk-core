cmake_minimum_required(VERSION 2.6)

PROJECT (dots_generated-code)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)
SET(dod_directory ${SAFIR_RUNTIME}/data/text/dots/config/)
FILE(GLOB dod_files ${dod_directory} *.dod)

if (REBUILD)
   message("Rebuilding all dou files...")
   FILE(GLOB_RECURSE dou_files ${dots_generated-code_SOURCE_DIR}/../*.dou)

   ADD_CUSTOM_COMMAND(OUTPUT tags
       COMMAND dots_v ARGS -dod=${dod_directory} -xdir=${dots_generated-code_SOURCE_DIR}/.. ${dots_generated-code_SOURCE_DIR}/..
       DEPENDS ${dod_files} ${dou_files}
       WORKING_DIRECTORY ..)
 
   ADD_CUSTOM_TARGET(generated_code ALL
                     DEPENDS tags)

else()
   message("Building only changed dou files...")
   FILE(GLOB_RECURSE dou_files RELATIVE ${dots_generated-code_SOURCE_DIR}/.. ../*.dou)

  #Loop over all dou files and add rules to generate them
  FOREACH(dou_file ${dou_files})
      #we need some of the generated files to use as dependencies for the 
      #sub-targets
      get_filename_component(dou_name ${dou_file} NAME) 
      STRING(REPLACE "." "-" txt_file ${dou_name})
      SET(txt_file "${dots_generated-code_SOURCE_DIR}/../tags/${txt_file}")
      STRING(REPLACE "-dou" ".txt" txt_file ${txt_file})

      SET(dependencies ${dependencies} ${txt_file})

     ADD_CUSTOM_COMMAND(OUTPUT  ${txt_file}
       COMMAND dots_v ARGS -dod=${dod_directory} -xdir=${dots_generated-code_SOURCE_DIR}/.. ${dou_file}
       DEPENDS ${dod_files} ../${dou_file}
       WORKING_DIRECTORY ..)
  ENDFOREACH()

  #Add several targets in order to enable parallel builds on multicore machines.
  #Ideally number_of_targets should be adapted to the build machine HW, but I
  #haven't figured out how to get this information, so for the time being, we use
  #a hardcoded value 

  SET(number_of_targets 8)

  LIST(LENGTH dependencies number_of_dependencies)
  MATH(EXPR target_size ${number_of_dependencies}/${number_of_targets})

  FOREACH(target RANGE 0 ${number_of_targets})
  
    SET(dependencies_subset)
    FOREACH(idx RANGE 1 ${target_size})

      LIST(LENGTH dependencies remaining_number_of_dependencies)
      IF(${remaining_number_of_dependencies} EQUAL 0)
        BREAK()
      ENDIF()

      LIST(GET dependencies 0 dependency)
      SET(dependencies_subset "${dependencies_subset};${dependency}")
      LIST(REMOVE_AT dependencies 0)

    ENDFOREACH()
  
    ADD_CUSTOM_TARGET(generated_code_${target} ALL
      DEPENDS ${dependencies_subset})
  
  ENDFOREACH()

endif()
