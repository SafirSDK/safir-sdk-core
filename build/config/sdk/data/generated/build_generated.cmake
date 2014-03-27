#TODO use CMakeParseArguments
FUNCTION(BUILD_GENERATED_LIBRARY NAME DEPENDENCIES)
  message("++ Name of project is ${name}")
  message("++ Dependencies of project are '${DEPENDENCIES}'")
  SET (SOURCE_DIR ${${NAME}_SOURCE_DIR})
  message("++ SOURCE_DIR is '${SOURCE_DIR}'")

  INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)
  FIND_PACKAGE(PythonInterp)

  #
  # Generate code
  #
  # We set up some variables to point to dots_v and dod-files and directories,
  # and then we define the custom command that generates the code
  #
  SET(dod_directory ${SAFIR_RUNTIME}/data/text/dots/config/)
  FILE(GLOB dod_files ${dod_directory} *.dod)

  # TEMPORARY!!!!
  # TODO: REMOVE THIS STEP WHEN WE CAN!
  # Copy all the dou files to somewhere where we can get at them, so that we can use this as the xdir of dots_v
  #FILE(GLOB_RECURSE dou_files ${SOURCE_DIR} "*.dou")
  #set(all_dou_directory ${SAFIR_RUNTIME}/data/text/dots/all_dous)
  #file(INSTALL ${dou_files} DESTINATION ${all_dou_directory})

  SET(dots_v_command ${PYTHON_EXECUTABLE} "${SAFIR_RUNTIME}/bin/dots_v.py" --dod-files=${dod_directory} --dependencies ${SAFIR_RUNTIME}/data/text/dots/classes)
  
  ADD_CUSTOM_COMMAND(OUTPUT tags cpp java ada dotnet
    COMMAND ${dots_v_command} ${SOURCE_DIR}
    COMMAND cmake -E copy /home/lars/logan/build/config/sdk/data/generated/build_cpp.cmake cpp/CMakeLists.txt
    DEPENDS ${dod_files}
    COMMENT "Generating code for ${SOURCE_DIR}")
  #############

  #Build type needs to be set so that we can forward it to subdirectory builds
  if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "RelWithDebInfo")
  endif()

  #
  # Build CPP
  #
  # Build the generated code by running cmake in the cpp subdirectory that was created
  # in the code generation step.
  #
  ADD_CUSTOM_TARGET(dots_generated-${NAME}-cpp ALL
    DEPENDS cpp
    COMMAND ${CMAKE_COMMAND} -G ${CMAKE_GENERATOR} -DSAFIR_PROJECT_NAME:string=${NAME}-cpp -DCMAKE_BUILD_TYPE:string=${CMAKE_BUILD_TYPE} -DDEPENDENCIES=${DEPENDENCIES} .
    COMMAND ${CMAKE_COMMAND} --build . -- -j3
    WORKING_DIRECTORY cpp)
  ############

  #TODO fix -j stuff somehow...

  FILE(GLOB_RECURSE files_to_install *.dou *.dom *.namespace.txt)
  #message("files_to_install = ${files_to_install}")
  INSTALL(FILES ${files_to_install} DESTINATION ${SAFIR_RUNTIME}/data/text/dots/classes/)
  INSTALL(CODE "EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} --build . --target install WORKING_DIRECTORY cpp)")

ENDFUNCTION()


return()










