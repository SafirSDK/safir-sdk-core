#TODO use CMakeParseArguments
FUNCTION(BUILD_GENERATED_LIBRARY NAME DEPENDENCIES)
  message("++ Name of project is ${NAME}")
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
  SET(dots_v_command ${PYTHON_EXECUTABLE} "${SAFIR_RUNTIME}/bin/dots_v.py" --dod-files=${dod_directory} --dependencies "${DEPENDENCIES}" --output-path=generated_code)
  
  ADD_CUSTOM_COMMAND(OUTPUT generated_code/tags generated_code/cpp generated_code/java generated_code/ada generated_code/dotnet
    COMMAND ${dots_v_command} ${SOURCE_DIR}
    COMMAND cmake -E copy /home/lars/logan/build/config/sdk/data/generated/build_cpp.cmake generated_code/cpp/CMakeLists.txt
    DEPENDS ${dod_files}
    COMMENT "Generating code for ${SOURCE_DIR}")
  #############

  #Build type needs to be set so that we can forward it to subdirectory builds
  if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "RelWithDebInfo")
  endif()

  #try to work out if make is being used, in that case we want to use $(MAKE) instead of cmake --build,
  #since that will allow the jobserver information to be propagated automatically
  #MESSAGE("Generator ${CMAKE_GENERATOR}")
  if ("${CMAKE_GENERATOR}" STREQUAL "Unix Makefiles")
    set (CPP_BUILD_COMMAND "$(MAKE)")
  else()
    #just guess that we can use 3 for parallelism...
    #TODO: do something intelligent for JOM and NMAKE
    set (CPP_BUILD_COMMAND "${CMAKE_COMMAND} --build . -- -j3")
  endif()

  #
  # Build CPP
  #
  # Build the generated code by running cmake in the cpp subdirectory that was created
  # in the code generation step.
  #
  ADD_CUSTOM_TARGET(dots_generated-${NAME}-cpp ALL
    DEPENDS generated_code/cpp
    COMMAND ${CMAKE_COMMAND} -G ${CMAKE_GENERATOR} -DSAFIR_PROJECT_NAME:string=${NAME}-cpp -DCMAKE_BUILD_TYPE:string=${CMAKE_BUILD_TYPE} -DDEPENDENCIES=${DEPENDENCIES} .
    COMMAND ${CPP_BUILD_COMMAND}
    VERBATIM
    WORKING_DIRECTORY generated_code/cpp)
  ############

  EXECUTE_PROCESS(
    COMMAND safir_show_config --module-install-dir ${NAME}
    RESULT_VARIABLE execute_result
    OUTPUT_VARIABLE DOU_INSTALL_DIRECTORY
    OUTPUT_STRIP_TRAILING_WHITESPACE)

  if (NOT execute_result EQUAL "0")
    MESSAGE(FATAL_ERROR "Could not find install directory for module ${NAME}. Is it configured in typesystem.ini?")
  endif()

  FILE(GLOB_RECURSE files_to_install *.dou *.dom *.namespace.txt)
  #message("files_to_install = ${files_to_install}")
  INSTALL(FILES ${files_to_install} DESTINATION ${DOU_INSTALL_DIRECTORY})
  INSTALL(CODE "EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} --build . --target install WORKING_DIRECTORY generated_code/cpp)")

ENDFUNCTION()


return()










