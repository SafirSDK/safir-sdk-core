#TODO use CMakeParseArguments
FUNCTION(BUILD_GENERATED_LIBRARY NAME DEPENDENCIES)
  message("++ Name of project is ${NAME}")
  message("++ Dependencies of project are '${DEPENDENCIES}'")
  SET (SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
  message("++ SOURCE_DIR is '${SOURCE_DIR}'")

  FIND_PACKAGE(PythonInterp)

  FILE(GLOB_RECURSE dou_files *.dou)
  #message("Dou files: ${dou_files}")
  #string (REGEX REPLACE "([a-zA-Z\\.0-9]*)\\.dou" "generated_code/cpp/\\1.cpp" cpp_files "${dou_files}")
  #string (REGEX REPLACE "([a-zA-Z\\.0-9]*)\\.dou" "generated_code/dotnet/\\1.cs" cs_files ${dou_files})

  #loop over all dou files
  foreach (dou ${dou_files})
    #message ("Dou ${dou}")
    string (REGEX REPLACE ".*/([a-zA-Z\\.0-9]*)\\.dou" "\\1" base_name ${dou})
    #message ("base_name ${base_name}")
    set (cpp_files ${cpp_files} generated_code/cpp/${base_name}.cpp)
  endforeach()

  #message("Cpp files: ${cpp_files}")
  
  

  #TODO: fix the paths below!
  #
  # Generate code
  #
  # We set up some variables to point to dots_v and dod-files and directories,
  # and then we define the custom command that generates the code
  #
  SET(dod_directory ${safir_sdk_core_SOURCE_DIR}/dots/dots_v.ss/data/)
  FILE(GLOB dod_files ${dod_directory} *.dod)
  SET(dots_v_command ${PYTHON_EXECUTABLE} "${safir_sdk_core_SOURCE_DIR}/dots/dots_v.ss/dots_v.py" --dod-files=${dod_directory} --dependencies "${DEPENDENCIES}" --output-path=generated_code)
  
  ADD_CUSTOM_COMMAND(OUTPUT generated_code/tags generated_code/cpp generated_code/java generated_code/ada generated_code/dotnet ${cpp_files}
    COMMAND ${dots_v_command} ${SOURCE_DIR}
#    COMMAND cmake -E copy ${safir_sdk_core_SOURCE_DIR}/../build/config/sdk/data/generated/build_cpp.cmake generated_code/cpp/CMakeLists.txt
    DEPENDS ${dod_files} ${dou_files}
    COMMENT "Generating code for ${SOURCE_DIR}")
  #############


  ADD_LIBRARY(dots_generated-${NAME}-cpp SHARED ${cpp_files}) #TODO headers?
  
  target_include_directories(dots_generated-${NAME}-cpp
    PRIVATE ${safir_sdk_core_SOURCE_DIR}/dots/dots_v.ss/data
    PUBLIC generated_code/cpp/include)
  
  target_link_libraries(dots_generated-${NAME}-cpp dots_cpp)

  FOREACH (DEP ${DEPENDENCIES})
    TARGET_LINK_LIBRARIES(dots_generated-${NAME}-cpp dots_generated-${DEP}-cpp)
  ENDFOREACH()

  #TODO: installation

  #Build type needs to be set so that we can forward it to subdirectory builds
  #if (NOT CMAKE_BUILD_TYPE)
  #  set(CMAKE_BUILD_TYPE "RelWithDebInfo")
  #endif()

  #try to work out if make is being used, in that case we want to use $(MAKE) instead of cmake --build,
  #since that will allow the jobserver information to be propagated automatically
  #MESSAGE("Generator ${CMAKE_GENERATOR}")
  # if ("${CMAKE_GENERATOR}" STREQUAL "Unix Makefiles")
  #   set (CPP_BUILD_COMMAND "$(MAKE)")
  # else()
  #   #just guess that we can use 3 for parallelism...
  #   #TODO: do something intelligent for JOM and NMAKE
  #   set (CPP_BUILD_COMMAND "${CMAKE_COMMAND} --build . -- -j3")
  # endif()

  # #
  # # Build CPP
  # #
  # # Build the generated code by running cmake in the cpp subdirectory that was created
  # # in the code generation step.
  # #
  # ADD_CUSTOM_TARGET(dots_generated-${NAME}-cpp ALL
  #   DEPENDS generated_code/cpp
  #   COMMAND ${CMAKE_COMMAND} -G ${CMAKE_GENERATOR} -DSAFIR_PROJECT_NAME:string=${NAME}-cpp -DCMAKE_BUILD_TYPE:string=${CMAKE_BUILD_TYPE} -DDEPENDENCIES=${DEPENDENCIES} .
  #   COMMAND ${CPP_BUILD_COMMAND}
  #   VERBATIM
  #   WORKING_DIRECTORY generated_code/cpp)
  ############

  #TODO: this requires an installed typesystem.ini!
  #TODO: sucks below, cant do it this way...

#  EXECUTE_PROCESS(
#    COMMAND safir_show_config --module-install-dir ${NAME}
#    RESULT_VARIABLE execute_result
#    OUTPUT_VARIABLE DOU_INSTALL_DIRECTORY
#    ERROR_VARIABLE ERR
#    OUTPUT_STRIP_TRAILING_WHITESPACE)
#
#  if (NOT execute_result EQUAL "0")
#    MESSAGE(FATAL_ERROR "Could not find install directory for module ${NAME}. Is it configured in typesystem.ini?")
#  endif()

#  FILE(GLOB_RECURSE files_to_install *.dou *.dom *.namespace.txt)
#  #message("files_to_install = ${files_to_install}")
#  INSTALL(FILES ${files_to_install} DESTINATION ${DOU_INSTALL_DIRECTORY})
#  INSTALL(CODE "EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} --build . --target install WORKING_DIRECTORY generated_code/cpp)")

ENDFUNCTION()


return()










