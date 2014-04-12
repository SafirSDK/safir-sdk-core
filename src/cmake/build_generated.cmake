FUNCTION(BUILD_GENERATED_LIBRARY)
  cmake_parse_arguments(GEN "" "NAME" "DEPENDENCIES" ${ARGN})
  message("++ Will build generated library ${GEN_NAME} with dependencies '${GEN_DEPENDENCIES}'")

  if ("${GEN_NAME}" STREQUAL "")
    message(FATAL_ERROR "Invalid NAME passed to BUILD_GENERATED_LIBRARY")
  endif()

  if (NOT "${GEN_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to BUILD_GENERATED_LIBRARY '${GEN_UNPARSED_ARGUMENTS}'")
  endif()

  # 
  # Dependency resolution
  #
  # Create custom targets dummy that we just use to have somewhere to put the dependencies.
  # Then, when we need to resolve a targets dependencies, we recursively look at the dummy
  # targets to find all of the dependencies of our dependencies.
  add_custom_target(${GEN_NAME}-dou)
  set_target_properties(${GEN_NAME}-dou PROPERTIES
    DOU_DEPENDENCIES "${GEN_DEPENDENCIES}"
    DOU_DIR ${CMAKE_CURRENT_SOURCE_DIR})

  #recursively get all dependendencies
  function(GET_DEPS)
    cmake_parse_arguments(DEP "" "" "DEPENDENCIES" ${ARGN})

    FOREACH (DEP ${DEP_DEPENDENCIES})
      if (NOT "${DEP}" STREQUAL "")
        get_target_property(deps ${DEP}-dou DOU_DEPENDENCIES)
        if ("${deps}" STREQUAL "deps-NOTFOUND")
          MESSAGE(FATAL_ERROR "Could not resolve dependency '${DEP}'")
        else()
          GET_DEPS(DEPENDENCIES ${deps})
          set (GET_DEPS_RESULT ${GET_DEPS_RESULT} ${DEP})
        endif()
      endif()
    ENDFOREACH()
    if(GET_DEPS_RESULT)
      LIST(REMOVE_DUPLICATES GET_DEPS_RESULT)
    endif()
    set (GET_DEPS_RESULT ${GET_DEPS_RESULT} PARENT_SCOPE)
  endfunction()

  GET_DEPS(DEPENDENCIES ${GEN_DEPENDENCIES})
  set(ALL_DEPENDENCIES ${GET_DEPS_RESULT})
  ################


  #
  # Set up variables containing all dou files and all expected source code files
  #
  FILE(GLOB_RECURSE dou_files *.dou)
  #message("Dou files: ${dou_files}")

  #loop over all dou files
  foreach (dou ${dou_files})
    #message ("Dou ${dou}")
    string (REGEX REPLACE ".*/([a-zA-Z\\.0-9]*)\\.dou" "\\1" base_name ${dou})
    #message ("base_name ${base_name}")
    set (cpp_files ${cpp_files} generated_code/cpp/${base_name}.cpp)
  endforeach()

  #message("Cpp files: ${cpp_files}")
  ##############  
  

  #TODO: fix the paths below!
  #
  # Generate code
  #
  # We set up some variables to point to dots_v and dod-files and directories,
  # and then we define the custom command that generates the code
  #
  foreach(DEP ${ALL_DEPENDENCIES})
    get_target_property(sd ${DEP}-dou DOU_DIR)
    set(DOTS_V_DEPS ${DOTS_V_DEPS} ${sd})
  endforeach()

  FIND_PACKAGE(PythonInterp)

  SET(dod_directory ${safir_sdk_core_SOURCE_DIR}/dots/dots_v.ss/data/)
  FILE(GLOB dod_files ${dod_directory} *.dod)
  SET(dots_v_command ${PYTHON_EXECUTABLE} "${safir_sdk_core_SOURCE_DIR}/dots/dots_v.ss/dots_v.py" --dod-files=${dod_directory} --dependencies ${DOTS_V_DEPS} --output-path=generated_code)
  
  ADD_CUSTOM_COMMAND(
    OUTPUT ${cpp_files}

    COMMAND ${dots_v_command} ${CMAKE_CURRENT_SOURCE_DIR}

    DEPENDS ${dod_files} ${dou_files}
    COMMENT "Generating code for ${CMAKE_CURRENT_SOURCE_DIR}")

  #make clean target remove the generated_code directory
  SET_DIRECTORY_PROPERTIES(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES generated_code)
  #############



  
  #
  # Build CPP
  #
  ADD_LIBRARY(dots_generated-${GEN_NAME}-cpp SHARED ${cpp_files}) #TODO headers?
  
  target_include_directories(dots_generated-${GEN_NAME}-cpp
    PRIVATE ${safir_sdk_core_SOURCE_DIR}/dots/dots_v.ss/data
    PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/generated_code/cpp/include)
  
  target_link_libraries(dots_generated-${GEN_NAME}-cpp 
    dots_cpp
    dots_internal
    dots_kernel
    lluf_utils)

  FOREACH (DEP ${GEN_DEPENDENCIES})
    TARGET_LINK_LIBRARIES(dots_generated-${GEN_NAME}-cpp dots_generated-${DEP}-cpp)
  ENDFOREACH()
  ############

  #TODO:precompiled headers?!
  #TODO: installation


#  FILE(GLOB_RECURSE files_to_install *.dou *.dom *.namespace.txt)
#  #message("files_to_install = ${files_to_install}")
#  INSTALL(FILES ${files_to_install} DESTINATION ${DOU_INSTALL_DIRECTORY})
#  INSTALL(CODE "EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} --build . --target install WORKING_DIRECTORY generated_code/cpp)")

ENDFUNCTION()


return()










