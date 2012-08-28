# This script is used to work around the fact that the Mono resgen
# fails in spurious ways if the machine it is running on is heavily 
# loaded. This script checks the output of resgen, and if it is 
# determined to have failed in one of the suspect ways resgen will
# be run again. Two attempts are made.

cmake_minimum_required(VERSION 2.6)

if (NOT $ENV{VERBOSE} STREQUAL "")
  message ("resgen.cmake command line: ${RESGEN_EXECUTABLE} ${resx_file} ${resource_file}")
endif()

execute_process(
  COMMAND ${RESGEN_EXECUTABLE} ${resx_file} ${resource_file}
  RESULT_VARIABLE result
  OUTPUT_VARIABLE output
  ERROR_VARIABLE output)

if (NOT result EQUAL 0)
  message ("Mono resgen failed. This sometimes happens spuriously, so I will try to rerun the command twice more, hoping that it will succeed this time.")    
  execute_process(
    COMMAND ${RESGEN_EXECUTABLE} ${resx_file} ${resource_file}
    RESULT_VARIABLE result
    OUTPUT_VARIABLE output
    ERROR_VARIABLE output)
  
  if (NOT result EQUAL 0)
    message ("Mono resgen failed again; one last attempt:")
    execute_process(
      COMMAND ${RESGEN_EXECUTABLE} ${resx_file} ${resource_file}
      RESULT_VARIABLE result
      OUTPUT_VARIABLE output
      ERROR_VARIABLE output)
    
    if (NOT result EQUAL 0)
      message ("Mono resgen failed again, this time it is probably a real error:")
      message(FATAL_ERROR "${output}")
    endif()
  endif()
endif()

#success!
message("${output}")
