# This script is used to work around the fact that the Mono resgen
# fails in spurious ways if the machine it is running on is heavily 
# loaded. This script checks the output of resgen, and if it is 
# determined to have failed in one of the suspect ways resgen will
# be run again. Two attempts are made.

cmake_minimum_required(VERSION 2.6)

if (NOT $ENV{VERBOSE} STREQUAL "")
  message ("resgen.cmake command line: ${RESGEN_EXECUTABLE} ${resx_file} ${resource_file}")
endif()

set(ATTEMPT 100)
#look for retries with delay
while(ATTEMPT GREATER 0)
  math (EXPR ATTEMPT "${ATTEMPT} - 1")

  execute_process(
    COMMAND ${RESGEN_EXECUTABLE} ${resx_file} ${resource_file}
    RESULT_VARIABLE result
    OUTPUT_VARIABLE output
    ERROR_VARIABLE output)

  if (result EQUAL 0)
    message("${output}")
    return()
  endif()
  message ("Mono resgen failed. This sometimes happens spuriously, so I will try to rerun the command, hoping that it will succeed this time.")
  #first attempt to sleep for a bit
  set(delay 5)
  # use the sleep command if we can find it
  find_program (SLEEP sleep)
  if (SLEEP)
    execute_process (
      COMMAND "${SLEEP}" ${delay}
      TIMEOUT ${delay}
      ERROR_QUIET OUTPUT_QUIET
      )
  elseif(WIN32)    #on windows we can work around not finding it by using ping
    find_program (PING ping)
    execute_process (
      COMMAND ${PING} 127.0.0.1 -n ${delay} -w 1000
      TIMEOUT ${delay}
      ERROR_QUIET OUTPUT_QUIET)
  else ()
    message (WARNING "Cannot delay retries as neither sleep nor ping command is available!")
  endif ()
endwhile()

if (NOT result EQUAL 0)
  message ("Mono resgen failed a lot of times,  so it is probably a real error:")
  message(FATAL_ERROR "${output}")
endif()
