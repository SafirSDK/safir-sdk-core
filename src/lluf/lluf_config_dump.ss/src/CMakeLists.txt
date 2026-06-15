

FIND_PACKAGE(Git QUIET)

# The revision can be injected from the outside (-DSAFIR_GIT_REVISION=...). The
# Debian package build does this because it compiles from a "git archive"
# tarball that has no .git of its own - querying git there would either fail or
# (when a parent checkout happens to be present) silently report the wrong tree.
# For a normal in-tree build no override is passed and we query git directly.
if (SAFIR_GIT_REVISION)
  set(GIT_REVISION_SHORT "${SAFIR_GIT_REVISION}")
  set(GIT_REVISION_FULL "${SAFIR_GIT_REVISION_FULL}")
  set(GIT_BRANCH "${SAFIR_GIT_BRANCH}")
  set(GIT_STATUS "${SAFIR_GIT_STATUS}")
elseif (GIT_FOUND)
  execute_process(
    COMMAND "${GIT_EXECUTABLE}" describe --always --dirty --abbrev=7
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
    OUTPUT_VARIABLE GIT_REVISION_SHORT
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
    RESULT_VARIABLE GIT_RESULT
  )

  execute_process(
    COMMAND "${GIT_EXECUTABLE}" rev-parse HEAD
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
    OUTPUT_VARIABLE GIT_REVISION_FULL
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )

  execute_process(
    COMMAND "${GIT_EXECUTABLE}" rev-parse --abbrev-ref HEAD
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
    OUTPUT_VARIABLE GIT_BRANCH
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )

  execute_process(
    COMMAND "${GIT_EXECUTABLE}" diff-index --quiet HEAD --
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
    RESULT_VARIABLE GIT_DIRTY
    ERROR_QUIET
  )

  if (GIT_RESULT)
    message(WARNING "Failed to obtain git revision! safir_show_config --revision will not be able to show correct information.")
    set(GIT_REVISION_SHORT "Unknown")
    set(GIT_REVISION_FULL "Unknown")
    set(GIT_BRANCH "Unknown")
    set(GIT_STATUS "Unknown")
  else()
    if (GIT_DIRTY)
      set(GIT_STATUS "dirty - uncommitted changes")
    else()
      set(GIT_STATUS "clean")
    endif()
  endif()
else()
  set(GIT_REVISION_SHORT "Unknown")
  set(GIT_REVISION_FULL "Unknown")
  set(GIT_BRANCH "Unknown")
  set(GIT_STATUS "Unknown")
endif()

FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/git_revision.h
"#pragma once
#define SAFIR_SDK_CORE_GIT_REVISION \"${GIT_REVISION_SHORT}\"
#define SAFIR_SDK_CORE_GIT_REVISION_FULL \"${GIT_REVISION_FULL}\"
#define SAFIR_SDK_CORE_GIT_BRANCH \"${GIT_BRANCH}\"
#define SAFIR_SDK_CORE_GIT_STATUS \"${GIT_STATUS}\"
"
)

# Dummy target for compatibility (no longer needed as we generate the file directly)
ADD_CUSTOM_TARGET(git_revision)

ADD_EXECUTABLE(safir_show_config show_config.cpp)
add_dependencies(safir_show_config git_revision)
TARGET_INCLUDE_DIRECTORIES(safir_show_config PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(safir_show_config PRIVATE
  lluf_config
  Boost::program_options
  Boost::filesystem
  )

SAFIR_INSTALL(TARGETS safir_show_config)
