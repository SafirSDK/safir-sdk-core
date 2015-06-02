

FIND_PACKAGE(Git QUIET)

FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/git_revision.h.in
"\#define SAFIR_SDK_CORE_GIT_REVISION \"@GIT_REVISION@\"\n"
)
FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/git_revision.cmake
"EXECUTE_PROCESS(
     COMMAND ${GIT_EXECUTABLE} log --pretty=format:\"%h\" -n 1
     OUTPUT_VARIABLE GIT_REVISION
     RESULT_VARIABLE RESULT
     ERROR_QUIET
 )

 if (RESULT)
   message(WARNING \"Failed to obtain git revision! safir_show_config --revision will not be able to show correct information.\")
   set(GIT_REVISION \"Unknown\")
 endif()

 CONFIGURE_FILE(\${SRC} \${DST} @ONLY)
")

ADD_CUSTOM_TARGET(
  git_revision
  COMMAND ${CMAKE_COMMAND} -D SRC=${CMAKE_CURRENT_BINARY_DIR}/git_revision.h.in
                           -D DST=${CMAKE_CURRENT_BINARY_DIR}/git_revision.h
                           -P ${CMAKE_CURRENT_BINARY_DIR}/git_revision.cmake
  COMMENT "Checking Git revision")

ADD_EXECUTABLE(safir_show_config show_config.cpp)
add_dependencies(safir_show_config git_revision)
TARGET_INCLUDE_DIRECTORIES(safir_show_config PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(safir_show_config PRIVATE
  lluf_config
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${Boost_FILESYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS safir_show_config)
