#We want to allow just the Users's Guide and the Requirements Specification to be built
#on their own, without building all the source as well.
get_property(has_parent DIRECTORY PROPERTY PARENT_DIRECTORY)
if(NOT has_parent)
  cmake_minimum_required(VERSION 3.16.0)
  project(safir-sdk-core-docs NONE)
  SET(SAFIR_INSTALL_DESTINATION_DOC ${CMAKE_CURRENT_BINARY_DIR}/rendered_docs)
  SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../src/cmake ${CMAKE_MODULE_PATH})
endif()

find_package(Asciidoctor)

#Some targets are only included if we're building the source too
if (has_parent)
  add_subdirectory(doxygen)
  add_subdirectory(examples)
  add_subdirectory(manpages)
endif()

add_subdirectory(users_guide)
add_subdirectory(requirements)

install(FILES nuget-readme.md
  DESTINATION docs
  COMPONENT NuGet EXCLUDE_FROM_ALL
  RENAME readme.md
)
