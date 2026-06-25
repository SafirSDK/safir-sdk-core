#We want to allow just the Users's Guide and the Requirements Specification to be built
#on their own, without building all the source as well.
get_property(has_parent DIRECTORY PROPERTY PARENT_DIRECTORY)
if(NOT has_parent)
  cmake_minimum_required(VERSION 3.16.0)
  project(safir-sdk-core-docs NONE)
  SET(SAFIR_INSTALL_DESTINATION_DOC ${CMAKE_CURRENT_BINARY_DIR}/rendered_docs)
  SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../src/cmake ${CMAKE_MODULE_PATH})

  #A standalone docs build does not include the top-level CMakeLists, which is the
  #only place VERSION.txt is normally parsed - so SAFIR_VERSION_STRING would be
  #empty and the rendered docs would show a blank version tag. Parse it here too.
  #Keep in sync with the parse in the top-level CMakeLists.txt. Plain version only
  #(no git revision) for the shipped docs.
  file(STRINGS ${CMAKE_CURRENT_SOURCE_DIR}/../VERSION.txt version_strings)
  foreach (line IN LISTS version_strings)
    if (NOT line MATCHES "^#.*" AND NOT line STREQUAL "")
      string(REPLACE "=" ";" line ${line})
      list(GET line 0 key)
      list(GET line 1 value)
      if (key STREQUAL "MAJOR")
        set(SAFIR_VERSION_MAJOR "${value}")
      elseif(key STREQUAL "MINOR")
        set(SAFIR_VERSION_MINOR "${value}")
      elseif(key STREQUAL "PATCH")
        set(SAFIR_VERSION_PATCH "${value}")
      elseif(key STREQUAL "SUFFIX")
        set(SAFIR_VERSION_SUFFIX "${value}")
      else()
        message(FATAL_ERROR "Failed to parse VERSION.txt file")
      endif()
    endif()
  endforeach()
  SET(SAFIR_VERSION_STRING "${SAFIR_VERSION_MAJOR}.${SAFIR_VERSION_MINOR}.${SAFIR_VERSION_PATCH}${SAFIR_VERSION_SUFFIX}")
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
