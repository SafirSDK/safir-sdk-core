#no support for man pages on windows...
if (NOT UNIX)
  return()
endif()

if (NOT ASCIIDOC_FOUND)
  message (STATUS "Asciidoc not found, not generating man pages")
  return()
endif()

add_asciidoc_manpage(sate.1 COMPONENT Development)
