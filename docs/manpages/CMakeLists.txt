#no support for man pages on windows...
if (NOT UNIX)
  return()
endif()

if (NOT ASCIIDOC_FOUND)
  message (STATUS "Asciidoc not found, not generating man pages")
  return()
endif()

add_asciidoc_manpage(sate.1 COMPONENT Development)
add_asciidoc_manpage(dose_main.1 COMPONENT Runtime)
add_asciidoc_manpage(dope_main.1 COMPONENT Runtime)
add_asciidoc_manpage(dope_bin2xml.1 COMPONENT Runtime)
add_asciidoc_manpage(safir_show_config.1 COMPONENT Runtime)
#add_asciidoc_manpage(dose_main.1 COMPONENT Runtime)
#add_asciidoc_manpage(dose_main.1 COMPONENT Runtime)
#add_asciidoc_manpage(dose_main.1 COMPONENT Runtime)
