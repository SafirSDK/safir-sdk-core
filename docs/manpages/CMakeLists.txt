#no support for man pages on windows...
if (NOT UNIX)
  return()
endif()

if (NOT ASCIIDOCTOR_FOUND)
  message (STATUS "Asciidoctor not found, not generating man pages")
  return()
endif()

add_asciidoctor_manpage(sate.1 COMPONENT Tools)
add_asciidoctor_manpage(dose_main.1 COMPONENT Runtime)
add_asciidoctor_manpage(dope_main.1 COMPONENT Runtime)
add_asciidoctor_manpage(dope_bin2xml.1 COMPONENT Runtime)
add_asciidoctor_manpage(safir_show_config.1 COMPONENT Runtime)
add_asciidoctor_manpage(bd.1 COMPONENT Tools)
add_asciidoctor_manpage(crash_dump_monitor.1 COMPONENT Runtime)
add_asciidoctor_manpage(dobexplorer.1 COMPONENT Tools)
add_asciidoctor_manpage(dose_memory_stats.1 COMPONENT Runtime)
add_asciidoctor_manpage(dots_configuration_check.1 COMPONENT Runtime)
add_asciidoctor_manpage(dots_v.1 COMPONENT Development)
add_asciidoctor_manpage(foreach.1 COMPONENT Runtime)
add_asciidoctor_manpage(dobmake.1 COMPONENT Development)
add_asciidoctor_manpage(dobmake-batch.1 COMPONENT Development)
add_asciidoctor_manpage(system_picture_listener.1 COMPONENT Development)
add_asciidoctor_manpage(safir_control.1 COMPONENT Runtime)
add_asciidoctor_manpage(safir_websocket.1 COMPONENT Runtime)
add_asciidoctor_manpage(safir_control_cli.1 COMPONENT Runtime)
add_asciidoctor_manpage(safir_control_gui.1 COMPONENT Runtime)
add_asciidoctor_manpage(safir_status.1 COMPONENT Runtime)
add_asciidoctor_manpage(safir_resolver.1 COMPONENT Tools)
add_asciidoctor_manpage(safir_tool_launcher.1 COMPONENT Tools)
add_asciidoctor_manpage(safir_entity_viewer.1 COMPONENT Tools)
add_asciidoctor_manpage(safir_memory_allocator.1 COMPONENT Tools)
add_asciidoctor_manpage(safir_statistics_dump.1 COMPONENT Tools)
