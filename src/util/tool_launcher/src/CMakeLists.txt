set(ui MainWindow.ui)

set(sources Main.cpp MainWindow.cpp tool_launcher.rc)

set(widget_headers MainWindow.h)

if (NOT TARGET Qt5::Widgets)
  message(STATUS "Could not find Qt5, will not build safir_tool_launcher")
  return()
endif()

QT5_WRAP_UI(ui_headers ${ui})
QT5_WRAP_CPP(moc_sources ${widget_headers} ${QT5_WRAP_CPP_OPTIONS})
QT5_ADD_RESOURCES(rcc_sources tool_launcher.qrc)

ADD_EXECUTABLE(safir_tool_launcher WIN32 ${sources} ${ui_headers} ${moc_sources} ${widget_headers} ${rcc_sources})

TARGET_LINK_LIBRARIES(safir_tool_launcher PRIVATE
  Boost::filesystem
  Qt5::Widgets)

SAFIR_INSTALL(TARGETS safir_tool_launcher COMPONENT Tools)

