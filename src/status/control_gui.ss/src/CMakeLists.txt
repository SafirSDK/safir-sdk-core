set(ui MainWindow.ui)

set(sources Main.cpp MainWindow.cpp NodeTableModel.cpp )

set(widget_headers MainWindow.h)

set(headers NodeTableModel.h)

if (NOT TARGET Qt::Widgets)
  message(STATUS "Could not find Qt, will not build safir_control_gui")
  return()
endif()

QT_WRAP_UI(ui_headers ${ui})
QT_WRAP_CPP(moc_sources ${widget_headers} ${QT_WRAP_CPP_OPTIONS})

ADD_EXECUTABLE(safir_control_gui WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers} ${widget_headers})

TARGET_LINK_LIBRARIES(safir_control_gui PRIVATE
    dose_cpp
    lluf_internal
    lluf_crash_reporter
    Qt::Widgets)

SAFIR_INSTALL(TARGETS safir_control_gui)
