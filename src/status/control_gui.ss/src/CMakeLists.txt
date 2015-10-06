set(ui MainWindow.ui)

set(sources Main.cpp MainWindow.cpp NodeTableModel.cpp )

set(widget_headers MainWindow.h)

set(headers NodeTableModel.h)

if (NOT Qt5Widgets_FOUND)
  message(STATUS "Could not find Qt5, will not build safir_control_gui")
  return()
endif()

QT5_WRAP_UI(ui_headers ${ui})
QT5_WRAP_CPP(moc_sources ${widget_headers} ${QT5_WRAP_CPP_OPTIONS})

mark_as_advanced(Qt5Core_DIR Qt5Gui_DIR Qt5Widgets_DIR)

ADD_EXECUTABLE(safir_control_gui WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers} ${widget_headers})

if (Qt5Widgets_FOUND)
  TARGET_LINK_LIBRARIES(safir_control_gui PRIVATE Qt5::Widgets)
else()
  TARGET_LINK_LIBRARIES(safir_control_gui PRIVATE ${QT_LIBRARIES})
endif()

TARGET_LINK_LIBRARIES(safir_control_gui PRIVATE
    dose_cpp
    lluf_utils
    lluf_crash_reporter
    )

SAFIR_INSTALL(TARGETS safir_control_gui)
