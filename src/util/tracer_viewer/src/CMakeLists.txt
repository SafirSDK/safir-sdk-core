find_package(qt-advanced-docking-system CONFIG REQUIRED)

set(ui
  tracer_viewer_main_window.ui
  apps_widget.ui
  tracer_edit_widget.ui
  highlight_widget.ui
)

set(sources
  main.cpp
  tracer_viewer_main_window.cpp
  apps_widget.cpp
  tracer_status_model.cpp
  tracer_edit_widget.cpp
  tracer_data_receiver.cpp
  log_widget.cpp
  log_model.cpp
  highlight_widget.cpp
  settings_manager.cpp
  tracer_viewer.rc
)

set(widget_headers
  tracer_viewer_main_window.h
  apps_widget.h
  edit_button_delegate.h
  enabled_checkbox_delegate.h
  tracer_edit_widget.h
  tracer_status_model.h
  log_widget.h
  log_model.h
  tracer_data_receiver.h
  selectable_text_delegate.h
  highlight_widget.h
)

set(headers
  common.h
  highlight_rule.h
  settings_manager.h
)

if (NOT TARGET Qt::Widgets)
  message(STATUS "Could not find Qt, will not build Safir TraceView")
  return()
endif()

QT_WRAP_UI(ui_headers ${ui})
QT_WRAP_CPP(moc_sources ${widget_headers} ${QT_WRAP_CPP_OPTIONS})
QT_ADD_RESOURCES(rcc_sources
  resources.qrc
  ../../style/dark/darkstyle.qrc
  ../../style/light/lightstyle.qrc
  ../../style/customizations/customizations.qrc
)

ADD_EXECUTABLE(safir_tracer_viewer WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers} ${widget_headers} ${rcc_sources})

TARGET_LINK_LIBRARIES(safir_tracer_viewer PRIVATE
  lluf_config
  communication
  control_utils
  dose_cpp
  qt-advanced-docking-system::qt-advanced-docking-system
  Boost::regex
  Qt::Svg
  Qt::Widgets
  Qt::Network)

SAFIR_INSTALL(TARGETS safir_tracer_viewer COMPONENT Tools)
