set(ui EntityViewerMainWindow.ui)

set(sources ColumnInfo.cpp  EntityModel.cpp  EntityView.cpp  EntityViewerApplication.cpp  main.cpp EntityViewerMainWindow.cpp)

set(widget_headers ColumnInfo.h  EntityModel.h  EntityViewerApplication.h  EntityView.h EntityViewerMainWindow.h)

if (NOT TARGET Qt5::Widgets)
  message(STATUS "Could not find Qt5, will not build safir_tool_launcher")
  return()
endif()

QT5_WRAP_UI(ui_headers ${ui})
QT5_WRAP_CPP(moc_sources ${widget_headers} ${QT5_WRAP_CPP_OPTIONS})

#This application does not use boost, yet
ADD_DEFINITIONS(-DSAFIR_NO_BOOST)

ADD_EXECUTABLE(safir_entity_viewer WIN32 ${sources} ${ui_headers} ${moc_sources} ${widget_headers})

TARGET_LINK_LIBRARIES(safir_entity_viewer PRIVATE
  douf_time_cpp
  dose_cpp
  lluf_config
  Qt5::Widgets
  Boost::date_time
)

SAFIR_INSTALL(TARGETS safir_entity_viewer COMPONENT Tools)

