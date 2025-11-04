set(ui dobmake.ui)
set(sources dobmake.cpp main.cpp settings_manager.cpp dobmake.rc)
set(widget_headers dobmake.h BuildThread.h)
set(headers settings_manager.h)

if (NOT TARGET Qt::Widgets)
  message(STATUS "Could not find Qt, will not build dobmake")
  return()
endif()

QT_WRAP_UI(ui_headers ${ui})
QT_WRAP_CPP(moc_sources ${widget_headers} ${QT_WRAP_CPP_OPTIONS})

ADD_EXECUTABLE(dobmake WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers} ${widget_headers})

TARGET_LINK_LIBRARIES(dobmake PRIVATE lluf_config Qt::Widgets)

SAFIR_INSTALL(TARGETS dobmake COMPONENT Development)
