set(ui About.ui connectionstats.ui dosemon.ui entitystats.ui
  memgraph.ui numberOfEntities.ui RawStatisticsPage.ui SystemPicturePage.ui)

set(sources About.cpp connectionstats.cpp dosemon.cpp
  entitystats.cpp graphwidget.cpp internalfunctions.cpp main.cpp
  memgraph.cpp numberofentities.cpp RawStatisticsPage.cpp SystemPicturePage.cpp)

set(widget_headers About.h connectionstats.h dosemon.h entitystats.h
  graphwidget.h memgraph.h numberofentities.h RawStatisticsPage.h SystemPicturePage.h)

set(headers internalfunctions.h common_header.h)

if (NOT Qt5Widgets_FOUND)
  message(STATUS "Could not find Qt5, will not build dobexplorer")
  return()
endif()

QT5_WRAP_UI(ui_headers ${ui})
QT5_WRAP_CPP(moc_sources ${widget_headers} ${QT5_WRAP_CPP_OPTIONS})

mark_as_advanced(Qt5Core_DIR Qt5Gui_DIR Qt5Widgets_DIR)

ADD_EXECUTABLE(dobexplorer WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers} ${widget_headers} dobexplorer.rc)

if (Qt5Widgets_FOUND)
  TARGET_LINK_LIBRARIES(dobexplorer PRIVATE Qt5::Widgets)
else()
  TARGET_LINK_LIBRARIES(dobexplorer PRIVATE ${QT_LIBRARIES})
endif()

TARGET_LINK_LIBRARIES(dobexplorer PRIVATE
  dose_internal
  system_picture
  lluf_utils
  lluf_crash_reporter)

SAFIR_INSTALL(TARGETS dobexplorer DEVELOPMENT)
