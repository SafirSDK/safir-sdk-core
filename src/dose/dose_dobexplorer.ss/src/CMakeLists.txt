set(ui About.ui connectionstats.ui dosemon.ui entitystats.ui
  memgraph.ui numberOfEntities.ui RawStatisticsPage.ui SystemPicturePage.ui registrations.ui)

set(sources About.cpp connectionstats.cpp dosemon.cpp
  entitystats.cpp graphwidget.cpp internalfunctions.cpp main.cpp
  memgraph.cpp numberofentities.cpp RawStatisticsPage.cpp SystemPicturePage.cpp registrations.cpp)

set(widget_headers About.h connectionstats.h dosemon.h entitystats.h
  graphwidget.h memgraph.h numberofentities.h RawStatisticsPage.h SystemPicturePage.h registrations.h)

set(headers internalfunctions.h common_header.h)

if (NOT TARGET Qt::Widgets)
  message(STATUS "Could not find Qt, will not build dobexplorer")
  return()
endif()

QT_WRAP_UI(ui_headers ${ui})
QT_WRAP_CPP(moc_sources ${widget_headers} ${QT_WRAP_CPP_OPTIONS})

ADD_EXECUTABLE(dobexplorer WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers} ${widget_headers} dobexplorer.rc)

TARGET_LINK_LIBRARIES(dobexplorer PRIVATE
  dose_internal
  system_picture
  lluf_internal
  lluf_crash_reporter
  Qt::Widgets)

SAFIR_INSTALL(TARGETS dobexplorer COMPONENT Tools)
