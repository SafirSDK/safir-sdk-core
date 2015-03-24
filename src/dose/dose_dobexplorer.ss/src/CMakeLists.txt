set(ui About.ui connectionstats.ui dosemon.ui entitystats.ui
  memgraph.ui numberOfEntities.ui RawStatisticsPage.ui SystemPicturePage.ui)

set(sources About.cpp connectionstats.cpp dosemon.cpp
  entitystats.cpp graphwidget.cpp internalfunctions.cpp main.cpp
  memgraph.cpp numberofentities.cpp RawStatisticsPage.cpp SystemPicturePage.cpp)

set(widget_headers About.h connectionstats.h dosemon.h entitystats.h
  graphwidget.h memgraph.h numberofentities.h RawStatisticsPage.h SystemPicturePage.h)

set(headers internalfunctions.h common_header.h)



# As moc files are generated in the binary dir, tell CMake
# to always look for includes there:
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# We default to Qt5 if we can find it and fall back to Qt4

find_package(Qt5Widgets QUIET)

if (Qt5Widgets_FOUND)
  message(STATUS "Building dobexplorer with Qt5")

  QT5_WRAP_UI(ui_headers ${ui})
  QT5_WRAP_CPP(moc_sources ${widget_headers} ${QT5_WRAP_CPP_OPTIONS})

  mark_as_advanced(Qt5Core_DIR Qt5Gui_DIR Qt5Widgets_DIR)
else()
  # Try to use Qt4 instead
  SET(QT_USE_QTMAIN TRUE)
  SET(QT4_FIND_QUIETLY TRUE)
  find_package(Qt4)

  if (NOT QT4_FOUND)
    message(STATUS "Could not find Qt4 or Qt5, will not build dobexplorer")
    return()
  endif()

  message(STATUS "Building dobexplorer with Qt4")

  include(${QT_USE_FILE})

  QT4_WRAP_UI(ui_headers ${ui})

  #qt 4.7 and 4.8 have a bug where moc will fail to parse some stuff in boost
  #so we have a test.h file which we try moc on, and if it fails we add a
  #workaround. See https://bugreports.qt-project.org/browse/QTBUG-22829
  execute_process(COMMAND moc -o moc_test.cxx test.h
    OUTPUT_VARIABLE moctest_output
    ERROR_VARIABLE moctest_output
    RESULT_VARIABLE moctest_result
    WORKING_DIRECTORY moctest)

  if (moctest_result) #if test failed
    set(QT4_WRAP_CPP_OPTIONS OPTIONS -DBOOST_TT_HAS_OPERATOR_HPP_INCLUDED)
  endif()

  QT4_WRAP_CPP(moc_sources ${widget_headers} ${QT4_WRAP_CPP_OPTIONS})

endif()

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
