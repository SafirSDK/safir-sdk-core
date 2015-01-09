set(ui dobmake.ui)
set(sources dobmake.cpp main.cpp dobmake.rc)
set(headers dobmake.h BuildThread.h)

# As moc files are generated in the binary dir, tell CMake
# to always look for includes there:
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# We default to Qt5 if we can find it and fall back to Qt4

find_package(Qt5Widgets QUIET)

if (Qt5Widgets_FOUND)
  message(STATUS "Building dobmake with Qt5")

  QT5_WRAP_UI(ui_headers ${ui})
  QT5_WRAP_CPP(moc_sources ${headers} ${QT5_WRAP_CPP_OPTIONS})

  mark_as_advanced(Qt5Core_DIR Qt5Gui_DIR Qt5Widgets_DIR)
else()
  # Try to use Qt4 instead
  SET(QT_USE_QTMAIN TRUE)
  SET(QT4_FIND_QUIETLY TRUE)
  find_package(Qt4)

  if (NOT QT4_FOUND)
    message(STATUS "Could not find Qt4 or Qt5, will not build dobmake")
    return()
  endif()

  message(STATUS "Building dobmake with Qt4")

  include(${QT_USE_FILE})

  QT4_WRAP_UI(ui_headers ${ui})
  QT4_WRAP_CPP(moc_sources ${headers} ${QT4_WRAP_CPP_OPTIONS})
endif()

ADD_EXECUTABLE(dobmake WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers})

if (Qt5Widgets_FOUND)
  TARGET_LINK_LIBRARIES(dobmake Qt5::Widgets)
else()
  TARGET_LINK_LIBRARIES(dobmake ${QT_LIBRARIES})
endif()

#TARGET_LINK_LIBRARIES(dobmake
#  dose_internal
#  lluf_crash_reporter
#  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS dobmake)
