set(ui dobmake.ui)
set(sources dobmake.cpp main.cpp dobmake.rc)
set(headers dobmake.h BuildThread.h)

if (NOT Qt5Widgets_FOUND)
  message(STATUS "Could not find Qt5, will not build dobmake")
  return()
endif()

QT5_WRAP_UI(ui_headers ${ui})
QT5_WRAP_CPP(moc_sources ${headers} ${QT5_WRAP_CPP_OPTIONS})

mark_as_advanced(Qt5Core_DIR Qt5Gui_DIR Qt5Widgets_DIR)

ADD_EXECUTABLE(dobmake WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers})

if (Qt5Widgets_FOUND)
  TARGET_LINK_LIBRARIES(dobmake PRIVATE Qt5::Widgets)
else()
  TARGET_LINK_LIBRARIES(dobmake PRIVATE ${QT_LIBRARIES})
endif()

SAFIR_INSTALL(TARGETS dobmake DEVELOPMENT)
