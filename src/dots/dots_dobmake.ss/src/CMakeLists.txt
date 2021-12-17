set(ui dobmake.ui)
set(sources dobmake.cpp main.cpp dobmake.rc)
set(headers dobmake.h BuildThread.h)

if (NOT TARGET Qt5::Widgets)
  message(STATUS "Could not find Qt5, will not build dobmake")
  return()
endif()

QT5_WRAP_UI(ui_headers ${ui})
QT5_WRAP_CPP(moc_sources ${headers} ${QT5_WRAP_CPP_OPTIONS})

ADD_EXECUTABLE(dobmake WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers})

TARGET_LINK_LIBRARIES(dobmake PRIVATE Qt5::Widgets)

SAFIR_INSTALL(TARGETS dobmake COMPONENT Development)
