#This is here to allow the jenkins build scripts to exclude this from
#32on64 builds on 64 bit machines that do not have a 32bit Qt.
if (NOT "$ENV{SAFIR_SKIP_QT}" STREQUAL "")
  message(STATUS "Skipping the VehicleMmiCppQt example since the SAFIR_SKIP_QT env variable was set.")
  return()
endif()

# find and setup Qt6 or Qt5 for this project
find_package(Qt6 COMPONENTS Core Widgets)
if (NOT Qt6_FOUND)
  find_package(Qt5 COMPONENTS Core Widgets)
  if (NOT Qt5_FOUND)
    message(STATUS "Qt6 or Qt5 was not found, not building the VehicleMmiCppQt example.")
    return()
  endif()
endif()

QT_WRAP_UI(ui_headers entitymw.ui messagedialog.ui servicedialog.ui categoryinfodialog.ui entitydialog.ui)
QT_ADD_RESOURCES(rcc_sources entitymw.qrc)
QT_WRAP_CPP(moc_sources categoryinfodialog.h entitydialog.h entitymw.h entitytablehandler.h messagedialog.h servicedialog.h)

ADD_EXECUTABLE(VehicleMmiCppQt WIN32
  app.h app.cpp
  categoryinfodialog.h categoryinfodialog.cpp
  entitydialog.h entitydialog.cpp
  entitymw.h entitymw.cpp
  entitytablehandler.h entitytablehandler.cpp
  messagedialog.h messagedialog.cpp
  servicedialog.h servicedialog.cpp
  qtworkaround.h qtworkaround.cpp
  qtdispatcher.h qtdispatcher.cpp
  main.cpp
  ${moc_sources}
  ${rcc_sources}
  ${ui_headers})

TARGET_LINK_LIBRARIES(VehicleMmiCppQt
  safir_generated-Example-cpp
  ${SAFIR_SDK_CORE_LIBRARIES}
  Qt::Widgets)

TARGET_INCLUDE_DIRECTORIES(VehicleMmiCppQt PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

INSTALL(TARGETS VehicleMmiCppQt DESTINATION bin)
