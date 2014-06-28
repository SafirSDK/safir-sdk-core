# find and setup Qt5 for this project

find_package(Qt5Widgets QUIET)
if (NOT Qt5Widgets_FOUND)
  message("Qt5 was not found, not building the VehicleMmiCppQt example.")
  return()
endif()

QT5_WRAP_UI(ui_headers entitymw.ui messagedialog.ui servicedialog.ui categoryinfodialog.ui entitydialog.ui)
QT5_ADD_RESOURCES(rcc_sources entitymw.qrc)
QT5_WRAP_CPP(moc_sources categoryinfodialog.h entitydialog.h entitymw.h entitytablehandler.h messagedialog.h servicedialog.h)

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
  Qt5::Widgets)
  

#TODO: install?
