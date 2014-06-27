# find and setup Qt4 for this project
SET(QT_USE_QTMAIN TRUE)
find_package(Qt4)
if (NOT Qt4_FOUND)
  message("Qt4 was not found, not building this example, which currently does not support Qt5")
  return()
endif()
include(${QT_USE_FILE})

QT4_WRAP_UI(ui_headers entitymw.ui messagedialog.ui servicedialog.ui categoryinfodialog.ui entitydialog.ui)
QT4_ADD_RESOURCES(rcc_sources entitymw.qrc)
QT4_WRAP_CPP(moc_sources categoryinfodialog.h entitydialog.h entitymw.h entitytablehandler.h messagedialog.h servicedialog.h ${QT4_WRAP_CPP_OPTIONS})

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
  ${QT_LIBRARIES})
  

#TODO: install?
