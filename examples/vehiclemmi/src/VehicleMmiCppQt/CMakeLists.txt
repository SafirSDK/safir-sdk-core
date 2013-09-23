cmake_minimum_required(VERSION 2.8)
project(VehicleMmiCppQt CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

# find and setup Qt4 for this project
SET(QT_USE_QTMAIN TRUE)
find_package(Qt4 REQUIRED)
include(${QT_USE_FILE})

FILE(GLOB ui *.ui)

QT4_WRAP_UI(ui_headers ${ui})

FILE(GLOB sources *.cpp)
FILE(GLOB headers [!ui_]*.h)
FILE(GLOB resources *.qrc)
  
QT4_ADD_RESOURCES(rcc_sources ${resources})
QT4_WRAP_CPP(moc_sources categoryinfodialog.h entitydialog.h entitymw.h entitytablehandler.h messagedialog.h servicedialog.h)

ADD_EXECUTABLE(VehicleMmiCppQt WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers} ${rcc_sources})

TARGET_LINK_LIBRARIES(VehicleMmiCppQt ${QT_LIBRARIES})
  
IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(VehicleMmiCppQt swre_interface_cpp dose_cpp dots_generated-Capabilities-cpp dots_generated-Safir-cpp dots_cpp logging_cpp lluf_utils)
ENDIF ()

INSTALL(TARGETS VehicleMmiCppQt
  RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
  LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
  ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(VehicleMmiCppQt)

