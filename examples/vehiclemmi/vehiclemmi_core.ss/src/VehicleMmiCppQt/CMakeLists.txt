cmake_minimum_required(VERSION 2.6)
PROJECT (VehicleMmiCppQt)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

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

TARGET_LINK_LIBRARIES(VehicleMmiCppQt ${QT_LIBRARIES} ${ACE_LIBRARY})

IF (NOT MSVC)
   TARGET_LINK_LIBRARIES(VehicleMmiCppQt dose_cpp swre_interface_cpp)
ENDIF ()

INSTALL(TARGETS VehicleMmiCppQt
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(VehicleMmiCppQt)