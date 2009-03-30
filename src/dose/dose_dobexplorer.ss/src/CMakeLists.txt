PROJECT (dobexplorer)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

# find and setup Qt4 for this project
SET(QT_USE_QTMAIN TRUE)
find_package(Qt4 REQUIRED)
include(${QT_USE_FILE})


FILE(GLOB ui *.ui)

QT4_WRAP_UI(ui_headers ${ui})

FILE(GLOB sources *.cpp)
FILE(GLOB headers [!ui_]*.h)
FILE(GLOB dose_com_sources ${SAFIR_SDK}/include/Safir/Dob/Internal/DoseComAux/*.cpp)

QT4_WRAP_CPP(moc_sources ${headers})

ADD_EXECUTABLE(dobexplorer WIN32 ${sources} ${ui_headers} ${moc_sources} ${dose_com_sources} ${headers})

TARGET_LINK_LIBRARIES(dobexplorer ${QT_LIBRARIES})

IF (${PLATFORM} STREQUAL Linux)
   TARGET_LINK_LIBRARIES(dobexplorer dose_internal dots_cpp)
ENDIF (${PLATFORM} STREQUAL Linux)


IF (${PLATFORM} STREQUAL Windows)
   TARGET_LINK_LIBRARIES(dobexplorer 
                         debug ACEd
                         optimized ACE)
ENDIF (${PLATFORM} STREQUAL Windows)

INSTALL(TARGETS dobexplorer
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
