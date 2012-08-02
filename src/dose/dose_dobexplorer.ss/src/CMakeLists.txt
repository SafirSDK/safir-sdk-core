cmake_minimum_required(VERSION 2.6)

PROJECT (dobexplorer)

INCLUDE("$ENV{SAFIR_SDK}/data/build/cmake_settings.txt")

# find and setup Qt4 for this project
SET(QT_USE_QTMAIN TRUE)
find_package(Qt4 REQUIRED)
include(${QT_USE_FILE})

FILE(GLOB ui *.ui)

QT4_WRAP_UI(ui_headers ${ui})

FILE(GLOB sources *.cpp)
FILE(GLOB headers [!ui_]*.h)
FILE(GLOB dose_com_sources ${SAFIR_SDK}/include/Safir/Dob/Internal/DoseComAux/*.cpp)

QT4_WRAP_CPP(moc_sources ${headers} OPTIONS -DBOOST_TT_HAS_OPERATOR_HPP_INCLUDED)

#The generated files end up in the binary dir, so we have to add that
#to the include path (in case the bin path is not the same as the source path)
INCLUDE_DIRECTORIES(${dobexplorer_BINARY_DIR})

ADD_EXECUTABLE(dobexplorer WIN32 ${sources} ${ui_headers} ${moc_sources} ${dose_com_sources} ${headers})

TARGET_LINK_LIBRARIES(dobexplorer ${QT_LIBRARIES} ${ACE_LIBRARY})

IF (NOT MSVC)
   TARGET_LINK_LIBRARIES(dobexplorer dose_internal dots_cpp)
ENDIF ()

INSTALL(TARGETS dobexplorer
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dobexplorer)