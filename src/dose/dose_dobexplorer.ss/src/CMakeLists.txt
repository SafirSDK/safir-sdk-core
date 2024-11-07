cmake_minimum_required(VERSION 3.27)

#make sure we link with qtmain.lib on windows
#if (NOT CMAKE_VERSION VERSION_LESS "2.8.11")
#  cmake_policy(SET CMP0020 NEW)
#endif()

project(dobexplorer C CXX) #FindQt needs C

INCLUDE("$ENV{SAFIR_SDK}/data/build/cmake_settings.txt")

FILE(GLOB ui *.ui)
FILE(GLOB sources *.cpp)
FILE(GLOB headers [!ui_]*.h)
FILE(GLOB dose_com_sources ${SAFIR_SDK}/include/Safir/Dob/Internal/DoseComAux/*.cpp)

# As moc files are generated in the binary dir, tell CMake
# to always look for includes there:
set(CMAKE_INCLUDE_CURRENT_DIR ON)

file(TO_CMAKE_PATH "$ENV{QTDIR}" SAFIR_QTDIR)
list(APPEND CMAKE_MODULE_PATH ${SAFIR_QTDIR})
list(APPEND CMAKE_PREFIX_PATH ${SAFIR_QTDIR})

find_package(Qt5 COMPONENTS Core Widgets Gui Network REQUIRED)

QT5_WRAP_UI(ui_headers ${ui})
QT5_WRAP_CPP(moc_sources ${headers} ${QT5_WRAP_CPP_OPTIONS})

FIND_PACKAGE(Boost COMPONENTS filesystem system)

ADD_EXECUTABLE(dobexplorer ${sources} ${ui_headers} ${moc_sources} ${dose_com_sources} ${headers})

target_link_libraries(dobexplorer  Qt5::Widgets Qt5::Network ${ACE_LIBRARY} ${Boost_LIBRARIES})

IF (NOT MSVC)
   TARGET_LINK_LIBRARIES(dobexplorer dose_internal dots_cpp)
ENDIF ()

INSTALL(TARGETS dobexplorer
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dobexplorer)
