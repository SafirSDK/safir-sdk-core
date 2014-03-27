cmake_minimum_required(VERSION 2.8)

#make sure we link with qtmain.lib on windows
if (NOT CMAKE_VERSION VERSION_LESS "2.8.11")
  cmake_policy(SET CMP0020 NEW)
endif()

project(dobexplorer C CXX) #FindQt needs C

INCLUDE("$ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake")

FILE(GLOB ui *.ui)
FILE(GLOB sources *.cpp)
FILE(GLOB headers [!ui_]*.h)
FILE(GLOB dose_com_sources ${SAFIR_SDK}/include/Safir/Dob/Internal/DoseComAux/*.cpp)

# As moc files are generated in the binary dir, tell CMake
# to always look for includes there:
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# We default to Qt5 if we can find it and fall back to Qt4

if (CMAKE_VERSION VERSION_GREATER "2.8.10") #need cmake 2.8.11 or greater for qt5
  find_package(Qt5Widgets QUIET)
endif()

if (Qt5Widgets_FOUND)
  message("-- Building dobexplorer with Qt5")
  set(CMAKE_AUTOMOC ON)
  QT5_WRAP_UI(ui_headers ${ui})
else()
  # Try to use Qt4 instead
  SET(QT_USE_QTMAIN TRUE)
  find_package(Qt4 REQUIRED)

  message("-- Building dobexplorer with Qt4")

  include(${QT_USE_FILE})
  
  QT4_WRAP_UI(ui_headers ${ui})
   
  #qt 4.7 and 4.8 have a bug where moc will fail to parse some stuff in boost
  #so we have a test.h file which we try moc on, and if it fails we add a 
  #workaround. See https://bugreports.qt-project.org/browse/QTBUG-22829
  execute_process(COMMAND moc -o moc_test.cxx test.h
    OUTPUT_VARIABLE moctest_output
    ERROR_VARIABLE moctest_output
    RESULT_VARIABLE moctest_result
    WORKING_DIRECTORY moctest)
  
  if (moctest_result) #if test failed
    set(QT4_WRAP_CPP_OPTIONS OPTIONS -DBOOST_TT_HAS_OPERATOR_HPP_INCLUDED)
  endif()
  
  QT4_WRAP_CPP(moc_sources ${headers} ${QT4_WRAP_CPP_OPTIONS})

endif()

ADD_EXECUTABLE(dobexplorer WIN32 ${sources} ${ui_headers} ${moc_sources} ${dose_com_sources} ${headers})

if (Qt5Widgets_FOUND)
  qt5_use_modules(dobexplorer Widgets)
  QT5_WRAP_CPP(moc_sources ${headers} ${QT5_WRAP_CPP_OPTIONS})

  if (CMAKE_VERSION VERSION_LESS "2.8.11")
    target_link_libraries(dobexplorer ${Qt5Core_QTMAIN_LIBRARIES})
  endif()
else()
  TARGET_LINK_LIBRARIES(dobexplorer ${QT_LIBRARIES})
endif()

FIND_PACKAGE(Boost COMPONENTS filesystem system)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(dobexplorer dots_cpp dots_generated-Safir-cpp dose_internal lluf_utils lluf_crash_reporter ${Boost_LIBRARIES})
ENDIF ()

INSTALL(TARGETS dobexplorer
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dobexplorer)
