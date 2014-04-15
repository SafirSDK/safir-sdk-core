FILE(GLOB ui *.ui)
FILE(GLOB sources *.cpp)
FILE(GLOB headers [!ui_]*.h)
SET(dose_com_sources ../../dose_communication.ss/src/dose_com/DoseOsSharedMem.cpp)

# As moc files are generated in the binary dir, tell CMake
# to always look for includes there:
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# We default to Qt5 if we can find it and fall back to Qt4

find_package(Qt5Widgets QUIET)

if (Qt5Widgets_FOUND)
  message(STATUS "Building dobexplorer with Qt5")
  QT5_WRAP_UI(ui_headers ${ui})
  QT5_WRAP_CPP(moc_sources ${headers} ${QT5_WRAP_CPP_OPTIONS})
else()
  # Try to use Qt4 instead
  SET(QT_USE_QTMAIN TRUE)
  find_package(Qt4)

  if (NOT QT4_FOUND)
    message(STATUS "Could not find Qt4 or Qt5, will not build dobexplorer")
    return()
  endif()

  message(STATUS "Building dobexplorer with Qt4")

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

else()
  TARGET_LINK_LIBRARIES(dobexplorer ${QT_LIBRARIES})
endif()

TARGET_LINK_LIBRARIES(dobexplorer
  dose_internal
  dots_generated-Safir-cpp
  dots_cpp 
  lluf_utils
  lluf_crash_reporter
  lluf_startup_synchronizer)

target_include_directories(dobexplorer PRIVATE ../../dose_communication.ss/src/defs/)
target_include_directories(dobexplorer PRIVATE ../../dose_communication.ss/src/dose_com/)

#TODO: hmmm, dobexplorer doesnt really need to link against dose_com, I added that to get the PUBLIC includes. Better way?

INSTALL(TARGETS dobexplorer
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dobexplorer)

