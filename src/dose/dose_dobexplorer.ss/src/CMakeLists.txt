cmake_minimum_required(VERSION 2.8)

project(dobexplorer C CXX) #FindQt needs C

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

#qt 4.7 and 4.8 have a bug where moc will fail to parse some stuff in boost
#so we have a test.h file which we try moc on, and if it fails we add a 
#workaround
#https://bugreports.qt-project.org/browse/QTBUG-22829
execute_process(COMMAND moc -o moc_test.cxx test.h
  OUTPUT_VARIABLE moctest_output
  ERROR_VARIABLE moctest_output
  RESULT_VARIABLE moctest_result
  WORKING_DIRECTORY moctest)

if (moctest_result) #if test failed
  set(QT4_WRAP_CPP_OPTIONS OPTIONS -DBOOST_TT_HAS_OPERATOR_HPP_INCLUDED)
endif()

QT4_WRAP_CPP(moc_sources ${headers} ${QT4_WRAP_CPP_OPTIONS})

#The generated files end up in the binary dir, so we have to add that
#to the include path (in case the bin path is not the same as the source path)
INCLUDE_DIRECTORIES(${dobexplorer_BINARY_DIR})

ADD_EXECUTABLE(dobexplorer WIN32 ${sources} ${ui_headers} ${moc_sources} ${dose_com_sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS filesystem system)
TARGET_LINK_LIBRARIES(dobexplorer ${QT_LIBRARIES})

IF (NOT MSVC)
   TARGET_LINK_LIBRARIES(dobexplorer dots_cpp dots_generated-Safir-cpp dots_generated-cpp dose_internal lluf_utils ${Boost_LIBRARIES})
ENDIF ()

INSTALL(TARGETS dobexplorer
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dobexplorer)
