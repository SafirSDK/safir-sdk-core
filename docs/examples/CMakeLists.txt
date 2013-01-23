cmake_minimum_required(VERSION 2.8)

#this cmake file just installs the example code into $SAFIR_RUNTIME/sdk/docs/examples

INSTALL(
  DIRECTORY ../../examples/

  DESTINATION $ENV{SAFIR_SDK}/docs/examples/
  
  FILES_MATCHING
  PATTERN "*.dou"
  PATTERN "*.dom"
  PATTERN "*.h"
  PATTERN "*.cpp"
  PATTERN "*.ui"
  PATTERN "*.qrc"
  PATTERN "*.png"
  PATTERN "*.sql"
  PATTERN "*.bat"
  PATTERN "*.java"
  PATTERN "Manifest.txt"
  PATTERN "*.cs"
  PATTERN "*.resx"
  PATTERN "*.ico"
  PATTERN "*.bmp"
  PATTERN "*.csproj"
  PATTERN "CMakeLists.txt"
  PATTERN "ui_*.h" EXCLUDE
  PATTERN ".svn" EXCLUDE
  PATTERN "CMakeFiles" EXCLUDE
  PATTERN "bin" EXCLUDE
  PATTERN "Debug" EXCLUDE
  PATTERN "Release" EXCLUDE
  PATTERN "RelWithDebInfo" EXCLUDE
  PATTERN "MinSizeRel" EXCLUDE)

