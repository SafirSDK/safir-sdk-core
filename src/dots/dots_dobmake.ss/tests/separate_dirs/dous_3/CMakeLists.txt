cmake_minimum_required(VERSION 2.8.12)

find_package(SafirSDKCore REQUIRED)

LINK_DIRECTORIES(/home/lars/tmp/bar/lib)
INCLUDE_DIRECTORIES(/home/lars/tmp/bar/include)
LIST(APPEND CSHARP_LIBRARY_PATHS /home/lars/tmp/bar/dotnet)
LIST(APPEND CMAKE_JAVA_INCLUDE_PATH
  /home/lars/tmp/bar/java/safir_generated-Test1-java.jar
  /home/lars/tmp/bar/java/safir_generated-Test2-java.jar)

#SET(ADD_SAFIR_GENERATED_LIBRARY_NO_JAVA True)
#SET(ADD_SAFIR_GENERATED_LIBRARY_NO_DOTNET True)

ADD_SAFIR_GENERATED_LIBRARY(
  NAME Test3
  DEPENDENCIES Test1 Test2
  DOU_FILES Dobmake.Test3)


#Can be absolute paths or relative paths. If relative, they will be prefixed with CMAKE_INSTALL_PREFIX
INSTALL_SAFIR_GENERATED_LIBRARY(
  TARGETS Test3
  CXX_BIN bin
  CXX_LIB lib
  CXX_INCLUDE include
  JAR java
  DOTNET dotnet
  DOU_BASE dou   #files will be installed to dou/Test1
  )

