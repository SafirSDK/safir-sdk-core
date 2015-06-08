cmake_minimum_required(VERSION 2.8.12)

find_package(SafirSDKCore REQUIRED)

ADD_SAFIR_GENERATED_LIBRARY(
  NAME Test1
  DEPENDENCIES Core
  DOU_FILES Dobmake.Test1.dou)


#Can be absolute paths or relative paths. If relative, they will be prefixed with CMAKE_INSTALL_PREFIX
INSTALL_SAFIR_GENERATED_LIBRARY(
  TARGETS Test1
  CXX_RUNTIME bin
  CXX_LIBRARY lib
  CXX_INCLUDE include
  JAR java
  DOTNET dotnet
  DOU_BASE dou   #files will be installed to dou/Test1
  )

#Note: installation into GAC is up to user.
