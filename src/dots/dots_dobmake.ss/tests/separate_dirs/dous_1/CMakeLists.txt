cmake_minimum_required(VERSION 2.8.12)

find_package(SafirSDKCore REQUIRED)

add_safir_generated_library(
  NAME Test1
  DEPENDENCIES Core
  DOU_FILES Dobmake.Test1.dou)


#Can be absolute paths or relative paths. If relative, they will be prefixed with CMAKE_INSTALL_PREFIX
install_safir_generated_library(
  TARGETS Test1
  CXX_RUNTIME bin
  CXX_LIBRARY lib
  CXX_INCLUDE include
  JAR java
  DOTNET dotnet
  DOU_BASE dou   #files will be installed to dou/Test1
  )

#Note: installation into GAC is up to user.
