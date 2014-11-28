cmake_minimum_required(VERSION 2.8.12)

find_package(SafirSDKCore REQUIRED)


#SAFIR_INSTALL_DESTINATION_BIN
    #SAFIR_INSTALL_DESTINATION_LIB
    #SAFIR_INSTALL_DESTINATION_INCLUDE
    #SAFIR_INSTALL_DESTINATION_DOU_BASE
    #SAFIR_INSTALL_DESTINATION_JAR
    #SAFIR_INSTALL_DESTINATION_CSHARP


ADD_SAFIR_GENERATED_LIBRARY(
  NAME Test1
  SOURCES Dobmake.Test1.dou
  DEPENDENCIES Core)


#Can be absolute paths or relative paths. If relative, they will be prefixed with CMAKE_INSTALL_PREFIX
INSTALL_SAFIR_GENERATED_LIBRARY(
  TARGETS Test1
  CXX_BIN bin
  CXX_LIB lib
  CXX_INCLUDE include
  JAR java
  DOTNET dotnet
  DOU_BASE dou   #files will be installed to dou/Test1
  )


#TODO: multiple paths for dou files in typesystem.ini?

#GAC?
