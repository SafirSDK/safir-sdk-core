cmake_minimum_required(VERSION 2.8.12)

find_package(SafirSDKCore REQUIRED)

ADD_SAFIR_GENERATED_LIBRARY(
  NAME Test2
  DEPENDENCIES Test1)


#Can be absolute paths or relative paths. If relative, they will be prefixed with CMAKE_INSTALL_PREFIX
INSTALL_SAFIR_GENERATED_LIBRARY(
  TARGETS Test2 #TODO: check that target exists in build_generated
  CXX_BIN bin
  CXX_LIB lib
  CXX_INCLUDE include
  JAR java
  DOTNET dotnet
  DOU_BASE dou   #files will be installed to dou/Test1
  )


#TODO: multiple paths for dou files in typesystem.ini?

#GAC is up to user, nothing we do for them...


#TODO: should not need namespace file here
