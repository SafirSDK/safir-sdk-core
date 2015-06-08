ADD_SAFIR_GENERATED_LIBRARY(
  NAME Test2
  DEPENDENCIES Test1
  DOU_FILES Dobmake.Test2.dou)


#Can be absolute paths or relative paths. If relative, they will be prefixed with CMAKE_INSTALL_PREFIX
INSTALL_SAFIR_GENERATED_LIBRARY(
  TARGETS Test2
  CXX_RUNTIME bin
  CXX_LIBRARY lib
  CXX_INCLUDE include
  JAR java
  DOTNET dotnet
  DOU_BASE dou   #files will be installed to dou/Test1
  )

#GAC is up to user, nothing we do for them...
