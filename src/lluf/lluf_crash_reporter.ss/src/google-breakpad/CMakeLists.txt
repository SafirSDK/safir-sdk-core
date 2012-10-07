#cmake_minimum_required(VERSION 2.8)
#INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

include(ExternalProject)
ExternalProject_Add(
   google-breakpad
   PREFIX ""
   SOURCE_DIR src
   BINARY_DIR bin
   SVN_REPOSITORY http://google-breakpad.googlecode.com/svn/trunk
   SVN_REVISION -r 991
   UPDATE_COMMAND "" #disable update since we're going against fixed revision
   CONFIGURE_COMMAND <SOURCE_DIR>/configure
   INSTALL_COMMAND ""
) 
