#Set SAFIR_RUNTIME and SAFIR_SDK variables to paths that cmake likes.
FILE(TO_CMAKE_PATH "$ENV{SAFIR_SDK}" SAFIR_SDK)
FILE(TO_CMAKE_PATH "$ENV{SAFIR_RUNTIME}" SAFIR_RUNTIME)
file(TO_CMAKE_PATH "$ENV{SAFIR_USER}" SAFIR_USER)

#Default build release
if (NOT CMAKE_BUILD_TYPE)
   set(CMAKE_BUILD_TYPE "Release")
endif()

#add include path
INCLUDE_DIRECTORIES(${SAFIR_SDK}/include)
if (SAFIR_USER)
   INCLUDE_DIRECTORIES(${SAFIR_USER}/sdk/include)
endif()

#Export symbol files for post-mortem debugging. Set the appropriate flags
set(EXPORT_SYMBOLS true)

if (EXPORT_SYMBOLS)
    if(MSVC)
        set(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} /Zi")  #Set C, C++ and linker flags for release
        set(CMAKE_C_FLAGS_RELEASE    "${CMAKE_C_FLAGS_RELEASE} /Zi")  
        set(CMAKE_EXE_LINKER_FLAGS_RELEASE  "${CMAKE_EXE_LINKER_FLAGS_RELEASE} /DEBUG  /OPT:REF")
        set(CMAKE_SHARED_LINKER_FLAGS_RELEASE  "${CMAKE_SHARED_LINKER_FLAGS_RELEASE} /DEBUG  /OPT:REF")
    else()
        #set(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -g")  #Set C, C++ and linker flags for release
        #set(CMAKE_C_FLAGS_RELEASE    "${CMAKE_C_FLAGS_RELEASE} -g")  
        #set(CMAKE_EXE_LINKER_FLAGS_RELEASE  "${CMAKE_EXE_LINKER_FLAGS_RELEASE} -g")
        #set(CMAKE_SHARED_LINKER_FLAGS_RELEASE  "${CMAKE_SHARED_LINKER_FLAGS_RELEASE} -g")
    endif(MSVC)
endif(EXPORT_SYMBOLS)

#if we're using gcc we need to set up some things
if (UNIX)
   #link directory for libraries (will this work with gcc under windows?)
   LINK_DIRECTORIES(${SAFIR_RUNTIME}/lib)
   if (SAFIR_USER)
      LINK_DIRECTORIES(${SAFIR_USER}/runtime/lib)
   endif()
endif()

if (CMAKE_COMPILER_IS_GNUCXX)
   #turn on more warnings
   SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
endif()

if(UNIX)
   #define some common libraries
   SET(COMMON_LIBRARIES rt pthread)
endif ()

if (MSVC)
   LINK_DIRECTORIES(${SAFIR_SDK}/lib)
   if (SAFIR_USER)
      LINK_DIRECTORIES(${SAFIR_USER}/sdk/lib)
   endif()
   ADD_DEFINITIONS(-DNOMINMAX)
   ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE -D_SCL_SECURE_NO_DEPRECATE -D_CRT_NONSTDC_NO_DEPRECATE)
   ADD_DEFINITIONS(-D_UNICODE -DUNICODE)
   ADD_DEFINITIONS(-D_WIN32_WINNT=0x0501)
   ADD_DEFINITIONS(-DWIN32_LEAN_AND_MEAN)

   #use multiprocessing 
   #LAHA turned off to see if that removes the vs2010 internal compiler errors we're seeing.
   #SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP")

   #increase warning level
   # Use the highest warning level for visual studio.
   SET(CMAKE_CXX_WARNING_LEVEL 4)
   IF(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
     STRING(REGEX REPLACE "/W[0-4]" "/W4"
       CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
   ELSE()
     SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
   ENDIF()
endif ()

#Add some more boost library versions that we want to be able to use,
# just to try to be "future safe"
set (Boost_ADDITIONAL_VERSIONS "1.40" "1.40.0" "1.41" "1.41.0" "1.42" "1.42.0" "1.43" "1.43.0" "1.44" "1.44.0" "1.45" "1.45.0" "1.46" "1.46.0" "1.47" "1.47.0" "1.48" "1.48.0" "1.49" "1.49.0" "1.50" "1.50.0" "1.51" "1.51.0" "1.52" "1.52.0")

set(Boost_USE_MULTITHREADED ON)
set(Boost_NO_BOOST_CMAKE ON)
set (Boost_FIND_QUIETLY 1)
# Use boost from tower if it exists, unless specifically set to something else
if (NOT BOOST_ROOT AND "$ENV{BOOST_ROOT}" STREQUAL "")
  set(BOOST_ROOT ${SAFIR_SDK})
endif()
find_package(Boost)
if(Boost_FOUND)
   include_directories(${Boost_INCLUDE_DIRS})
   link_directories(${Boost_LIBRARY_DIRS})
else()
   unset(BOOST_ROOT)
   find_package(Boost)
   if(Boost_FOUND)
      include_directories(${Boost_INCLUDE_DIRS})
      link_directories(${Boost_LIBRARY_DIRS})
    else()
        message(FATAL_ERROR "Failed to find a boost installation!")
    endif()
endif()
set (Boost_FIND_QUIETLY 0)
#use dynamic linking with boost
ADD_DEFINITIONS(-DBOOST_ALL_DYN_LINK)

#Note that we do not request a specific boost.filesystem version,
#so we may get either v2 or v3. This is due to the fact that
#we're still supporting versions where v3 does not exist.
#When we drop support for those old versions, uncomment the line below,
#and add the same define to the CMAKE_REQUIRED_DEFINITIONS below.
#ADD_DEFINITIONS(-DBOOST_FILESYSTEM_VERSION=3)

#disable all deprecated functionality in boost.filesystem.
ADD_DEFINITIONS(-DBOOST_FILESYSTEM_NO_DEPRECATED)

#Set up boost for any test code (i.e. CheckCXXSourceCompiles stuff)
set(CMAKE_REQUIRED_INCLUDES ${Boost_INCLUDE_DIRS})
set(CMAKE_REQUIRED_DEFINITIONS -DBOOST_ALL_DYN_LINK -DBOOST_FILESYSTEM_NO_DEPRECATED)

if (MSVC AND NOT NO_LIBRARY_POSTFIXES)
  SET(CMAKE_DEBUG_POSTFIX "d")
endif()

MACRO(INSTALL_DEBUG_INFO target)
  if(MSVC)

    GET_TARGET_PROPERTY(location ${target} LOCATION)
    # fix for vs2010
    if (MSVC_VERSION EQUAL 1600)
       STRING(REPLACE "$(Configuration)" Debug location ${location})
    else()
       STRING(REPLACE "$(OutDir)" Debug location ${location})
    endif()

    STRING(REPLACE .dll ${CMAKE_DEBUG_POSTFIX}.pdb location ${location})
    STRING(REPLACE .exe .pdb location ${location})
    
    if (SAFIR_USER)
      INSTALL(FILES ${location} DESTINATION ${SAFIR_USER}/runtime/bin CONFIGURATIONS Debug)
    else()
      INSTALL(FILES ${location} DESTINATION ${SAFIR_RUNTIME}/bin CONFIGURATIONS Debug)
    endif()

    GET_TARGET_PROPERTY(location ${target} LOCATION)
    # fix for vs2010
    if (MSVC_VERSION EQUAL 1600)
       STRING(REPLACE "$(Configuration)" RelWithDebInfo location ${location})
    else()
       STRING(REPLACE "$(OutDir)" RelWithDebInfo location ${location})
    endif()

    STRING(REPLACE .dll ${CMAKE_RELWITHDEBINFO_POSTFIX}.pdb location ${location})

    if (SAFIR_USER)
      INSTALL(FILES ${location} DESTINATION ${SAFIR_USER}/runtime/bin CONFIGURATIONS RelWithDebInfo)
    else()
      INSTALL(FILES ${location} DESTINATION ${SAFIR_RUNTIME}/bin CONFIGURATIONS RelWithDebInfo)
    endif()

    if (EXPORT_SYMBOLS)
      GET_TARGET_PROPERTY(location ${target} LOCATION)
      #fix for vs2010
      if (MSVC_VERSION EQUAL 1600)
        STRING(REPLACE "$(Configuration)" Release location ${location})
      else()
        STRING(REPLACE "$(OutDir)" Release location ${location})
      endif()
      
      STRING(REPLACE .dll ${CMAKE_RELEASE_POSTFIX}.pdb location ${location})
      STRING(REPLACE .exe ${CMAKE_RELEASE_POSTFIX}.pdb location ${location})
      
      if (SAFIR_USER)
        INSTALL(FILES ${location} DESTINATION ${SAFIR_USER}/runtime/dump/Symbols CONFIGURATIONS Release)
      else()
        INSTALL(FILES ${location} DESTINATION ${SAFIR_RUNTIME}/dump/Symbols CONFIGURATIONS Release)
      endif()
    endif(EXPORT_SYMBOLS)

  endif()
ENDMACRO()

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${SAFIR_SDK}/data/build/)

if (MSVC)
    SET(COMMON_CS_FLAGS "-warn:4")
else()
    SET(COMMON_CS_FLAGS "-warn:4" "-nowarn:1587")
endif(MSVC)


#work out if we've got a configuration on the command line or if 
#we're running in an IDE.
if (CMAKE_CONFIGURATION_TYPES AND CMAKE_BUILD_TYPE)
    #MESSAGE("Both CMAKE_CONFIGURATION_TYPES and CMAKE_BUILD_TYPE are set! Using CMAKE_BUILD_TYPE as the CUSTOM_BUILD_TYPE")
    SET(CUSTOM_BUILD_TYPE ${CMAKE_BUILD_TYPE})
elseif(CMAKE_CONFIGURATION_TYPES)
    # fix for vs2010
    if (MSVC_VERSION EQUAL 1600)
       SET(CUSTOM_BUILD_TYPE "$(Configuration)")
    else()
       SET(CUSTOM_BUILD_TYPE "$(OutDir)")
    endif()
    
elseif(CMAKE_BUILD_TYPE)
    SET(CUSTOM_BUILD_TYPE ${CMAKE_BUILD_TYPE})
else()
    SET(CUSTOM_BUILD_TYPE "Release")
endif()


