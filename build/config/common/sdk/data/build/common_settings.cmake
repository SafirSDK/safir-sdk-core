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

#if we're using gcc we need to set up some things
if (UNIX)
   #link directory for libraries (will this work with gcc under windows?)
   LINK_DIRECTORIES(${SAFIR_RUNTIME}/lib)
   if (SAFIR_USER)
      LINK_DIRECTORIES(${SAFIR_USER}/runtime/lib)
   endif()

   #turn on more warnings and set up use of threads etc
   SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pthread")
   SET(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS} -Wall -pthread")
   SET (CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -DNDEBUG")

   #make sure we get the correct posix version
   ADD_DEFINITIONS(-D_POSIX_C_SOURCE=200809L)
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
   ADD_DEFINITIONS(/wd4503) #decorated name length exceeded
   ADD_DEFINITIONS(/wd4512) #assignment operator could not be generated

   #increase warning level
   # Use the highest warning level for visual studio.
   SET(CMAKE_CXX_WARNING_LEVEL 4)
   IF(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
     STRING(REGEX REPLACE "/W[0-4]" "/W4"
       CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
   ELSE()
     SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
   ENDIF()

   #Set linker flag /OPT:REF (eliminates functions and/or data that are never referenced) reduces size of executable to approx the same size as in Release mode. Also disable incremental linking to avoid warning.
   set(CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO  "${CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO} /OPT:REF /INCREMENTAL:NO")
   set(CMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO  "${CMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO} /OPT:REF /INCREMENTAL:NO")

endif ()

#Add some more boost library versions that we want to be able to use,
# just to try to be "future safe"
set (Boost_ADDITIONAL_VERSIONS 
  "1.40" "1.40.0" "1.41" "1.41.0" "1.42" "1.42.0" "1.43" "1.43.0" "1.44" "1.44.0" 
  "1.45" "1.45.0" "1.46" "1.46.0" "1.47" "1.47.0" "1.48" "1.48.0" "1.49" "1.49.0" 
  "1.50" "1.50.0" "1.51" "1.51.0" "1.52" "1.52.0" "1.53" "1.53.0" "1.54" "1.54.0" 
  "1.55" "1.55.0" "1.56" "1.56.0" "1.57" "1.57.0" "1.58" "1.58.0" "1.59" "1.59.0") 

set(Boost_NO_BOOST_CMAKE ON)
set(Boost_USE_MULTITHREADED ON)
set(Boost_FIND_QUIETLY 1)
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

#Let ctest output stdout on failure by default.
set(CTEST_OUTPUT_ON_FAILURE ON)

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
    STRING(REPLACE .exe ${CMAKE_RELWITHDEBINFO_POSTFIX}.pdb location ${location})

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

#MSVC variable is not set when using None as project languages
#as is done in the dotnet projects.
if (WIN32)
    SET(COMMON_CS_FLAGS "-warn:4")

    #this will make 32 bit builds work on a 64bit machine
    if (NOT "$ENV{Platform}" STREQUAL "X64")
      SET(COMMON_CS_FLAGS ${COMMON_CS_FLAGS} "-platform:x86")
    endif()
else()
    SET(COMMON_CS_FLAGS "-warn:4" "-nowarn:1587")
endif()


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

#just use these variables to avoid some cmake warnings 
#(they're set in the build script)
if(SAFIR_ADA_SUPPORT OR SAFIR_JAVA_SUPPORT)
endif()

if (UNIX)
  set(PATH_SEPARATOR ":")
else()
  set(PATH_SEPARATOR ";")
endif()

#This function traverses up from PROJECT_SOURCE_DIR to find the root of the 
#source code tree. It looks for some special files in that directory.
function (FIND_SAFIR_SDK_SOURCE_ROOT RESULT_VARIABLE)
  set (curdir ${PROJECT_SOURCE_DIR})
  while (NOT EXISTS "${curdir}/INSTALL.Linux.txt" 
      OR NOT EXISTS "${curdir}/INSTALL.Windows.txt")
    get_filename_component(parent ${curdir} PATH)
    if (parent STREQUAL curdir)
      set(${RESULT_VARIABLE} "SOURCE_ROOT-NOTFOUND")
      message (FATAL_ERROR "!!! Failed to find Safir SDK Core source code root.")
      return()
    endif()
    set(curdir ${parent})
  endwhile()
  set(${RESULT_VARIABLE} ${curdir} PARENT_SCOPE)
endfunction()

#also accepts an optional second argument TIMEOUT which will set test timeout
#in seconds
function (SET_SAFIR_TEST_PROPERTIES TEST_NAME) 
  FIND_SAFIR_SDK_SOURCE_ROOT(SAFIR_SOURCE_ROOT)
  if (ARGV1)
    SET_TESTS_PROPERTIES(${TEST_NAME} PROPERTIES TIMEOUT ${ARGV1})
  endif()

  set (pypath "$ENV{PYTHONPATH}${PATH_SEPARATOR}${SAFIR_SOURCE_ROOT}/src/tests/test_support/python")
  string(REGEX REPLACE "^${PATH_SEPARATOR}+" "" pypath ${pypath}) # remove any leading path separators

  SET_PROPERTY(TEST ${TEST_NAME}
    PROPERTY ENVIRONMENT 
    "SAFIR_TEST_CONFIG_OVERRIDE=${SAFIR_SOURCE_ROOT}/src/tests/test_support/test_config"
    "PYTHONPATH=${pypath}")
endfunction()
