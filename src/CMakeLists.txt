cmake_minimum_required(VERSION 2.8.12)

PROJECT(safir_sdk_core CXX C)


#TODO: !!
set(SAFIR_SDK "/NO_THIS_IS_NOT_RIGHT")
set(SAFIR_RUNTIME "/NO_THIS_IS_NOT_RIGHT")

SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

#if we're using gcc we need to set up some things
if (UNIX)

   if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
     if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.9")
       set(stack_protector_option "-fstack-protector")
     else()
       set(stack_protector_option "-fstack-protector-strong")
     endif()
   endif()

   #turn on more warnings, set up use of threads, and set symbol visibility to hide as much as possible
   SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pthread -fvisibility=hidden -fvisibility-inlines-hidden -Bsymbolic -Wl,--exclude-libs=ALL ${stack_protector_option}")
   SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pthread -fvisibility=hidden -Bsymbolic  -Wl,--exclude-libs=ALL ${stack_protector_option}")

   SET (CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -DNDEBUG")

   #make sure we get the correct posix version
   ADD_DEFINITIONS(-D_POSIX_C_SOURCE=200809L)
endif ()

if (MSVC)
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

# Add some more boost library versions that we want to be able to use,
# just to try to be "future safe". This does not actually mean that we
# support all these versions, see our release information for that info.
set (Boost_ADDITIONAL_VERSIONS 
  "1.50" "1.50.0" "1.51" "1.51.0" "1.52" "1.52.0" "1.53" "1.53.0" "1.54" "1.54.0" 
  "1.55" "1.55.0" "1.56" "1.56.0" "1.57" "1.57.0" "1.58" "1.58.0" "1.59" "1.59.0") 

#Date_time is required on windows, even though we build header-only...
if (MSVC)
  set (BOOST_REQUIRED_ON_WINDOWS date_time)
endif()

set(Boost_NO_BOOST_CMAKE ON)
set(Boost_USE_MULTITHREADED ON)
set(Boost_FIND_QUIETLY True)

find_package(Boost COMPONENTS regex timer program_options filesystem thread system ${BOOST_REQUIRED_ON_WINDOWS} REQUIRED)
if(Boost_FOUND)
   include_directories(${Boost_INCLUDE_DIRS})
   link_directories(${Boost_LIBRARY_DIRS})
endif()

#Date_time is needed by boost thread on windows, so we monkey-patch that in, rather than adding link dependencies everywhere.
if (MSVC)
  set (Boost_THREAD_LIBRARY ${Boost_DATE_TIME_LIBRARY} ${Boost_THREAD_LIBRARY})
endif()

#make sure all linking is explicit as to what parts of boost it needs, so we clear the variable
#that "contains all".
SET(Boost_LIBRARIES "DONT_USE_Boost_LIBRARIES_VARIABLE")

#use dynamic linking with boost
ADD_DEFINITIONS(-DBOOST_ALL_DYN_LINK)

#dont use autolinking with boost
ADD_DEFINITIONS(-DBOOST_ALL_NO_LIB)

#disable deprecated functionality that we don't want
ADD_DEFINITIONS(-DBOOST_FILESYSTEM_NO_DEPRECATED)
ADD_DEFINITIONS(-DBOOST_SYSTEM_NO_DEPRECATED)

#we want to use boost::chrono instead of std::chrono and date_time for threads and asio
ADD_DEFINITIONS(-DBOOST_ASIO_DISABLE_STD_CHRONO)
ADD_DEFINITIONS(-DBOOST_THREAD_DONT_USE_DATETIME)

#Make Boost.Chrono header-only
ADD_DEFINITIONS(-DBOOST_CHRONO_HEADER_ONLY)

#Make sure we only use the header-only part of Boost.DateTime
ADD_DEFINITIONS(-DBOOST_DATE_TIME_NO_LIB)

#Set up boost for any test code (i.e. CheckCXXSourceCompiles stuff)
set(CMAKE_REQUIRED_INCLUDES ${Boost_INCLUDE_DIRS})
set(CMAKE_REQUIRED_DEFINITIONS 
  -DBOOST_ALL_DYN_LINK
  -DBOOST_FILESYSTEM_NO_DEPRECATED
  -DBOOST_SYSTEM_NO_DEPRECATED
  -DBOOST_ASIO_DISABLE_STD_CHRONO
  -DBOOST_THREAD_DONT_USE_DATETIME
  -DBOOST_CHRONO_HEADER_ONLY
  -DBOOST_DATE_TIME_NO_LIB)

if(MSVC)
   #We have a weird issue which causes a buffer overrun error when using Visual Studio 2013
   #and Boost 1.55 in 64 bit and release builds. 
   #Don't know if this is a bug in our code or in the compiler or in boost.
   #The workaround below disables some optimizations and all inlining in release builds
   #which appears to resolve the problem.
   if(MSVC_VERSION EQUAL 1800 AND Boost_VERSION EQUAL 105500 AND CMAKE_SIZEOF_VOID_P EQUAL 8)
     STRING(REGEX REPLACE "/Ob1" "/Ob0" CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")
     STRING(REGEX REPLACE "/O2" "/O1" CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")
     STRING(REGEX REPLACE "/Ob1" "/Ob0" CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE}")
     STRING(REGEX REPLACE "/O2" "/O1" CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE}")
   endif()
endif()


#Let ctest output stdout on failure by default.
set(CTEST_OUTPUT_ON_FAILURE ON)

if (MSVC)
  SET(CMAKE_DEBUG_POSTFIX "d")
endif()


#
# Find an ODBC installation, of either iODBC or UnixODBC
#
if (UNIX)
    FIND_PACKAGE(IODBC)
    if (IODBC_FOUND)
      INCLUDE_DIRECTORIES(${IODBC_INCLUDE_DIRS})
      SET(SAFIR_ODBC_LIBRARIES ${IODBC_LIBRARIES})
      ADD_DEFINITIONS(-DSAFIR_ODBC_IS_IODBC)
    else()
      message(STATUS "iODBC not found, trying UnixODBC")
      FIND_PACKAGE(UnixODBC)
      if (NOT UnixODBC_FOUND)
        message(FATAL_ERROR "Failed to find either iODBC or UnixODBC!")
      endif()
      INCLUDE_DIRECTORIES(${UnixODBC_INCLUDE_DIRS})
      SET(SAFIR_ODBC_LIBRARIES ${UnixODBC_LIBRARIES})
      ADD_DEFINITIONS(-DSAFIR_ODBC_IS_UNIXODBC)
    endif()
endif()
###########


# MACRO(INSTALL_DEBUG_INFO target)
#   if(MSVC)
#     #the problem here is to find out where the pdb file is located. It is located next to the binary
#     #in some directory which either the nmake/jom builds create or that the studio creates.

#     GET_TARGET_PROPERTY(debug_location ${target} LOCATION_Debug)
#     GET_TARGET_PROPERTY(relwithdebinfo_location ${target} LOCATION_RelWithDebInfo)

#     #replace binary's the extension with .pdb
#     #.exe --> .pdb
#     #.dll --> .pdb
#     STRING(REPLACE .dll .pdb debug_location ${debug_location})
#     STRING(REPLACE .exe .pdb debug_location ${debug_location})

#     STRING(REPLACE .dll .pdb relwithdebinfo_location ${relwithdebinfo_location})
#     STRING(REPLACE .exe .pdb relwithdebinfo_location ${relwithdebinfo_location})
    
#     #TODO
#     #Install the pdb files using the locations we just worked out.
#     #INSTALL(FILES ${debug_location} DESTINATION ${SAFIR_RUNTIME}/bin CONFIGURATIONS Debug)
#     #INSTALL(FILES ${relwithdebinfo_location} DESTINATION ${SAFIR_RUNTIME}/bin CONFIGURATIONS RelWithDebInfo)
    
#     UNSET(debug_location)
#     UNSET(relwithdebinfo_location)
#   endif()
# ENDMACRO()

function (SAFIR_INSTALL) 
  cmake_parse_arguments(INST "" "INCLUDE_DIR" "TARGETS" ${ARGN})

  if (NOT "${INST_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to SAFIR_INSTALL '${INST_UNPARSED_ARGUMENTS}'")
  endif()

  if (INST_TARGETS)
    INSTALL(TARGETS ${INST_TARGETS}
      RUNTIME DESTINATION bin
      LIBRARY DESTINATION lib
      ARCHIVE DESTINATION lib)
  endif()

  if (INST_INCLUDE_DIR)
    INSTALL(DIRECTORY ${INST_INCLUDE_DIR} DESTINATION include
      PATTERN ".svn" EXCLUDE
      PATTERN "*~" EXCLUDE)
  endif()

  #TODO debug info
endfunction()


if (UNIX)
  set(PATH_SEPARATOR ":")
else()
  set(PATH_SEPARATOR ";")
endif()

#We need to collect all library paths so that tests on windows (where there is no rpath equivalent)
#and java and dotnet tests on both platforms can find libraries.
if (MSVC)
  set_property(GLOBAL PROPERTY SAFIR_LIBRARY_PATH "$ENV{PATH}")
else()
  set_property(GLOBAL PROPERTY SAFIR_LIBRARY_PATH "$ENV{LD_LIBRARY_PATH}")
endif()

function(add_library)
  if (WIN32)
    file(TO_NATIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}" curdir)
  else()
    set(curdir "${CMAKE_CURRENT_BINARY_DIR}")
  endif()
  get_property(SAFIR_LIBRARY_PATH GLOBAL PROPERTY SAFIR_LIBRARY_PATH)
  #TODO: can we do the studio build path in a better way?
  #maybe use generator expressions, like in the build_generated path thing.
  #;${curdir}\\RelWithDebInfo;${curdir}\\Release;${curdir}\\Debug
  #cmake 3 appears to support generator expressions for test properties.
  if ("${SAFIR_LIBRARY_PATH}" STREQUAL "")
    set_property(GLOBAL PROPERTY SAFIR_LIBRARY_PATH "${curdir}")
    #message("SAFIR_LIBRARY_PATH set to ${curdir}")
  else()
    set_property(GLOBAL PROPERTY SAFIR_LIBRARY_PATH "${SAFIR_LIBRARY_PATH}${PATH_SEPARATOR}${curdir}")
    #message("SAFIR_LIBRARY_PATH set to ${SAFIR_LIBRARY_PATH}${PATH_SEPARATOR}${curdir}")
  endif()
  
  #call the real add_library function
  _add_library(${ARGN})
endfunction()

# Set up properties on tests
# Usage: SET_SAFIR_TEST_PROPERTIES(TEST <test_target> [TIMEOUT <timeout>] [CONFIG_OVERRIDE <config_dir>] [ENVIRONMENT ...])
#
# TIMEOUT (optional): Time in seconds after which the test is interrupted. If not specified will be set to 60s.
# CONFIG_OVERRIDE (optional): Absolute path to directory containing safir configuration. If not specified
#                             it will be set to a default configuration.
#
function (SET_SAFIR_TEST_PROPERTIES) 
  cmake_parse_arguments(PROP "" "TEST;TIMEOUT;CONFIG_OVERRIDE" "ENVIRONMENT" ${ARGN})

  if (NOT "${PROP_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to SET_SAFIR_TEST_PROPERTIES '${PROP_UNPARSED_ARGUMENTS}'")
  endif()

  if (NOT PROP_TEST)
    message(FATAL_ERROR "SET_SAFIR_TEST_PROPERTIES needs TEST argument")
  endif()


  if (PROP_TIMEOUT)
    SET_TESTS_PROPERTIES(${PROP_TEST} PROPERTIES TIMEOUT ${PROP_TIMEOUT})
  else()
    SET_TESTS_PROPERTIES(${PROP_TEST} PROPERTIES TIMEOUT 60)
  endif()

  if (NOT PROP_CONFIG_OVERRIDE)
    set(PROP_CONFIG_OVERRIDE "${safir_sdk_core_SOURCE_DIR}/tests/test_support/test_config")
  endif()

  set (pypath "$ENV{PYTHONPATH}${PATH_SEPARATOR}${safir_sdk_core_SOURCE_DIR}/tests/test_support/python")
  string(REGEX REPLACE "^${PATH_SEPARATOR}+" "" pypath ${pypath}) # remove any leading path separators

  #Add the library path to the environment
  get_property(SAFIR_LIBRARY_PATH GLOBAL PROPERTY SAFIR_LIBRARY_PATH)
  if (MSVC)
    #TODO: wtf?
    STRING(REPLACE "\;" ";" SAFIR_LIBRARY_PATH "${SAFIR_LIBRARY_PATH}")
    STRING(REPLACE ";" "\\;" SAFIR_LIBRARY_PATH "${SAFIR_LIBRARY_PATH}")
    set(PATH_ENVIRONMENT "PATH=${SAFIR_LIBRARY_PATH}")
  else()
    set(PATH_ENVIRONMENT "LD_LIBRARY_PATH=${SAFIR_LIBRARY_PATH}")
  endif()
  
  SET_PROPERTY(TEST ${PROP_TEST}
    PROPERTY ENVIRONMENT 
    "SAFIR_TEST_CONFIG_OVERRIDE=${PROP_CONFIG_OVERRIDE}"
    "SAFIR_SOURCE_ROOT=${safir_sdk_core_SOURCE_DIR}/../"
    "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
    "CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}"
    "PYTHONPATH=${pypath}"
    "${PATH_ENVIRONMENT}"
    ${PROP_ENVIRONMENT})
endfunction()

INCLUDE(build_generated)

include_directories(lluf/lluf_build_helpers.ss/src)

ADD_DEFINITIONS(-DSAFIR_NO_AUTOLINK)


#TODO: make this setting available from ccmake et al
#
# Set up .NET compilation
#
set(CSharp_FIND_QUIETLY True)
find_package(CSharp)
if (NOT CSHARP_FOUND)
  MESSAGE(WARNING "Failed to find the C# development tools, will not build .NET interfaces")
else()
  INCLUDE(CSharpMacros)
endif()

#
# Set up Java compilation
#
# We need to find both Java and JNI stuff, hence the convoluted mess below.
# Anyway, if Java_FOUND is set we're ok!
#
set(Java_FIND_QUIETLY True)
find_package(Java COMPONENTS Development Runtime)
if (Java_Development_FOUND AND Java_Runtime_FOUND)
  find_package(JNI QUIET)
  if (JNI_FOUND)
    #Found everything!
    SET(Java_FOUND True)
    INCLUDE(UseJava)
	SET(CMAKE_JAVA_COMPILE_FLAGS -encoding UTF-8)
  else()
    SET(Java_FOUND Java-NOTFOUND)
  endif()
  unset(Java_Development_FOUND)  
  unset(Java_Runtime_FOUND)
else()
  SET(Java_FOUND Java_NOTFOUND)
endif()
if (NOT Java_FOUND)
  MESSAGE(WARNING "Failed to find the Java development tools, will not build Java interfaces")
endif()

#TODO: javadoc?

function(SAFIR_GET_JAVA_CLASSPATH)
  cmake_parse_arguments(CP "" "" "JAVA_TARGETS" ${ARGN})

  if (NOT "${CP_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to SET_JAVA_CLASSPATH '${CP_UNPARSED_ARGUMENTS}'")
  endif()

  unset(SAFIR_JAVA_CLASSPATH)
  foreach (TARGET ${CP_JAVA_TARGETS})
    get_target_property(jar ${TARGET} JAR_FILE)
    if (NOT SAFIR_JAVA_CLASSPATH)
      SET(SAFIR_JAVA_CLASSPATH ${jar})
    else()
      SET(SAFIR_JAVA_CLASSPATH "${SAFIR_JAVA_CLASSPATH}${PATH_SEPARATOR}${jar}")
    endif()
  endforeach()

  set (SAFIR_JAVA_CLASSPATH "${SAFIR_JAVA_CLASSPATH}" PARENT_SCOPE)
endfunction()

FIND_PACKAGE(PythonInterp)
if(PYTHON_VERSION_STRING VERSION_LESS "2.7")
  message(FATAL_ERROR "Need Python version 2.7 or greater. Sorry.")
endif()

ENABLE_TESTING()

add_subdirectory(patch_check)
add_subdirectory(lluf)
add_subdirectory(logging)
add_subdirectory(dots)
add_subdirectory(dose)
add_subdirectory(douf)
add_subdirectory(swre)
add_subdirectory(dose/dose_test.ss)
add_subdirectory(foreach)
add_subdirectory(olib)
add_subdirectory(dope)

add_subdirectory(tests)

#TODO: move the test suite to outside dose, maybe into tests?


#TODO
#run_test_suite   src/tests/stop_orders_at_exit             stop_orders_at_exit

#Run static header checks to ensure only "good" headers have been included
#run_test_suite   src/tests/header_check                    header_check

#build_dir        docs/doxygen                              Release

#This will install the code examples under sdk/docs
#build_dir        docs/examples                             Release


SET(safir_config_files config/typesystem.ini config/locations.ini config/logging.ini)

if (UNIX)
  INSTALL(FILES ${safir_config_files}
    DESTINATION /etc/safir_sdk_core) #TODO: absolute path?! eller gor om lluf_config att vara PREFIX-aware? Funkar det med NSIS?
elseif(WIN32)
  INSTALL(FILES ${safir_config_files}
    DESTINATION $ENV{APPDATA}/safir_sdk_core/config) #TODO?!
endif()
