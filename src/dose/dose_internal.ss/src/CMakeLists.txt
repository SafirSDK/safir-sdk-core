FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_internal SHARED ${sources} ${headers})

target_include_directories(dose_internal PUBLIC .)

TARGET_LINK_LIBRARIES(dose_internal PRIVATE
  dots_generated-Safir-cpp
  dots_cpp 
  dots_internal
  dots_kernel
  lluf_id
  lluf_utils
  lluf_startup_synchronizer
  ${Boost_REGEX_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})


INSTALL(DIRECTORY Safir 
  DESTINATION ${SAFIR_SDK}/include
  PATTERN ".svn" EXCLUDE
  PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dose_internal
  RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
  LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
  ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_internal)
