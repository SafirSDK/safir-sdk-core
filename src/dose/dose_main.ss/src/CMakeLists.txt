FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_main ${sources} ${headers})
FIND_PACKAGE(Boost COMPONENTS regex thread system)

TARGET_LINK_LIBRARIES(dose_main 
  dose_cpp
  dose_internal 
  dose_com 
  dots_generated-Safir-cpp
  dots_cpp
  lluf_utils
  lluf_crash_reporter
  ${Boost_LIBRARIES})

INSTALL(TARGETS dose_main
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_main)

#TODO: general: only the really exported headers need to be installed! e.g. dose_internal headers dont ever need to be installed!!
