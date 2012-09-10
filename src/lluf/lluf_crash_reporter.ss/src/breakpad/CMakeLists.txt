if (MSVC)
  FILE(GLOB headers 
    ./client/windows/handler/*.h 
    ./client/windows/crash_generation/crash_generation_client.h 
    ./common/windows/guid_string.h)
  FILE(GLOB sources 
    ./client/windows/handler/*.cc 
    ./client/windows/crash_generation/crash_generation_client.cc 
    ./common/windows/guid_string.cc)
else()
  FILE(GLOB headers   
    ./client/linux/handler/*.h
    ./client/linux/crash_generation/*.h
    ./client/linux/minidump_writer/*.h
    ./client/linux/log/*.h
    ./client/*.h
    ./common/linux/*.h
    ./common/dwarf/*.h
    ./common/*.h
    )
  FILE(GLOB sources   
    ./client/linux/handler/exception_handler.cc
    ./client/linux/crash_generation/*.cc
    ./client/linux/minidump_writer/*.cc
    ./client/linux/log/*.cc
    ./client/*.cc
    ./common/linux/*.cc
    ./common/dwarf/*.cc
    ./common/*.cc
    ./common/*.c
    )
endif()

add_library(breakpad SHARED ${headers} ${sources})

ADD_DEFINITIONS(-DBREAKPAD_EXPORTS -DBREAKPAD_NO_TERMINATE_THREAD)

SET_TARGET_PROPERTIES(breakpad PROPERTIES LINKER_LANGUAGE CXX)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(breakpad  dl)
endif()

INSTALL(TARGETS breakpad 
  RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
  LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
  ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(breakpad)
