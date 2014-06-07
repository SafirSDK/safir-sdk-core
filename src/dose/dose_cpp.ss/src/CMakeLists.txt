FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_cpp SHARED ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_cpp PRIVATE 
  dose_dll

  PUBLIC dots_generated-Safir-cpp)

SAFIR_INSTALL(TARGETS dose_cpp)
