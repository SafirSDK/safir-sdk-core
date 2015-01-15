FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_cpp SHARED ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_cpp PRIVATE
  lluf_utils
  dose_dll

  PUBLIC
  dots_cpp
  safir_generated-Core-cpp)

SAFIR_INSTALL(TARGETS dose_cpp)
