FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_cpp SHARED ${sources} ${headers})

target_include_directories(dose_cpp PUBLIC .)

TARGET_LINK_LIBRARIES(dose_cpp PRIVATE 
  dose_internal
  dose_dll
  dots_generated-Safir-cpp)

SAFIR_INSTALL(TARGETS dose_cpp INCLUDE_DIR Safir)
