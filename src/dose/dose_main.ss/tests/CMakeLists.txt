ADD_EXECUTABLE(timer_test timer_test.cpp ../src/dose_main_timers.h ../src/dose_main_timers.cpp)

TARGET_LINK_LIBRARIES(timer_test PRIVATE
  dose_internal
  lluf_utils)

ADD_TEST(NAME TryStart_dose_main
  COMMAND ${PYTHON_EXECUTABLE}
  ${CMAKE_CURRENT_SOURCE_DIR}/try_start_dose_main.py
  --dose-main $<TARGET_FILE:dose_main>)

ADD_TEST(NAME dose_main_timers COMMAND timer_test)

SET_SAFIR_TEST_PROPERTIES(TEST TryStart_dose_main TIMEOUT 600)
SET_SAFIR_TEST_PROPERTIES(TEST dose_main_timers TIMEOUT 600)


#this executable is useful for signalling that persistence is ready in
#tests where dope is not built yet.
ADD_EXECUTABLE(signal_persistence_ready signal_persistence_ready.cpp)
TARGET_LINK_LIBRARIES(signal_persistence_ready PRIVATE
  dose_cpp)
