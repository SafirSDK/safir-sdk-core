ADD_EXECUTABLE(timer_test timer_test.cpp ../src/dose_main_timers.h ../src/dose_main_timers.cpp)

TARGET_LINK_LIBRARIES(timer_test PRIVATE 
  dose_internal
  dots_generated-Safir-cpp
  dots_cpp
  dots_internal
  dots_kernel
  lluf_utils
  lluf_startup_synchronizer
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

ADD_TEST(NAME TryStart_dose_main  
  COMMAND ${PYTHON_EXECUTABLE} 
  ${CMAKE_CURRENT_SOURCE_DIR}/try_start_dose_main.py
  --dose-main $<TARGET_FILE:dose_main>)

ADD_TEST(dose_main_timers timer_test)

SET_SAFIR_TEST_PROPERTIES(TEST TryStart_dose_main TIMEOUT 600)
SET_SAFIR_TEST_PROPERTIES(TEST dose_main_timers TIMEOUT 600)
