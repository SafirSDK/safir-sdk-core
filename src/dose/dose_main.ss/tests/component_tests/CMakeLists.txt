ADD_TEST(NAME TryStart_dose_main
  COMMAND ${PYTHON_EXECUTABLE}
  ${CMAKE_CURRENT_SOURCE_DIR}/try_start_dose_main.py
  --dose-main $<TARGET_FILE:dose_main>)

SET_SAFIR_TEST_PROPERTIES(TEST TryStart_dose_main TIMEOUT 600)
