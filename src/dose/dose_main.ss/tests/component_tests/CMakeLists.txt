ADD_TEST(NAME TryStart_safir
  COMMAND ${PYTHON_EXECUTABLE}
  ${CMAKE_CURRENT_SOURCE_DIR}/try_start_safir.py
  --safir-control $<TARGET_FILE:safir_control>
  --dose-main $<TARGET_FILE:dose_main>)

SET_SAFIR_TEST_PROPERTIES(TEST TryStart_safir TIMEOUT 600)
