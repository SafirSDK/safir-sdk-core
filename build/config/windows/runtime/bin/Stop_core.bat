@echo off
REM ############################################################################
REM #
REM # Usage: This script is used to kill the Safir SDK Core system.
REM #
REM ############################################################################

setlocal

taskkill /IM dose_main.exe

::Sleep for 2 seconds to let applications exit from the stop order issued by dose_main.
ping localhost -n 2 >/nul

taskkill /F /IM dope_bin2xml.exe 2>/nul
taskkill /F /IM dope_main.exe 2>/nul
taskkill /F /IM dose_main.exe 2>/nul
taskkill /F /IM foreach.exe 2>/nul
taskkill /F /IM lluf_logger_control.exe 2>/nul
taskkill /F /IM crash_dump_monitor.exe 2>/nul
taskkill /F /IM sate.exe 2>/nul

echo Safir SDK Core should now be terminated.

endlocal

pause
