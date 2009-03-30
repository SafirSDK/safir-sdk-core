@echo off
set go=call START


cd %SAFIR_RUNTIME%\bin

%go% dose_test_cpp.exe 2 &
%go% dose_test_dotnet.exe 2 &
REM %go% dose_test_ada.exe 2 &
REM %go% java -Xcheck:jni -jar dose_test_java.jar 2 &
