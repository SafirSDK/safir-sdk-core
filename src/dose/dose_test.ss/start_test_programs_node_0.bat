@echo off
set go=call START

cd %SAFIR_RUNTIME%\bin

%go% dose_test_cpp.exe 0 &
%go% dose_test_cpp.exe 1 &
%go% dose_test_dotnet.exe 0 &
%go% dose_test_dotnet.exe 1 &
REM %go% dose_test_ada.exe 0 &
REM %go% dose_test_ada.exe 1 &
REM %go% java -Xcheck:jni -jar dose_test_java.jar 0 &
REM %go% java -Xcheck:jni -jar dose_test_java.jar 1 &