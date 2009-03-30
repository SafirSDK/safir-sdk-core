REM
REM Run this script to generate safir_dob_DoseInterface.h
REM

make --directory=..\..\java

javah -classpath ..\..\java\bin safir.swreports.Library safir.application.Library

pause
