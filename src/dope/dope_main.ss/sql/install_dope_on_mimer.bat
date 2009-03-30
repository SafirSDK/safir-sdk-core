REM==============================================================
REM
REM Copyright:      Saab Systems 2007
REM Author:         Jonas Thor
REM
REM Purpose:        Run DOPE database DDL script using BSQL.
REM                 Note that the Mimer database 'SafirDb'
REM                 must exists and run before this script is
REM                 executed.
REM 
REM Improvements:   None.
REM
REM History:        2007-01-09      /stjth/   First version.
REM                 2007-03-07       stlrha   Adapted for DOPE from the ALRT variant
REM
REM==============================================================

REM username: sysadm  password: sysadm  script: install_dope.sql  database: SafirDb
(echo sysadm & echo sysadm & echo read 'install_dope.sql'; & echo exit;) | "c:\Program Files\Mimer SQL 9.2\BSQL.exe" SafirDb

echo "Done..."

pause
:EOF