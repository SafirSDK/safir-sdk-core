REM==============================================================
REM
REM Copyright:      Saab Systems 2008
REM Author:         Jörgen Johansson
REM
REM Purpose:        Run OlibTester database DDL script using BSQL.
REM                 Note that the Mimer database 'SyskDb'
REM                 must exists and run before this script is
REM                 executed.
REM 
REM Improvements:   None.
REM
REM History:        2008-03-17      /stjrjo/   First version.
REM
REM==============================================================

REM username: sysadm  password: sysadm  script: mimer_database_structure.sql  database: SafirDb
(echo sysadm & echo sysadm & echo read 'mimer_database_structure.sql'; & echo exit;) | "c:\Program Files\Mimer SQL 9.2\BSQL.exe" SafirDb

echo "Done..."

pause
:EOF