REM============================================================================
REM
REM Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
REM 
REM Created by: Saab AB
REM
REM Purpose: Run database script using BSQL.
REM          Note that the Mimer database 'SafirDb' must exist and run
REM          before this script is executed.
REM 
REM============================================================================

REM username: sysadm  password: sysadm  script: install_vehicledb.sql  database: SafirDb
(echo sysadm & echo sysadm & echo read 'install_vehicledb.sql'; & echo exit;) | "c:\Program Files\Mimer SQL 9.2\BSQL.exe" SafirDb

echo "Done..."

pause
:EOF
