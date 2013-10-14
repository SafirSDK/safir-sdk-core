echo off
REM==============================================================
REM
REM Copyright:      Saab AB 2010
REM Author:         Mikael Wennerberg
REM
REM Purpose:        Run DOPE database script using MYSQL.
REM                 Administrator is sysadm with password sysadm
REM                 
REM                 
REM                 
REM 
REM Improvements:   None.
REM
REM History:        2010-03-22       stmiwn   First version.
REM
REM==============================================================

mysql -u sysadm -psysadm -e "source install_dope_mysql.sql"

echo "Done..."

pause
:EOF
