Run the install.sql script as a database administrator to create user, databank and
tables needed by Dope for persistent storage into a mimer database.

bsql --username=sysadm --query="read 'install.sql'" database

The user will be called "dopeuser" and have password "dopeuser". The databank will be
named dope_db. If you wish to change any of these values you will have to reflect these
changes in the connection string in Safir.Dob.PersistenceParameters.


IMPORTANT
---------
The sizes of the XMLDATA, BINARYDATA and BINARYSMALLDATA columns must correspond to the
size values in Safir.Dob.PersistenceParameters.
