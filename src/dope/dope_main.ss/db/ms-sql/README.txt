Run the install.sql script as a database administrator to create the database and table
needed by Dope for persistent storage into a Microsoft SQL Server database.

sqlcmd /E /S localhost\<database> /i install.sql

(where <database> is SQLEXPRESS or MSSQLSERVER, or whatever your database is called.)

IMPORTANT
---------
MS SQL Server does not support UTF-8, so you will have to set XmlDataColumnIsUtf8 in
Safir.Dob.PersistenceParameters to False.
