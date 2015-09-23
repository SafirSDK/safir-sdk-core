Run the install.sql script as a database administrator to create user, database and
tables needed by Dope for persistent storage into a MySQL or MariaDB database.

mysql -u root -p -e "source install.sql"

The user will be called "dopeuser" and have password "dopeuser". The database will
be named dope_db. If you wish to change any of these values you will have to reflect these
changes in the connection string in Safir.Dob.PersistenceParameters.


IMPORTANT
---------
MySQL/MariaDB needs one setting changed in my.cnf, otherwise Dope cannot work with
large objects in the database.
Set max_allowed_packet in the [mysqld] section to something large, e.g. 10M or 100M.

