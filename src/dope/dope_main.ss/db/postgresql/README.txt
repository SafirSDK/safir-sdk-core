Run the install.sql script as a database administrator to create user, database and
tables needed by Dope for persistent storage into a PostgreSQL database.

psql -U postgres -f install.sql

This assumes that you are able to log in to the postgres database as postgres user
without password. If that is not the case, then change the command line accordingly.

The user will be called "dopeuser" and have password "dopeuser". The database will
be named dope_db. If you wish to change any of these values you will have to reflect these
changes in the connection string in Safir.Dob.PersistenceParameters.

