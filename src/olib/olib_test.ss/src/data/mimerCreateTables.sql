
CREATE IDENT olibuser AS USER USING 'olibuser';

GRANT DATABANK TO olibuser;
GRANT IDENT TO olibuser WITH GRANT OPTION;

DISCONNECT;


--==============================================================
-- Connect as the OLIB database owner.
--==============================================================

CONNECT TO 'SafirDb' USER 'olibuser' USING 'olibuser';


--==============================================================
-- Create databanks to hold the tables.
--==============================================================

CREATE DATABANK olib_db OF 1000 PAGES IN 'olib_db.dbf' WITH TRANS OPTION;


@
create table "TBLOLIBTEST"(
     "ID" INTEGER,
     "STRINGNAME" NATIONAL CHARACTER VARYING(10),
     "STRINGDESCRIPTION" NATIONAL CHARACTER VARYING(40),
     "INT32" INTEGER,
     "INT64" BIGINT,
     "FLOAT32" FLOAT(32), 
     "FLOAT64" DOUBLE PRECISION,
     "bool" INTEGER,
    constraint "SQL_PRIMARY_KEY_491" primary key("ID"),
    constraint "SQL_TABLE_CHECK_492" check("ID" is not null)
)
@

@
create table "TBLOLIBTESTBINARY"(
     "ID" INTEGER
   , "DATA" BINARY(10000)
   , constraint "SQL_PRIMARY_KEY_494" primary key("ID")
   , constraint "SQL_TABLE_CHECK_495" check("ID" is not null)
)
@

@
create table "TBLOLIBTESTBLOB"(
     "ID" INTEGER
   , "DATA" BINARY LARGE OBJECT(1048576)
   , constraint "SQL_PRIMARY_KEY_502" primary key("ID")
   , constraint "SQL_TABLE_CHECK_503" check("ID" is not null)
)
@

@
create table "TBLOLIBTESTNCLOB"(
     "ID" INTEGER
   , "DATA" NATIONAL CHAR LARGE OBJECT(10485760)
   , constraint "SQL_PRIMARY_KEY_497" primary key("ID")
   , constraint "SQL_TABLE_CHECK_498" check("ID" is not null)
)
@

@
create table "tblPerfTest"(
     "TypeID" BIGINT
   , "InstanceNo" INTEGER
   , "Data" CHARACTER VARYING(2100)
)
@
