@
create table "TBLOLIBTEST"(
     "UNITID" INTEGER
   , "CALLSIGN" NATIONAL CHARACTER VARYING(6)
   , "COMBATREADINESS" INTEGER
   , "COMBATREADINESSDESCRIPTION" NATIONAL CHARACTER VARYING(100)
   , "UNITSIZEID" NATIONAL CHARACTER VARYING(50)
   , "UNITIDENTITYID" NATIONAL CHARACTER VARYING(50)
   , "LATITUDE" FLOAT
   , "LONGITUDE" FLOAT
   , "SPEED" REAL
   , "COURSE" REAL
   , "MEASUREMENTTIME" TIMESTAMP(8)
   , "ISALIVE" INTEGER
   , "ALARGEINT" BIGINT
   , constraint "SQL_PRIMARY_KEY_491" primary key("UNITID")
   , constraint "SQL_TABLE_CHECK_492" check("UNITID" is not null)
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