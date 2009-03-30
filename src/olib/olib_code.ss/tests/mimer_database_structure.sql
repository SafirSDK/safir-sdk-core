--==============================================================
--
-- Copyright:      Saab Systems 2008
-- Author:         Jörgen Johansson
--
-- Purpose:        Mimer SQL ALRT database DDL script.
--                 Inspiration for this installscript is taken
--                 from Mimers example database.
-- 
-- Improvements:   None.
--
-- History:        2008-03-17      /stjrjo/   First version.
--
--==============================================================

SET ECHO ON;

WHENEVER WARNING CONTINUE;

--==============================================================
-- Create a log file and record the date/time.
--==============================================================

LOG INPUT, OUTPUT ON 'olib_tester_install.log';
SET :start_time = LOCALTIMESTAMP(0);


--==============================================================
-- Ensure that the default transaction modes are set.
--==============================================================

ROLLBACK;
SET SESSION READ WRITE;
SET SESSION ISOLATION LEVEL REPEATABLE READ;
-- *****SET SESSION DIAGNOSTICS SIZE 50;
SET TRANSACTION START IMPLICIT;


--==============================================================
-- Script should be read by the System Administrator (SYSADM).
--==============================================================

SET :user = SESSION_USER;


--==============================================================
-- Remove any existing objects related to the ALRT database.
--==============================================================

WHENEVER ERROR CONTINUE;
DROP IDENT olibtesteruser CASCADE;
WHENEVER ERROR EXIT, ROLLBACK;


--==============================================================
--#Creating databanks, idents, and tables
-- Create owning ident for ALRT database objects.
--
-- Ident has the rights to create databanks and idents.
--==============================================================

CREATE IDENT olibtesteruser AS USER USING 'olibtesteruser';

GRANT DATABANK TO olibtesteruser;
GRANT IDENT TO olibtesteruser WITH GRANT OPTION;

DISCONNECT;


--==============================================================
-- Connect as the OlibTester database owner.
--==============================================================

CONNECT;
olibtesteruser
olibtesteruser


--==============================================================
-- Create databanks to hold the tables.
--==============================================================

CREATE DATABANK olib_tester_db OF 1000 PAGES IN 'olib_tester_db.dbf' WITH LOG OPTION;

--==============================================================
-- Create tables.
--==============================================================
create table TBLOLIBTEST(
UNITID INTEGER,
CALLSIGN NATIONAL CHARACTER VARYING(6) collate UCS_BASIC,
COMBATREADINESS INTEGER,
COMBATREADINESSDESCRIPTION NATIONAL CHARACTER VARYING(100) collate UCS_BASIC,
UNITSIZEID NATIONAL CHARACTER VARYING(50) collate UCS_BASIC,
UNITIDENTITYID NATIONAL CHARACTER VARYING(50) collate UCS_BASIC,
LATITUDE FLOAT ,
LONGITUDE FLOAT ,
SPEED REAL,
COURSE REAL,
MEASUREMENTTIME timestamp(8),
ISALIVE INTEGER,
ALARGEINT BIGINT,
primary key(UNITID),
CHECK ("UNITID" is not null)
) in olib_tester_db;

create table TBLOLIBTESTBinary(
ID INTEGER,
DATA Binary(10000),
primary key(ID),
CHECK ("ID" is not null)
) in olib_tester_db;

create table TBLOLIBTESTNCLOB(
ID INTEGER,
DATA NATIONAL CHAR LARGE OBJECT(10M) collate UCS_BASIC,
primary key(ID),
CHECK ("ID" is not null)
) in olib_tester_db;

create table tblOlibTestBlob(
ID INTEGER,
DATA blob,
primary key(ID),
CHECK ("ID" is not null)
) in olib_tester_db;

--==============================================================
-- Create procedures.
--==============================================================
@
create procedure spCreateOlibTest (pCallSign nvarchar(6), 
                        pUnitSizeId nvarchar(50), 
                    pUnitIdentityId nvarchar(50), 
                    pCombatReadiness int, 
                    pCombatReadinessDescription nvarchar(100), 
                    pLatitude float, 
                    pLongitude float, 
                    pSpeed real, 
                    pCourse real, 
                    pMeasurementTime timestamp(6), 
                    pIsAlive int, 
                    pALargeInt bigint)
modifies SQL data
begin

declare pUnitId int;

set pUnitId = (select max(UnitId)+1 from tblOlibTest);

insert into tblOlibTest(UnitId,
                        CallSign, 
                    UnitSizeId, 
                    UnitIdentityId, 
                    CombatReadiness, 
                    CombatReadinessDescription, 
                    Latitude, 
                    Longitude, 
                    Speed, 
                    Course, 
                    MeasurementTime, 
                    IsAlive,
                    ALargeInt  )
values (    pUnitId,
            pCallSign, 
        pUnitSizeId, 
        pUnitIdentityId, 
        pCombatReadiness, 
        pCombatReadinessDescription, 
        pLatitude, 
        pLongitude, 
        pSpeed, 
        pCourse, 
        pMeasurementTime, 
        pIsAlive,
        pALargeInt);

end
@

@
create procedure spDeleteOlibTest(pUnitId int)
modifies SQL data
begin

delete from tblOlibTest where UnitId = pUnitId;

delete from tblOlibTestnclob where Id = pUnitId;

end
@

@
create procedure spGetAllOlibTests ()
values (int, nvarchar(6), nvarchar(50), nvarchar(50), int, nvarchar(100), 
        float, float, real, real, timestamp(8), int, bigint) as
       (UnitId, CallSign, UnitSizeId, UnitIdentityId, CombatReadiness, 
        CombatReadinessDescription, Latitude, Longitude, Speed, Course,
        MeasurementTime, IsAlive, ALargeInt )
reads SQL data
begin
declare pUnitId int;
declare pCallSign nvarchar(6);
declare pUnitSizeId nvarchar(50);
declare pUnitIdentityId nvarchar(50);
declare pCombatReadiness int;
declare pCombatReadinessDescription nvarchar(100);
declare pLatitude float;
declare pLongitude float;
declare pSpeed real;
declare pCourse real;
declare pMeasurementTime timestamp(8);
declare pIsAlive int;
declare pALargeInt bigint;
DECLARE L CURSOR FOR 
select UnitId, 
         CallSign, 
     UnitSizeId, 
     UnitIdentityId, 
     CombatReadiness, 
     CombatReadinessDescription, 
     Latitude, 
     Longitude, 
     Speed, 
     Course, 
     MeasurementTime, 
     IsAlive, 
     ALargeInt
from tblOlibTest;
 declare exit handler for not found 
    begin
      close L; 
    end;
 open L;
 loop 
   fetch L into pUnitId, pCallSign, pUnitSizeId, pUnitIdentityId, pCombatReadiness, 
                pCombatReadinessDescription, pLatitude, pLongitude, pSpeed, pCourse, 
                pMeasurementTime, pIsAlive, pALargeInt;
   return (pUnitId, pCallSign, pUnitSizeId, pUnitIdentityId, pCombatReadiness, 
           pCombatReadinessDescription, pLatitude, pLongitude, pSpeed, pCourse,
           pMeasurementTime, pIsAlive, pALargeInt);
 end loop;
end
@

@
create procedure spUpdateOlibTest (pUnitId int,
                        pCallSign nvarchar(6), 
                        pUnitSizeId nvarchar(50), 
                    pUnitIdentityId nvarchar(50), 
                    pCombatReadiness int, 
                    pCombatReadinessDescription nvarchar(100), 
                    pLatitude float, 
                    pLongitude float, 
                    pSpeed real, 
                    pCourse real, 
                    pMeasurementTime timestamp(8), 
                    pIsAlive int, 
                    pALargeInt bigint)
modifies SQL data
begin

update tblOlibTest
set     CallSign = pCallSign,
    UnitSizeId = pUnitSizeId,
    UnitIdentityId = pUnitIdentityId,
    CombatReadiness = pCombatReadiness,
    CombatReadinessDescription = pCombatReadinessDescription,
    Latitude = pLatitude,
    Longitude = pLongitude,
    Speed = pSpeed,
    Course = pCourse,
    MeasurementTime = pMeasurementTime,
    IsAlive = pIsAlive
where UnitId = pUnitId
;

end
@

@
create procedure spInputOutputOlibTest (inout pCallSign nvarchar(6), 
                        inout pUnitSizeId nvarchar(50), 
                    inout pUnitIdentityId nvarchar(50), 
                    inout pCombatReadiness int, 
                    inout pCombatReadinessDescription nvarchar(100), 
                    inout pLatitude float, 
                    inout pLongitude float, 
                    inout pSpeed real, 
                    inout pCourse real, 
                    inout pMeasurementTime timestamp(0), 
                    inout pIsAlive int, 
                    inout pALargeInt bigint)
modifies SQL data
begin
DECLARE L CURSOR FOR 
select CallSign, 
     UnitSizeId, 
     UnitIdentityId, 
     CombatReadiness, 
     CombatReadinessDescription, 
     Latitude, 
     Longitude, 
     Speed, 
     Course, 
     MeasurementTime, 
     IsAlive, 
     ALargeInt
from tblOlibTest
where UnitId = 1;
 declare exit handler for not found 
    begin
      close L; 
    end;

declare pUnitId int;
set pUnitId = (select max(UnitId)+1 from tblOlibTest);
insert into tblOlibTest(UnitId,
                        CallSign, 
                    UnitSizeId, 
                    UnitIdentityId, 
                    CombatReadiness, 
                    CombatReadinessDescription, 
                    Latitude, 
                    Longitude, 
                    Speed, 
                    Course, 
                    MeasurementTime, 
                    IsAlive,
                    ALargeInt  )
values (    pUnitId,
            pCallSign, 
        pUnitSizeId, 
        pUnitIdentityId, 
        pCombatReadiness, 
        pCombatReadinessDescription, 
        pLatitude, 
        pLongitude, 
        pSpeed, 
        pCourse, 
        pMeasurementTime, 
        pIsAlive,
        pALargeInt);

open L;
fetch L into pCallSign, pUnitSizeId, pUnitIdentityId, pCombatReadiness, 
                pCombatReadinessDescription, pLatitude, pLongitude, pSpeed, pCourse, 
                pMeasurementTime, pIsAlive, pALargeInt;

end
@

@
create procedure spOutputOlibTest (out pCallSign nvarchar(6), 
                        out pUnitSizeId nvarchar(50), 
                    out pUnitIdentityId nvarchar(50), 
                    out pCombatReadiness int, 
                    out pCombatReadinessDescription nvarchar(100), 
                    out pLatitude float, 
                    out pLongitude float, 
                    out pSpeed real, 
                    out pCourse real, 
                    out pMeasurementTime timestamp(0), 
                    out pIsAlive int, 
                    out pALargeInt bigint)
modifies SQL data
begin

DECLARE L CURSOR FOR 
select CallSign, 
     UnitSizeId, 
     UnitIdentityId, 
     CombatReadiness, 
     CombatReadinessDescription, 
     Latitude, 
     Longitude, 
     Speed, 
     Course, 
     MeasurementTime, 
     IsAlive, 
     ALargeInt
from tblOlibTest
where UnitId = 1;
 declare exit handler for not found 
    begin
      close L; 
    end;
 open L;
fetch L into pCallSign, pUnitSizeId, pUnitIdentityId, pCombatReadiness, 
                pCombatReadinessDescription, pLatitude, pLongitude, pSpeed, pCourse, 
                pMeasurementTime, pIsAlive, pALargeInt;

end
@

COMMIT;


--==============================================================
-- Done. Close log.
--==============================================================


SET :end_time = LOCALTIMESTAMP(0);

CLOSE LOG;

SET MESSAGE ON;

-- EXIT;
