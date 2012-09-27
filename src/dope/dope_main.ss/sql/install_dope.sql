--==============================================================
--
-- Copyright:      Saab Systems 2007
-- Author:         Jonas Thor
--
-- Purpose:        Mimer SQL DOPE database DDL script.
--                 Inspiration for this installscript is taken
--                 from Mimers example database.
-- 
-- Improvements:   None.
--
-- History:        2007-01-09      /stjth/   First version.
--                 2007-03-07      /stlrha/  Adapted to DOPE from the ALRT variant
--                 2010-01-19      /staeh/   Connect has to specify database, SafirDb.
--
--==============================================================

SET ECHO ON;

WHENEVER WARNING CONTINUE;

--==============================================================
-- Create a log file and record the date/time.
--==============================================================

LOG INPUT, OUTPUT ON 'dope_install.log';
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
-- Remove any existing objects related to the DOPE database.
--==============================================================

WHENEVER ERROR CONTINUE;
DROP IDENT dopeuser CASCADE;
WHENEVER ERROR EXIT, ROLLBACK;


--==============================================================
--#Creating databanks, idents, and tables
-- Create owning ident for DOPE database objects.
--
-- Ident has the rights to create databanks and idents.
--==============================================================

CREATE IDENT dopeuser AS USER USING 'dopeuser';

GRANT DATABANK TO dopeuser;
GRANT IDENT TO dopeuser WITH GRANT OPTION;

DISCONNECT;


--==============================================================
-- Connect as the DOPE database owner.
--==============================================================

CONNECT TO 'SafirDb' USER 'dopeuser' USING 'dopeuser';


--==============================================================
-- Create databanks to hold the tables.
--==============================================================

CREATE DATABANK dope_db OF 1000 PAGES IN 'dope_db.dbf' WITH TRANS OPTION;

--==============================================================
-- Create tables.
--==============================================================

CREATE table PersistentEntity
(
    TYPEID bigint  NOT NULL,
    INSTANCE bigint NOT NULL,
    HANDLERID bigint NULL,            -- This is set for existing entities. It doesn't uniquely identify the entity so its not part of the primarykey.
    TYPENAME nvarchar(236) NULL,
    XMLDATA NCLOB(10485760) NULL,
    BINARYDATA BLOB(10485760) NULL,
    PRIMARY KEY(TYPEID, INSTANCE)
);

--==============================================================
-- Create procedures.
--==============================================================

@
create procedure spInsertEntity(in TypeIdIn bigint,
                                in InstanceIdIn bigint,
                                in TypeNameIn nvarchar(236) )
modifies SQL data
begin
    If not exists(SELECT * from PersistentEntity where typeId=TypeIdIn AND instance=InstanceIdIn) then
        INSERT INTO PersistentEntity (typeid, instance, typename) 
            values (TypeIdIn, InstanceIdIn, TypeNameIn);
    end if;
end
@

--==============================================================
-- Done. Close log.
--==============================================================


SET :end_time = LOCALTIMESTAMP(0);

CLOSE LOG;

SET MESSAGE ON;

-- EXIT;

