--==============================================================================
--
-- Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
-- 
-- Created by: Saab AB
--
--==============================================================================

SET ECHO ON;
WHENEVER WARNING CONTINUE;


--==============================================================================
-- Create a log file and record the date/time.
--==============================================================================

LOG INPUT, OUTPUT ON 'vehicledb_install.log';
SET :start_time = LOCALTIMESTAMP(0);


--==============================================================================
-- Ensure that the default transaction modes are set.
--==============================================================================

ROLLBACK;
SET SESSION READ WRITE;
SET SESSION ISOLATION LEVEL REPEATABLE READ;
-- *****SET SESSION DIAGNOSTICS SIZE 50;
SET TRANSACTION START IMPLICIT;


--==============================================================================
-- Script should be read by the system administrator.
--==============================================================================

SET :user = SESSION_USER;


--==============================================================================
-- Remove any existing objects related to the database.
--==============================================================================

WHENEVER ERROR CONTINUE;
DROP IDENT vehicledbuser CASCADE;
WHENEVER ERROR EXIT, ROLLBACK;


--==============================================================================
-- Creating databanks, idents, and tables
-- Create owning ident for database objects.
--
-- Ident has the rights to create databanks and idents.
--==============================================================================

CREATE IDENT vehicledbuser AS USER USING 'vehicledbuser';

GRANT DATABANK TO vehicledbuser;
GRANT IDENT TO vehicledbuser WITH GRANT OPTION;

DISCONNECT;


--==============================================================================
-- Connect as the database owner.
--==============================================================================

CONNECT;
vehicledbuser
vehicledbuser


--==============================================================================
-- Create databanks to hold the tables.
--==============================================================================

CREATE DATABANK vehicledb_db OF 1000 PAGES IN 'vehicledb_db.dbf' WITH TRANS OPTION;


--==============================================================================
-- Create tables.
--==============================================================================
-- Some data types (see: Mimer SQL Engine Documentation)
--      Boolean <--> int(1)
--      Int32   <--> int     -2 147 483 648 through 2 147 483 647
--      Int64   <--> bigint  -9 223 372 036 854 775 808 through 9 223 372 036 854 775 807
--      Float32 <--> real    mantissa precision 7, exponent 10-38 to 10+38
--      Float64 <--> float   mantissa precision 16, exponent 10-308 to 10+308

CREATE TABLE tblVehicleCategory
    (VehicleCategory           int  NOT NULL,
     MaxSpeed                  real,
     IsDrivingLicenceRequired  int(1),
     Remark                    nvarchar(100),
     PRIMARY KEY (VehicleCategory) )
IN vehicledb_db;


--==============================================================================
-- Create stored procedures.
--==============================================================================

--------------------------------------------------------------------------------
-- Note that the stored procedure handles the difference between
-- Create (=INSERT) and Update (=UPDATE).
@
CREATE PROCEDURE spSetVehicleCategory
                    (parVehicleCategory           int,
                     parMaxSpeed                  real,
                     parIsDrivingLicenceRequired  int(1),
                     parRemark                    nvarchar(100))

    MODIFIES SQL DATA
BEGIN
    IF EXISTS ( SELECT * FROM tblVehicleCategory WHERE VehicleCategory = parVehicleCategory ) THEN
        UPDATE tblVehicleCategory
        SET    MaxSpeed                 = parMaxSpeed,
               IsDrivingLicenceRequired = parIsDrivingLicenceRequired,
               Remark                   = parRemark
        WHERE  VehicleCategory = parVehicleCategory;
    ELSE
        INSERT INTO tblVehicleCategory
        VALUES (parVehicleCategory,
                parMaxSpeed,
                parIsDrivingLicenceRequired,
                parRemark);
    END IF;
END
@

--------------------------------------------------------------------------------
@
CREATE PROCEDURE spGetVehicleCategory
                    (parVehicleCategory  int)
    VALUES ( real,     int(1),                   nvarchar(100) )
    AS     ( MaxSpeed, IsDrivingLicenceRequired, Remark        )

    READS SQL DATA
BEGIN
    DECLARE colMaxSpeed                  real;
    DECLARE colIsDrivingLicenceRequired  int(1);
    DECLARE colRemark                    nvarchar(100);

    DECLARE L CURSOR FOR
        SELECT MaxSpeed, IsDrivingLicenceRequired, Remark
        FROM   tblVehicleCategory
        WHERE  VehicleCategory = parVehicleCategory;

    DECLARE EXIT HANDLER FOR NOT FOUND
        BEGIN
        END;

    OPEN L;
    LOOP
        FETCH L INTO colMaxSpeed, colIsDrivingLicenceRequired, colRemark;
        RETURN     ( colMaxSpeed, colIsDrivingLicenceRequired, colRemark );
    END LOOP; 
    CLOSE L;

END
@

--------------------------------------------------------------------------------
@
CREATE PROCEDURE spDeleteVehicleCategory
                    (parVehicleCategory  int)

    MODIFIES SQL DATA
BEGIN
    IF parVehicleCategory IS NOT NULL THEN
        DELETE FROM tblVehicleCategory
        WHERE  VehicleCategory = parVehicleCategory;
    END IF;
END
@


--==============================================================================
-- Insert testdata.
--==============================================================================


--==============================================================================
-- Done. Close log.
--==============================================================================

SET :end_time = LOCALTIMESTAMP(0);
CLOSE LOG;
SET MESSAGE ON;

-- EXIT;
