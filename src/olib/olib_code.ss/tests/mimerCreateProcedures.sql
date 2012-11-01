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
if pUnitId is null then
    set pUnitId =0;
end if;
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
CREATE PROCEDURE "OLIBTESTERUSER"."ROWCOUNT" 
(OUT "P1" BIGINT) 
LANGUAGE SQL 
 
NOT DETERMINISTIC 
READS SQL DATA 
BEGIN 
   DECLARE L CURSOR FOR 
    SELECT  COUNT (*)
        FROM    TBLOLIBTEST;
open L;
fetch L into P1;
 
END
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

@create procedure spInputOutputOlibTest(inout pSpeed real)
modifies SQL data
begin
declare SpeedSum real;
declare pUnitId real;
set pUnitId =  (select max(UnitId)+1 from tblOlibTest);
set SpeedSum = pSpeed + pSpeed;

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
        null, 
        null, 
        null, 
        null, 
        null, 
        null, 
        null, 
        SpeedSum, 
        null, 
        null, 
        null,
        null);
        
select Speed into pSpeed from tblOlibTest where UnitId = pUnitId;
end
@

@create procedure spOutputOlibTest (out pCallSign nvarchar(6), 
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
    IsAlive = pIsAlive,
    ALargeInt = pALargeInt
where UnitId = pUnitId
;

end
@