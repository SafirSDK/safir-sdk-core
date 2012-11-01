delimiter $$

CREATE DEFINER=root@localhost PROCEDURE ROWCOUNT(OUT P1 BIGINT)
BEGIN
 Declare result BIGINT;
select count(*) into result from tblOlibTest ;

set P1=result;
 
END
$$


delimiter $$
CREATE DEFINER=root@localhost PROCEDURE spCreateOlibTest(   pCallSign nvarchar(6), 
                                                            pUnitSizeId nvarchar(50), 
                                                            pUnitIdentityId nvarchar(50), 
                                                            pCombatReadiness int, 
                                                            pCombatReadinessDescription nvarchar(100), 
                                                            pLatitude double, 
                                                            pLongitude double, 
                                                            pSpeed real, 
                                                            pCourse real, 
                                                            pMeasurementTime timestamp, 
                                                            pIsAlive int, 
                                                            pALargeInt bigint)
    MODIFIES SQL DATA
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
values (pUnitId,
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

end$$

delimiter $$
CREATE DEFINER=root@localhost PROCEDURE spDeleteOlibTest(pUnitId int)
    MODIFIES SQL DATA
begin

delete from tblOlibTest where UnitId = pUnitId;
-- delete from tblOlibTestnclob where Id = pUnitId;

end
$$



delimiter $$

CREATE DEFINER=root@localhost PROCEDURE spInputOutputOlibTest(inout pSpeed float)
BEGIN
declare SpeedSum float;
declare pUnitId float;
declare result float;
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
values (pUnitId,
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
 
END
$$

delimiter $$
CREATE DEFINER=root@localhost PROCEDURE spOutputOlibTest(   out pCallSign nvarchar(6), 
                                                            out pUnitSizeId nvarchar(50), 
                                                            out pUnitIdentityId nvarchar(50), 
                                                            out pCombatReadiness int, 
                                                            out pCombatReadinessDescription nvarchar(100), 
                                                            out pLatitude double, 
                                                            out pLongitude double, 
                                                            out pSpeed real, 
                                                            out pCourse real, 
                                                            out pMeasurementTime timestamp, 
                                                            out pIsAlive int, 
                                                            out pALargeInt bigint)
MODIFIES SQL DATA
begin
select  CallSign, 
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
from    tblOlibTest where   UnitId = 1 
into    pCallSign, pUnitSizeId, pUnitIdentityId, pCombatReadiness, 
        pCombatReadinessDescription, pLatitude, pLongitude, pSpeed, pCourse, 
        pMeasurementTime, pIsAlive, pALargeInt;

end
$$


delimiter $$

CREATE DEFINER=root@localhost PROCEDURE spUpdateOlibTest(   pUnitId int,
                                                            pCallSign nvarchar(6), 
                                                            pUnitSizeId nvarchar(50), 
                                                            pUnitIdentityId nvarchar(50), 
                                                            pCombatReadiness int, 
                                                            pCombatReadinessDescription nvarchar(100), 
                                                            pLatitude double, 
                                                            pLongitude double, 
                                                            pSpeed real, 
                                                            pCourse real, 
                                                            pMeasurementTime timestamp, 
                                                            pIsAlive int, 
                                                            pALargeInt bigint)
MODIFIES SQL DATA
begin
update  tblOlibTest
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
where   UnitId = pUnitId
;

end
$$