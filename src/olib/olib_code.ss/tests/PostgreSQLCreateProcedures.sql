
 --=   spcreateolibtest =--
CREATE OR REPLACE FUNCTION spcreateolibtest(character varying, 
                                            character varying, 
                                            character varying, 
                                            integer, 
                                            character varying, 
                                            double precision, 
                                            double precision, 
                                            double precision, 
                                            double precision, 
                                            timestamp without time zone, 
                                            integer, 
                                            bigint)  RETURNS void AS
$BODY$
DEClARE pUnitId integer;
BEGIN
pUnitId = (select  max(UnitId)+1 from tblOlibTest);
case 
    when pUnitId is null then pUnitId=0 ;
    else
end case;
 
insert into tblOlibTest( UnitID,                  
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
          )
values (    pUnitId,
            $1, 
            $2,
            $3,
            $4,
            $5,
            $6,
            $7,
            $8,
            $9,
            $10,
            $11,
            $12
 );
 END;
$BODY$
LANGUAGE plpgsql;


 --=   rowcount =--
CREATE OR REPLACE FUNCTION rowcount(OUT p1 bigint)
RETURNS bigint AS
$BODY$                    
 begin

P1 = (select count(*) from tblOlibTest);

 end;
$BODY$
LANGUAGE plpgsql;


 --=   spupdateolibtest =--
CREATE OR REPLACE FUNCTION spupdateolibtest(integer, 
                                            character varying, 
                                            character varying, 
                                            character varying, 
                                            integer, 
                                            character varying, 
                                            double precision, 
                                            double precision, 
                                            double precision, 
                                            double precision, 
                                            timestamp without time zone, 
                                            integer, 
                                            bigint) RETURNS void AS
$BODY$                    
begin
update tblOlibTest
set     CallSign = $2,
        UnitSizeId = $3,
        UnitIdentityId = $4,
        CombatReadiness = $5,
        CombatReadinessDescription = $6,
        Latitude = $7,
        Longitude = $8,
        Speed = $9,
        Course = $10,
        MeasurementTime = $11,
        IsAlive = $12,
        ALargeInt = $13
where UnitId = $1
;

end;
$BODY$
  LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION spdeleteolibtest(integer)
  RETURNS void AS
$BODY$                    
 begin
delete from tblOlibTest where UnitId = $1;
 end;
$BODY$
  LANGUAGE plpgsql;
 

 --=   spinputoutputolibtest =--
CREATE OR REPLACE FUNCTION spinputoutputolibtest(INOUT pspeed double precision)
  RETURNS double precision AS
$BODY$  
declare SpeedSum float;
declare pUnitId float;               
begin

pUnitId =  (select max(UnitId)+1 from tblOlibTest);
SpeedSum = pSpeed + pSpeed;

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
        
pSpeed = (select Speed from tblOlibTest where UnitId = pUnitId);
end;
$BODY$
  LANGUAGE plpgsql;
  
--=   spoutputolibtest =--
CREATE OR REPLACE FUNCTION spoutputolibtest(OUT pcallsign character varying, 
                                            OUT punitsizeid character varying, 
                                            OUT punitidentityid character varying, 
                                            OUT pcombatreadiness integer, 
                                            OUT pcombatreadinessdescription character varying, 
                                            OUT platitude double precision, 
                                            OUT plongitude double precision, 
                                            OUT pspeed real, 
                                            OUT pcourse real, 
                                            OUT pmeasurementtime timestamp without time zone, 
                                            OUT pisalive integer, 
                                            OUT palargeint bigint)
  RETURNS record AS
$BODY$                    
 begin

pCallSign = (select CallSign from tblOlibTest where UnitId = 1);
pUnitSizeId = (select UnitSizeId from tblOlibTest where UnitId = 1);
pUnitIdentityId = (select UnitIdentityId from tblOlibTest where UnitId = 1);
pCombatReadiness = (select CombatReadiness from tblOlibTest where UnitId = 1);
pCombatReadinessDescription = (select CombatReadinessDescription from tblOlibTest where UnitId = 1);
pLatitude = (select Latitude from tblOlibTest where UnitId = 1);
pLongitude = (select Longitude from tblOlibTest where UnitId = 1);
pSpeed = (select Speed from tblOlibTest where UnitId = 1);
pCourse = (select Course from tblOlibTest where UnitId = 1);
pMeasurementTime = (select MeasurementTime from tblOlibTest where UnitId = 1);
pIsAlive = (select IsAlive from tblOlibTest where UnitId = 1);
pALargeInt = (select ALargeInt from tblOlibTest where UnitId = 1);
 end;
$BODY$
  LANGUAGE plpgsql;
