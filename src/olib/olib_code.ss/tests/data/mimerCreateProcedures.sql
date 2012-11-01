@
create procedure spCreateOlibTest (pStringName nvarchar(10), 
                        pStringDescription nvarchar(50), 
                    pInt32 int,
                    pInt64 BIGINT,
                    pFloat32 Float(32),
                    pFloat64 DOUBLE PRECISION,
                    pBool int)
modifies SQL data
begin

declare pId int;

set pId = (select max(Id)+1 from tblOlibTest);
if pId is null then
    set pId =0;
end if;
insert into tblOlibTest(Id,
                        StringName, 
                    StringDescription, 
                    Int32, 
                    Int64, 
                    Float32, 
                    Float64,
                    Bool)
values (pId,
        pStringName, 
        pStringDescription, 
        pInt32, 
        pInt64, 
        pFloat32, 
        pFloat64,
        pBool);

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
create procedure spDeleteOlibTest(pId int)
modifies SQL data
begin

delete from tblOlibTest where Id = pId;

end
@

 

@
create procedure spInputOutputOlibTest(inout pInt32 float)
modifies SQL data
begin
declare Int32Sum float;
declare pId float;
set pId =  (select max(Id+1) from tblOlibTest);
set Int32sum = pInt32 + pInt32;

insert into tblOlibTest(Id, Int32  )
values ( pId, Int32sum);
        
select Int32 into pInt32 from tblOlibTest where Id = pId;
end
@

@
create procedure spOutputOlibTest (out pStringName nvarchar(10), 
                        out pStringDescription nvarchar(40), 
                    out pInt32 int, 
                    out pInt64 BIGINT, 
                    out pFloat32 float, 
                    out pFloat64 double precision, 
                    out pBool int)
modifies SQL data
begin

DECLARE L CURSOR FOR 
select StringName, 
     StringDescription, 
     Int32, 
     Int64, 
     Float32, 
     Float64, 
     Bool 
from tblOlibTest
where Id = 1;
 declare exit handler for not found 
    begin
      close L; 
    end;
 open L;
fetch L into pStringName,  pStringDescription, pInt32, pInt64, pFloat32,pFloat64,pBool;
end
@

@
create procedure spUpdateOlibTest ( pId int,
                          pStringName nvarchar(10), 
                         pStringDescription nvarchar(40), 
                     pInt32 int, 
                     pInt64 BIGINT, 
                     pFloat32 float, 
                     pFloat64 double precision, 
                     pBool int)
modifies SQL data
begin

update tblOlibTest
set StringName = pStringName,
    StringDescription = pStringDescription,
    Int32 = pInt32,
    Int64 = pInt64,
    Float32 = pFloat32,
    Float64 = pFloat64,
    Bool = pBool
    
where Id = pId
;

end
@