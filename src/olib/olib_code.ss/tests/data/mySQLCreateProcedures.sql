delimiter $$

CREATE DEFINER=root@localhost PROCEDURE ROWCOUNT(OUT P1 BIGINT)
BEGIN
 Declare result BIGINT;
select count(*) into result from tblOlibTest ;

set P1=result;
 
END
$$


delimiter $$
CREATE DEFINER=root@localhost PROCEDURE spCreateOlibTest(   pStringName varchar(10), 
                                                            pStringDescription varchar(40), 
                                                            pInt32 Int, 
                                                            pInt64 BIGINT, 
                                                            pFloat32 Float,
                                                            pFloat64 double, 
                                                            pBool Int)
    MODIFIES SQL DATA
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

end$$

delimiter $$
CREATE DEFINER=root@localhost PROCEDURE spDeleteOlibTest(pId int)
    MODIFIES SQL DATA
begin

delete from tblOlibTest where Id = pId;
-- delete from tblOlibTestnclob where Id = pUnitId;

end
$$



delimiter $$

CREATE DEFINER=root@localhost PROCEDURE spInputOutputOlibTest(inout pInt32 float)
BEGIN
declare Int32Sum float;
declare pId float;
declare result float;
set pId =  (select max(Id)+1 from tblOlibTest);
set Int32Sum = pInt32 + pInt32;

insert into tblOlibTest(Id, Int32  )
values ( pId, Int32sum);
 
select Int32 into pInt32 from tblOlibTest where Id = pId;        
 
 
END
$$

delimiter $$
CREATE DEFINER=root@localhost PROCEDURE spOutputOlibTest(   out pStringName varchar(10), 
                                                            out pStringDescription varchar(40), 
                                                            out pInt32 int, 
                                                            out pInt64 BIGINT, 
                                                            out pFloat32 float, 
                                                            out pFloat64 double precision, 
                                                            out pBool int)
MODIFIES SQL DATA
begin
select StringName, 
     StringDescription, 
     Int32, 
     Int64, 
     Float32, 
     Float64, 
     Bool 
from tblOlibTest where Id = 1
into    pStringName, pStringDescription, pInt32, pInt64, pFloat32,pFloat64,pBool;

end
$$


delimiter $$

CREATE DEFINER=root@localhost PROCEDURE spUpdateOlibTest(   pId int,
                                                            pStringName varchar(10), 
                                                            pStringDescription varchar(40), 
                                                            pInt32 int, 
                                                            pInt64 BIGINT, 
                                                            pFloat32 float, 
                                                            pFloat64 double precision, 
                                                            pBool int)
MODIFIES SQL DATA
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
$$