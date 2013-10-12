
 --=   spcreateolibtest =--
CREATE OR REPLACE FUNCTION spcreateolibtest(character varying, 
                                            character varying, 
                                            integer, 
                                            bigint, 
                                            float, 
                                            double precision, 
                                            integer)  RETURNS void AS
$BODY$
DEClARE pId integer;
BEGIN
pId = (select  max(Id)+1 from tblOlibTest);
case 
    when pId is null then pId=0 ;
    else
end case;
 
insert into tblOlibTest( Id,                  
                    StringName, 
                    StringDescription, 
                    Int32,
                    Int64,                    
                    Float32, 
                    Float64, 
                    Bool)
values (    pId,
            $1, 
            $2,
            $3,
            $4,
            $5,
            $6,
            $7
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
                                            integer, 
                                            bigint, 
                                            float, 
                                            double precision, 
                                            integer) RETURNS void AS
$BODY$                    
begin
update tblOlibTest
set     StringName = $2,
        StringDescription = $3,
        Int32 = $4,
        Int64 = $5,
        Float32 = $6,
        Float64 = $7,
        Bool = $8
where Id = $1
;

end;
$BODY$
  LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION spdeleteolibtest(integer)
  RETURNS void AS
$BODY$                    
 begin
delete from tblOlibTest where Id = $1;
 end;
$BODY$
  LANGUAGE plpgsql;
 

 --=   spinputoutputolibtest =--
CREATE OR REPLACE FUNCTION spinputoutputolibtest(INOUT pInt32 double precision)
  RETURNS double precision AS
$BODY$  
declare Int32Sum float;
declare pId float;               
begin

pId =  (select max(Id)+1 from tblOlibTest);
Int32Sum = pInt32 + pInt32;

insert into tblOlibTest(Id, Int32  )
values ( pId, Int32sum);
        
pInt32 = (select Int32 from tblOlibTest where Id = pId);
end;
$BODY$
  LANGUAGE plpgsql;
  
--=   spoutputolibtest =--
CREATE OR REPLACE FUNCTION spoutputolibtest(OUT pStringName character varying, 
                                            OUT pStringDescription character varying, 
                                            out pInt32 int, 
                                            out pInt64 BIGINT, 
                                            out pFloat32 float, 
                                            out pFloat64 double precision, 
                                            out pBool int)
  RETURNS record AS
$BODY$                    
 begin

pStringName = (select StringName from tblOlibTest where Id = 1);
pStringDescription = (select StringDescription from tblOlibTest where Id = 1);
pInt32 = (select Int32 from tblOlibTest where Id = 1);
pInt64 = (select Int64 from tblOlibTest where Id = 1);
pFloat32 = (select Float32 from tblOlibTest where Id = 1);
pFloat64 = (select Float64 from tblOlibTest where Id = 1);
pBool = (select Bool from tblOlibTest where Id = 1);
 
 end;
$BODY$
  LANGUAGE plpgsql;
