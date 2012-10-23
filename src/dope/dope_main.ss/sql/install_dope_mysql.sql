#==============================================================
#
# Copyright:      Saab Systems 2010
# Author:         Mikael Wennerberg
#
# Purpose:        MYSQL DOPE database sql script.
# 
# Improvements:   None.
#
# History:        2010-01-12      /stmiwn/   First version.
#
#==============================================================




#==============================================================
# Create databanks to hold the tables.
#==============================================================

CREATE USER dopeuser IDENTIFIED BY 'dopeuser';

CREATE DATABASE dope_db;
USE dope_db;


GRANT ALL ON dope_db.* TO dopeuser;

set global max_allowed_packet=1000000000;


#==============================================================
# Create tables.
#==============================================================

CREATE table PersistentEntity
(
    TYPEID bigint  NOT NULL,
    INSTANCE bigint NOT NULL,
    HANDLERID bigint NULL,
    TYPENAME nvarchar(236) NULL,
    XMLDATA MEDIUMTEXT NULL,
    BINARYDATA MEDIUMBLOB NULL,
    BINARYSMALLDATA BLOB NULL,
    PRIMARY KEY(TYPEID, INSTANCE)
);

#==============================================================
# Create procedures.
#==============================================================
delimiter //


create procedure spInsertEntity(in TypeIdIn bigint,
                                in InstanceIdIn bigint,
                                in TypeNameIn nvarchar(236) )
modifies SQL data
begin
    if not exists(SELECT * from PersistentEntity where typeId=TypeIdIn AND instance=InstanceIdIn) then
        INSERT INTO PersistentEntity (typeid, instance, typename) values (TypeIdIn, InstanceIdIn, TypeNameIn);
    end if;
end//

delimiter ;

# EXIT
