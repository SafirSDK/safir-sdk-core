#==============================================================
#
# Copyright:      Saab AB 2010-2016
# Author:         Lars Hagström
#
# Purpose:        MYSQL DOPE database sql script.
#
#==============================================================


# Switch to mysql db, so that we can drop dope_db if it
# exists
use information_schema;

# Drop database and user, if they exist
DROP DATABASE IF EXISTS dope_db;
DROP USER IF EXISTS dopeuser;


# Create user and db
CREATE USER dopeuser IDENTIFIED BY 'dopeuser';
CREATE DATABASE dope_db;

# Switch to the new db
USE dope_db;


# Create table
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
) CHARACTER SET=utf8;

#give dopeuser permissions.
GRANT INSERT, UPDATE, SELECT, DELETE ON TABLE PersistentEntity TO dopeuser;

