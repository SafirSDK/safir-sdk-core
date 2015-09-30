#==============================================================
#
# Copyright:      Saab AB 2010-2015
# Author:         Mikael Wennerberg
#
# Purpose:        MYSQL DOPE database sql script.
#
#==============================================================


#==============================================================
# Create databanks to hold the tables.
#==============================================================

CREATE USER dopeuser IDENTIFIED BY 'dopeuser';

CREATE DATABASE dope_db;
USE dope_db;

GRANT ALL ON dope_db.* TO dopeuser;

#==============================================================
# Create tables.
#==============================================================

CREATE table PersistentEntity
(
    TYPEID bigint  NOT NULL,
    INSTANCE bigint NOT NULL,
    HANDLERID bigint NULL,
    XMLDATA MEDIUMTEXT NULL,
    BINARYDATA MEDIUMBLOB NULL,
    BINARYSMALLDATA BLOB NULL,
    PRIMARY KEY(TYPEID, INSTANCE)
) CHARACTER SET=utf8;


# EXIT
