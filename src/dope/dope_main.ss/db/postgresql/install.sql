--==============================================================
--
-- Copyright:      Saab AB 2016
-- Author:         Lars Hagström
--
-- Purpose:        PostgreSQL DOPE database sql script.
--
--==============================================================


-- Switch to postgres db, so that we can drop dope_db if it
-- exists
\c postgres

-- Drop database and user, if they exist
DROP DATABASE IF EXISTS dope_db;
DROP USER IF EXISTS dopeuser;


-- Create user and db
CREATE USER dopeuser WITH PASSWORD 'dopeuser';
CREATE DATABASE dope_db;

-- Switch to the new db
\c dope_db

-- Create table
CREATE table PersistentEntity
(
    TYPEID bigint  NOT NULL,
    INSTANCE bigint NOT NULL,
    HANDLERID bigint NULL,
    TYPENAME varchar NULL,
    XMLDATA varchar NULL,
    BINARYDATA bytea NULL,
    BINARYSMALLDATA bytea NULL,
    PRIMARY KEY(TYPEID, INSTANCE)
);


--give dopeuser permissions.
GRANT INSERT, UPDATE, SELECT, DELETE ON TABLE PersistentEntity TO dopeuser;
