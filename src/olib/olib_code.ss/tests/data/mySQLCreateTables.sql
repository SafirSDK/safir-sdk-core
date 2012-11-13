CREATE USER olibuser IDENTIFIED BY 'olibuser';
GRANT ALL ON SafirDb.* TO olibuser;

drop table tblOlibTest;
drop table tblOlibTestBinary;
drop table tblOlibTestBlob;
drop table tblOlibTestNClob;
drop table tblPerfTest;

CREATE TABLE tblOlibTest (
  ID int(11) NOT NULL,
  STRINGNAME nvarchar(10) DEFAULT NULL,
  STRINGDESCRIPTION nvarchar(40) DEFAULT NULL,
  INT32 INTEGER,
  INT64 BIGINT,
  FLOAT32 FLOAT,
  FLOAT64 double,
  BOOL Int,
  PRIMARY KEY (`ID`)
);

CREATE TABLE tblOlibTestBinary (
  ID int(11) NOT NULL,
  DATA binary(255) DEFAULT NULL,
  PRIMARY KEY (ID)
); 

CREATE TABLE tblOlibTestBlob (
  ID int(11) NOT NULL,
  DATA mediumblob,
  PRIMARY KEY (ID)
);

CREATE TABLE tblOlibTestNClob (
  ID int(11) NOT NULL,
  DATA longtext,
  PRIMARY KEY (ID)
);

CREATE TABLE tblPerfTest (
  TypeID bigint(20) DEFAULT NULL,
  InstanceNo int(11) DEFAULT NULL,
  data nvarchar(2100) DEFAULT NULL
);
