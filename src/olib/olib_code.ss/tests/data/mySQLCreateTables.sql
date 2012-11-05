 

CREATE TABLE tblolibtest (
  ID int(11) NOT NULL,
  STRINGNAME varchar(10) DEFAULT NULL,
  STRINGDESCRIPTION varchar(40) DEFAULT NULL,
  INT32 INTEGER,
  INT64 BIGINT,
  FLOAT32 FLOAT,
  FLOAT64 double,
  BOOL Int,
  PRIMARY KEY (`ID`)
);

CREATE TABLE tblolibtestbinary (
  ID int(11) NOT NULL,
  DATA binary(255) DEFAULT NULL,
  PRIMARY KEY (ID)
); 

CREATE TABLE tblolibtestblob (
  ID int(11) NOT NULL,
  DATA mediumblob,
  PRIMARY KEY (ID)
);

CREATE TABLE tblolibtestnclob (
  ID int(11) NOT NULL,
  DATA longtext,
  PRIMARY KEY (ID)
);

CREATE TABLE tblperftest (
  TypeID bigint(20) DEFAULT NULL,
  InstanceNo int(11) DEFAULT NULL,
  data varchar(2100) DEFAULT NULL
);
 