CREATE TABLE tblolibtest
(
  ID integer NOT NULL,
  STRINGNAME character varying(10),
  STRINGDESCRIPTION character varying(40),
  INT32 integer,
  INT64 BIGINT,
  FLOAT32 float,
  FLOAT64 double precision,
  BOOL integer,
  CONSTRAINT tblolibtest_pkey PRIMARY KEY (ID),
  CONSTRAINT tblolibtest_unitid_check CHECK (ID IS NOT NULL)
);

CREATE TABLE tblolibtestbinary
(
  id integer NOT NULL,
  data bytea,
  CONSTRAINT tblolibtestbinary_pkey PRIMARY KEY (id),
  CONSTRAINT tblolibtestbinary_id_check CHECK (id IS NOT NULL)
);

CREATE TABLE tblolibtestblob
(
  id integer NOT NULL,
  data bytea,
  CONSTRAINT tblolibtestblob_pkey PRIMARY KEY (id),
  CONSTRAINT tblolibtestblob_id_check CHECK (id IS NOT NULL)
);

CREATE TABLE tblolibtestnclob
(
  id integer NOT NULL,
  data text,
  CONSTRAINT tblolibtestnclob_pkey PRIMARY KEY (id),
  CONSTRAINT tblolibtestnclob_id_check CHECK (id IS NOT NULL)
);

CREATE TABLE tblperftest
(
  typeid bigint,
  instanceno integer,
  data character varying(2100)
);