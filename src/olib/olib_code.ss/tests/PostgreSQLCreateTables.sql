CREATE TABLE tblolibtest
(
  unitid integer NOT NULL,
  callsign character varying(6),
  combatreadiness integer,
  combatreadinessdescription character varying(100),
  unitsizeid character varying(50),
  unitidentityid character varying(50),
  latitude double precision,
  longitude double precision,
  speed real,
  course real,
  measurementtime timestamp(6) without time zone,
  isalive integer,
  alargeint bigint,
  CONSTRAINT tblolibtest_pkey PRIMARY KEY (unitid),
  CONSTRAINT tblolibtest_unitid_check CHECK (unitid IS NOT NULL)
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