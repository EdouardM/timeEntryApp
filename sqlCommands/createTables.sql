-- https://github.com/fsprojects/SQLProvider/tree/master/src/DatabaseScripts/MySql

-- https://github.com/swlaschin/low-risk-ways-to-use-fsharp-at-work/blob/master/SqlInFsharp/CreateTables.sql
USE timeentryapp;

/* =====================================================
Create Site table  
===================================================== */

DROP TABLE  IF EXISTS Site;

CREATE TABLE Site (
    Site VARCHAR(3) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (Site)
);

INSERT Site VALUES('F21', 1);
INSERT Site VALUES('F22', 1);

/* =====================================================
Create Shopfloor table  
===================================================== */

CREATE TABLE Shopfloor (
    Shopfloor VARCHAR(5) NOT NULL,
    Site VARCHAR(3) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (Shopfloor)
);

CREATE INDEX SiteId ON Shopfloor (Site);

INSERT Shopfloor VALUES('F211A', 'F21', 1);
INSERT Shopfloor VALUES('F221A', 'F22', 1);

/* =====================================================
Create Work Centers table  
===================================================== */

DROP TABLE  IF EXISTS WorkCenter;

CREATE TABLE WorkCenter (
    Shopfloor VARCHAR(5) NOT NULL,
    WorkCenter VARCHAR(5) NOT NULL,
    StartHour INT NOT NULL, 
    EndHour INT NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (WorkCenter)
);

CREATE INDEX ShopfloorId ON WorkCenter (Shopfloor);

INSERT WorkCenter VALUES('F211A', 'F1', '4', '4', 1);
INSERT WorkCenter VALUES('F211A', 'F2', '4', '4', 1);

/* =====================================================
Create Machine table
======================================================*/
DROP TABLE IF EXISTS Machine;

CREATE TABLE Machine (
    Shopfloor VARCHAR(5) NOT NULL,
    Machine  VARCHAR(10) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (Machine)
);

CREATE INDEX ShopfloorId ON Machine (Shopfloor);

INSERT Machine VALUES('F221A', 'Rooslvo', 1);


/* =====================================================
Create Work Orders Info table  
===================================================== */

DROP TABLE  IF EXISTS WorkOrderInfo;

CREATE TABLE WorkOrderInfo (
    WorkOrder VARCHAR(10) NOT NULL,
    WorkCenter VARCHAR(4) NOT NULL,
    ItemCode VARCHAR(6) NOT NULL,
    WorkOrderStatus ENUM('open','closed') NOT NULL,
    TotalMachineTimeHr FLOAT(7,4) NOT NULL,
    TotalLabourTimeHr FLOAT(7,4) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (WorkOrder)
);

CREATE INDEX WorkCenterId ON WorkOrderInfo (WorkCenter);

/* =====================================================
Create Activity table  
===================================================== */

DROP TABLE  IF EXISTS Activity;    

CREATE TABLE Activity (
    Code VARCHAR(4) NOT NULL,
    Site VARCHAR(3) NOT NULL,
    RecordLevel ENUM('workcenter', 'shopfloor') NOT NULL,
    AccessAll TINYINT(1) NOT NULL,
    ExtraInfo ENUM('withinfo', 'withoutinfo') NOT NULL,
    TimeType ENUM('machine', 'labour') NOT NULL,
    isLinked TINYINT(1) NOT NULL, 
    LinkedActivity VARCHAR(4), 
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (Activity)
);

INSERT Activity VALUES('FOR', 'F21', 'workcenter', 1, 'withoutinfo', 'machine', 1, 'MFOR', 1)
INSERT Activity VALUES('MFOR','F21', 'workcenter', 1, 'withinfo', 'labour', 1, 'FOR', 1)
INSERT Activity VALUES('DIV', 'F21', 'workcenter', 1, 'withoutinfo', 'machine', 1, 'MDIV', 1)
INSERT Activity VALUES('MDIV', 'F21', 'workcenter', 1, 'withinfo', 'labour', 1, 'DIV', 1)
INSERT Activity VALUES('PAN', 'F21', 'workcenter', 1, 'withinfo', 'machine', 1, 'MPAN', 1)
INSERT Activity VALUES('MPAN', 'F21', 'workcenter', 1, 'withinfo', 'labour', 1, 'PAN', 1)
INSERT Activity VALUES('ARR', 'F21', 'workcenter', 1, 'withinfo', 'machine', 1, 'MARR', 1)
INSERT Activity VALUES('MARR', 'F21', 'workcenter', 1, 'withinfo', 'labour', 1, 'ARR', 1)


/* =====================================================
Create Activity WorkCenter Access table  
===================================================== */

DROP TABLE  IF EXISTS ActivityWorkCenterAccess;

CREATE TABLE ActivityWorkCenterAccess (
    Activity VARCHAR(4) NOT NULL,
    WorkCenter VARCHAR(4) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (Activity, WorkCenter)
);

/* =====================================================
Create Activity ShopFloor Access table  
===================================================== */

DROP TABLE  IF EXISTS ActivityShopFloorAccess;

CREATE TABLE ActivityShopFloorAccess (
    Activity VARCHAR(4) NOT NULL,
    ShopFloor VARCHAR(5) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (Activity, ShopFloor)
);

/* =====================================================
Create Activity Info table  
===================================================== */

DROP TABLE  IF EXISTS ActivityInfo;

CREATE TABLE ActivityInfo (
    ActivityInfoId INT NOT NULL auto_increment,
    Activity VARCHAR(4) NOT NULL,
    Machine VARCHAR(10),
    Cause VARCHAR(50),
    Solution VARCHAR(50),
    Comments VARCHAR(200),
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (ActivityInfoId)
);

CREATE INDEX ActivityId ON ActivityInfo (Activity);

/* =====================================================
Create Time Record table  
===================================================== */

DROP TABLE  IF EXISTS TimeRecord;

CREATE TABLE TimeRecord (
    TimeRecordId INT NOT NULL auto_increment,
    Site VARCHAR(4) NOT NULL,
    Shopfloor VARCHAR(5) NOT NULL,
    WorkCenter VARCHAR(4),
    TimeType ENUM('machine', 'labour') NOT NULL,
    StartTime TIMESTAMP NOT NULL DEFAULT '2016-01-01 00:00:01',
    EndTime TIMESTAMP NOT NULL DEFAULT '2016-01-01 00:00:01', 
    TimeHr FLOAT(7,4) NOT NULL, 
    NbPeople FLOAT(3,1) NOT NULL,
    Attribution ENUM('workorder','event') NOT NULL,
    WorkOrder VARCHAR(10),
    ActivityInfoId INT,
    RecordStatus ENUM('entered', 'validated') NOT NULL,
    Active TINYINT(1) NOT NULL,
    Login VARCHAR(8) NOT NULL,
    LastUpdate TIMESTAMP NOT NULL DEFAULT '2016-01-01 00:00:01',
    PRIMARY KEY (TimeRecordId)
);

CREATE INDEX SiteId ON TimeRecord (Site);
CREATE INDEX ShopfloorId ON TimeRecord (Shopfloor);
CREATE INDEX Login ON TimeRecord (Login);
CREATE INDEX WorkOrderEntryId ON TimeRecord (WorkOrder);
CREATE INDEX ActivityInfoId ON TimeRecord (ActivityInfoId);


/* =====================================================
Create User table  
===================================================== */

DROP TABLE  IF EXISTS User;

CREATE TABLE User (
    Login VARCHAR(8),
    UserRealName VARCHAR(50) NOT NULL,
    Password VARCHAR(50) NOT NULL,
    AllSites TINYINT(1) NOT NULL,
    AuthLevel ENUM('viewer', 'user', 'keyuser', 'admin') NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (Login)
);

INSERT User VALUES('moureed1', 'Edouard', 'indaclub', 0, 'admin', 1);

/* =====================================================
Create User Site Authorization table  
===================================================== */

DROP TABLE  IF EXISTS UserAuthorization;

CREATE TABLE UserAuthorization (
    Login VARCHAR(8) NOT NULL,
    Site VARCHAR(3) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (Login, Site)
);

INSERT User VALUES('moureed1', 'F21', 1);
INSERT User VALUES('moureed1', 'F22', 1);