-- https://github.com/swlaschin/low-risk-ways-to-use-fsharp-at-work/blob/master/SqlInFsharp/CreateTables.sql
USE timeentryapp;

/* =====================================================
Create Site table  
===================================================== */

DROP TABLE  IF EXISTS Site;

CREATE TABLE Site (
    Site VARCHAR(4) NOT NULL,
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
    Site VARCHAR(4) NOT NULL,
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
    WorkCenter VARCHAR(4) NOT NULL,
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
    WorkCenter VARCHAR(4) NOT NULL,
    Machine  VARCHAR(10) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (Machine)
);

CREATE INDEX WorkCenterId ON Machine (WorkCenter);

INSERT Machine VALUES('F1', 'Rooslvo', 1);


/* =====================================================
Create Work Orders Entry table  
===================================================== */

DROP TABLE  IF EXISTS WorkOrderEntry;

CREATE TABLE WorkOrderEntry (
    WorkOrder VARCHAR(10) NOT NULL,
    WorkCenter VARCHAR(4) NOT NULL,
    ItemCode VARCHAR(6) NOT NULL,
    WorkOrderStatus ENUM('open','closed') NOT NULL,
    TotalMachineTimeHr FLOAT(7,4) NOT NULL,
    TotalLabourTimeHr FLOAT(7,4) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (WorkOrder)
);

CREATE INDEX WorkCenterId ON WorkOrderEntry (WorkCenter);

/* =====================================================
Create Event table  
===================================================== */

DROP TABLE  IF EXISTS Event;    

CREATE TABLE Event (
    Event VARCHAR(4) NOT NULL,
    HasInfo TINYINT(1) NOT NULL,
    AllowZeroPerson TINYINT(1) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (Event)
);

INSERT Event VALUES('FOR', 0, 0, 1)
INSERT Event VALUES('DIV', 0, 0, 1)
INSERT Event VALUES('PAN', 1, 0, 1)
INSERT Event VALUES('ARR', 1, 0, 1)
 

/* =====================================================
Create Event Entry table  
===================================================== */

DROP TABLE  IF EXISTS EventEntry;

CREATE TABLE EventEntry (
    EventEntryId INT NOT NULL auto_increment,
    Event VARCHAR(4) NOT NULL,
    Machine VARCHAR(10),
    Cause VARCHAR(50),
    Solution VARCHAR(50),
    Comments VARCHAR(200),
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (EventEntryId)
);

CREATE INDEX EventId ON EventEntry (Event);

/* =====================================================
Create Time Record table  
===================================================== */

DROP TABLE  IF EXISTS TimeRecord;

CREATE TABLE TimeRecord (
    TimeRecordId INT NOT NULL auto_increment,
    Site VARCHAR(4) NOT NULL,
    Shopfloor VARCHAR(4) NOT NULL,
    TimeType ENUM('machine', 'labour') NOT NULL,
    StartTime TIMESTAMP NOT NULL DEFAULT '2016-01-01 00:00:01',
    EndTime TIMESTAMP NOT NULL DEFAULT '2016-01-01 00:00:01', 
    DurationHr FLOAT(4,4) NOT NULL, 
    NbPeople FLOAT(2,1) NOT NULL,
    WorkOrder VARCHAR(10),
    EventEntryId INT,
    Allocation ENUM('workorder','event') NOT NULL,
    RecordStatus ENUM('entered', 'validated') NOT NULL,
    Active TINYINT(1) NOT NULL,
    UserId INT NOT NULL,
    LastUpdate TIMESTAMP NOT NULL DEFAULT '2016-01-01 00:00:01',
    PRIMARY KEY (TimeRecordId)
);

CREATE INDEX SiteId ON TimeRecord (Site);
CREATE INDEX ShopfloorId ON TimeRecord (Shopfloor);
CREATE INDEX UserId ON TimeRecord (UserId);
CREATE INDEX WorkOrderEntryId ON TimeRecord (WorkOrderEntryId);
CREATE INDEX EventEntryId ON TimeRecord (EventEntryId);