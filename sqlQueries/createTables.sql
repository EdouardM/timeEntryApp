-- https://github.com/swlaschin/low-risk-ways-to-use-fsharp-at-work/blob/master/SqlInFsharp/CreateTables.sql
USE timeentryapp;


/* =====================================================
Create Work Centers table  
===================================================== */

DROP TABLE  IF EXISTS WorkCenter;

CREATE TABLE WorkCenter (
    WorkCenterId INT NOT NULL auto_increment,
    Site VARCHAR(4) NOT NULL,
    WorkCenter VARCHAR(4) NOT NULL,
    Shopfloor VARCHAR(4) NOT NULL,
    StartTime INT NOT NULL, 
    EndTime INT NOT NULL,
    NbPeople INT NOT NULL,
    Active BIT(1),
    PRIMARY KEY (WorkCenterId)
);


/* =====================================================
Create Work Orders Entry table  
===================================================== */

DROP TABLE  IF EXISTS WorkOrderEntry;

CREATE TABLE WorkOrderEntry (
    WorkOrderId INT NOT NULL auto_increment,
    WorkCenterId INT,
    ItemCode VARCHAR(6) NOT NULL,
    Weight FLOAT(4,4),
    Unit VARCHAR(2),
    WorkOrderStatus ENUM('open','closed'),
    TotalLabourTimeHr INT(4),
    Active BIT(1),
    UserName VARCHAR(10) NOT NULL,
    LastUpdate TIMESTAMP,
    PRIMARY KEY (WorkOrderId)
);

CREATE INDEX WorkCenterId ON WorkOrderEntry (WorkCenterId);


/* =====================================================
Create Event table  
===================================================== */

DROP TABLE  IF EXISTS Event;

CREATE TABLE Event (
    EventId INT NOT NULL auto_increment,
    Event VARCHAR(4) NOT NULL,
    HasInfo INT(1) NOT NULL,
    AllowZeroPerson INT(1) NOT NULL,
    Active BIT(1),
    PRIMARY KEY (EventId)
);


/* =====================================================
Create Event Entry table  
===================================================== */

DROP TABLE  IF EXISTS EventEntry;

CREATE TABLE EventEntry (
    EventEntryId INT NOT NULL auto_increment,
    EventId INT NOT NULL,
    Machine VARCHAR(10),
    Cause VARCHAR(50),
    Solution VARCHAR(50),
    Comments VARCHAR(200),
    PRIMARY KEY (EventEntryId)
);

-- Look for Foreign Key: check for existence of event when inserting event entry
CREATE INDEX EventId ON EventEntry (EventId);

/* =====================================================
Create Time Record table  
===================================================== */

DROP TABLE  IF EXISTS timerecord;

CREATE TABLE timerecord (
    TimeRecordId INT NOT NULL auto_increment,
    Site VARCHAR(4) NOT NULL,
    Shopfloor VARCHAR(4) NOT NULL,
    TimeType INT NOT NULL, 
    DurationMn INT NOT NULL,
    NbPeople INT NOT NULL,
    PRIMARY KEY (TimeRecordId)
);

