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
    Shopfloor VARCHAR(5) NOT NULL,
    StartHour INT NOT NULL, 
    EndHour INT NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (WorkCenterId)
);

CREATE UNIQUE INDEX WorkCenter ON WorkCenter (Site, Shopfloor, WorkCenter);

/* =====================================================
Create Work Orders Entry table  
===================================================== */

DROP TABLE  IF EXISTS WorkOrderEntry;

CREATE TABLE WorkOrderEntry (
    WorkOrderEntryId INT NOT NULL auto_increment,
    WorkOrder VARCHAR(10) NOT NULL,
    WorkCenterId INT,
    ItemCode VARCHAR(6) NOT NULL,
    WorkOrderStatus ENUM('open','closed'),
    TotalMachineTimeHr FLOAT(4,4) NOT NULL,
    TotalLabourTimeHr FLOAT(4,4) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (WorkOrderEntryId)
);

CREATE INDEX WorkCenterId ON WorkOrderEntry (WorkCenterId);
CREATE UNIQUE INDEX WorkOrder ON WorkOrderEntry (WorkOrder);


/* =====================================================
Create Event table  
===================================================== */

DROP TABLE  IF EXISTS Event;

CREATE TABLE Event (
    EventId INT NOT NULL auto_increment,
    Event VARCHAR(4) NOT NULL,
    HasInfo TINYINT(1) NOT NULL,
    AllowZeroPerson TINYINT(1) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (EventId)
);

CREATE UNIQUE INDEX Event ON Event (Event);


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
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (EventEntryId)
);

-- Look for Foreign Key: check for existence of event when inserting event entry
CREATE INDEX EventId ON EventEntry (EventId);

/* =====================================================
Create Time Record table  
===================================================== */

DROP TABLE  IF EXISTS TimeRecord;

CREATE TABLE TimeRecord (
    TimeRecordId INT NOT NULL auto_increment,
    Site VARCHAR(4) NOT NULL,
    Shopfloor VARCHAR(4) NOT NULL,
    TimeType ENUM('machine', 'labour') NOT NULL,
    StartTime TIMESTAMP NOT NULL,
    EndTime TIMESTAMP NOT NULL, 
    DurationHr FLOAT(4,4) NOT NULL, 
    NbPeople FLOAT(2,1) NOT NULL,
    WorkOrderEntryId INT,
    EventEntryId INT,
    Allocation ENUM('workorder','event') NOT NULL,
    RecordStatus ENUM('entered', 'validated') NOT NULL,
    Active TINYINT(1) NOT NULL,
    UserId INT NOT NULL,
    LastUpdate TIMESTAMP NOT NULL,
    PRIMARY KEY (TimeRecordId)
);

CREATE INDEX UserId ON TimeRecord (UserId);
CREATE INDEX WorkOrderEntryId ON TimeRecord (WorkOrderEntryId);
CREATE INDEX EventEntryId ON TimeRecord (EventEntryId);