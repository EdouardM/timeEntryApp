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
    StartTime INT NOT NULL, 
    EndTime INT NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (WorkCenterId)
);


/* =====================================================
Create Work Orders Entry table  
===================================================== */

DROP TABLE  IF EXISTS WorkOrderEntry;

CREATE TABLE WorkOrderEntry (
    WorkOrderId INT NOT NULL auto_increment,
    WorkOrder VACHAR(10) NOT NULL,
    WorkCenterId INT,
    ItemCode VARCHAR(6) NOT NULL,
    WorkOrderStatus ENUM('open','closed'),
    TotalMachineTimeHr FLOAT(4,4) NOT NULL,
    TotalLabourTimeHr FLOAT(4,4) NOT NULL,
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (WorkOrderId)
);

CREATE INDEX WorkCenterId ON WorkOrderEntry (WorkCenterId);
CREATE INDEX UserId ON WorkOrderEntry (UserId);


/* =====================================================
Create Event table  
===================================================== */

DROP TABLE  IF EXISTS Event;

CREATE TABLE Event (
    EventId INT NOT NULL auto_increment,
    Event VARCHAR(4) NOT NULL,
    HasInfo INT(1) NOT NULL,
    AllowZeroPerson INT(1) NOT NULL,
    Active TINYINT(1) NOT NULL,
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
    Active TINYINT(1) NOT NULL,
    PRIMARY KEY (WorkOrderId)

    PRIMARY KEY (EventEntryId)
);

-- Look for Foreign Key: check for existence of event when inserting event entry
CREATE INDEX EventId ON EventEntry (EventId);
CREATE INDEX UserId ON EventEntry (UserId);

/* =====================================================
Create Time Record table  
===================================================== */

DROP TABLE  IF EXISTS timerecord;

CREATE TABLE Timerecord (
    TimeRecordId INT NOT NULL auto_increment,
    Site VARCHAR(4) NOT NULL,
    Shopfloor VARCHAR(4) NOT NULL,
    TimeType ENUM('machine', 'labour') NOT NULL, 
    DurationHr FLOAT(4,4) NOT NULL,
    --Only one decimal because nbpeople can be multiple of 0.5 only 
    NbPeople FLOAT(2,1) NOT NULL,
    WorkOrderId INT,
    EnventEntryId INT,
    Allocation ENUM('workorder','event') NOT NULL,
    Active TINYINT(1) NOT NULL,
    UserId INT NOT NULL,
    LastUpdate TIMESTAMP NOT NULL,
    PRIMARY KEY (TimeRecordId)
);
