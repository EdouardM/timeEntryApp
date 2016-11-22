-- https://github.com/swlaschin/low-risk-ways-to-use-fsharp-at-work/blob/master/SqlInFsharp/CreateTables.sql
USE timeentryapp;


/* =====================================================
Create Work Centers table  
===================================================== */

DROP TABLE  IF EXISTS workcenter;

CREATE TABLE workcenter (
    WorkCenterId INT NOT NULL auto_increment,
    Site VARCHAR(4) NOT NULL,
    WorkCenter VARCHAR(4) NOT NULL,
    Shopfloor VARCHAR(4) NOT NULL,
    StartTime INT NOT NULL, 
    EndTime INT NOT NULL,
    NbPeople INT NOT NULL,
    PRIMARY KEY (WorkCenterId)
);



/* =====================================================
Create Work Orders table  
===================================================== */

DROP TABLE  IF EXISTS workcenter;

CREATE TABLE workcenter (
    WorkOrderId INT NOT NULL auto_increment,
    ItemCode VARCHAR(6) NOT NULL,
    Weight FLOAT(4,4),
    Unit VARCHAR(2),
    WorkOrderStatus ENUM('open','closed'),
    PRIMARY KEY (WorkOrderId)
);

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

