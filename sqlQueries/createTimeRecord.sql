-- https://github.com/swlaschin/low-risk-ways-to-use-fsharp-at-work/blob/master/SqlInFsharp/CreateTables.sql
USE timeentryapp

/* =====================================================
Create Time Record table  
===================================================== */

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'TimeRecord' AND TABLE_SCHEMA='dbo')
   DROP TABLE dbo.TimeRecord
GO

CREATE TABLE TimeRecord (
    TimeRecordId INT NOT NULL IDENTITY(1,1),
    Site VARCHAR(4) NOT NULL,
    Shopfloor VARCHAR(4) NOT NULL,
    TimeType INT NOT NULL, 
    DurationMn INT NOT NULL,
    NbPeople INT NOT NULL

    CONSTRAINT PK_TimeRecord PRIMARY KEY CLUSTERED (TimeRecordId)
);

