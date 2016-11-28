namespace TimeEntry

module DBCommands =
    open System
    open TimeEntry.Result
    open TimeEntry.ConstrainedString
    open TimeEntry.DomainTypes
    open TimeEntry.DataBase

    //Insert new event in DB
    type InsertEvent = Event -> unit

    //Insert new workcenter in DB
    type InsertWorkCenter = WorkCenter -> unit



    //return id of record and record
    type GetTimeRecordsByWorkCenter = WorkCenter -> (TimeRecordId * TimeRecord) list
    
    //update one record 
    type UpdateTimeRecord = TimeRecordId -> TimeRecord -> Result<unit>



