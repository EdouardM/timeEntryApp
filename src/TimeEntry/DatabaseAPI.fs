namespace TimeEntry

module DBCommands =
    open System
    open TimeEntry.Result
    open TimeEntry.DomainTypes
    open TimeEntry.DataBase
    
    (*
        EVENT FUNCTIONS
    *)

    //Insert new event in DB
    type InsertEvent = Event -> unit



    //return id of record and record
    type GetTimeRecordsByWorkCenter = WorkCenter -> (TimeRecordId * TimeRecord) list
    
    //update one record 
    type UpdateTimeRecord = TimeRecordId -> TimeRecord -> Result<unit>



