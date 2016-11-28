namespace TimeEntry

module DBCommands =
    open System
    open TimeEntry.Result
    open TimeEntry.DomainTypes
    open TimeEntry.DataBase
    open FSharp.Data.SqlProvider

    //Connection string described here: https://www.connectionstrings.com/mysql/
    let [<Literal>] ConnectionString  = "Server=localhost;Port=3306;Database=timeentryapp;User=root;Password="

    //Path to mysql ODBC divers: http://fsprojects.github.io/SQLProvider/core/parameters.html

    let [<Literal>] ResolutionPath = __SOURCE_DIRECTORY__ + @"/../../packages/MySql.Data/lib/net45"

    type Sql = SqlDataProvider< 
                ConnectionString = ConnectionString,
                DatabaseVendor = Common.DatabaseProviderTypes.MYSQL,
                ResolutionPath = ResolutionPath,
                IndividualsAmount = 1000,
                UseOptionTypes = true,
                Owner = "timeentryapp" >

    (*
        WORKCENTER FUNCTIONS
    *)

    //Insert new workcenter in DB
    type InsertWorkCenter = WorkCenterInfo -> Result<unit>

    type UpdateWorkCenter = WorkCenterInfoId -> WorkCenterInfo -> Result<unit>

    type WorkCenterByCode = WorkCenter -> (WorkCenterInfoId * WorkCenterInfo)

    (*
        EVENT FUNCTIONS
    *)

    //Insert new event in DB
    type InsertEvent = Event -> unit



    //return id of record and record
    type GetTimeRecordsByWorkCenter = WorkCenter -> (TimeRecordId * TimeRecord) list
    
    //update one record 
    type UpdateTimeRecord = TimeRecordId -> TimeRecord -> Result<unit>



