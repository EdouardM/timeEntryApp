// reference the type provider dll
#r "../../packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll"

#load "./Helpers.fs" 
#load "./DomainTypes.fs"
#load "./Constructors.fs"
#load "./Database.fs"

open FSharp.Data.Sql
open TimeEntry.Result
open TimeEntry.Conversions
open TimeEntry.DomainTypes
open TimeEntry.Constructors
open TimeEntry.DataBase


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

let ctx = Sql.GetDataContext()

(* WORKCENTER FUNCTIONS  *)

//Insert new workcenter in DB
type InsertWorkCenter = WorkCenterInfo -> Result<unit>

let insertWorkCenter workcenterInfo = 
    let wc = ctx.Timeentryapp.Workcenter.Create()
    let dbWc = toDBWorkCenterInfo workcenterInfo
    wc.Site <- dbWc.Site
    wc.Shopfloor <- dbWc.ShopFloor
    wc.WorkCenter <- dbWc.WorkCenter
    wc.StartHour <- dbWc.StartHour
    wc.EndHour <- dbWc.EndHour
    wc.Active <- 1y
    
    try 
        ctx.SubmitUpdates()
        |> Success
    with
    | ex -> Failure <| sprintf "%s" ex.Message

type GetWorkCenterCodes = unit -> string list

let getWorkCenterCodes () =
    query {
        for workcenter in ctx.Timeentryapp.Workcenter do
            select workcenter.WorkCenter
    }
    |> Seq.toArray

type GetWorkCenterById = WorkCenterId -> WorkCenterInfo

let getWorkCenter id = 
    query {
        for workcenter in ctx.Timeentryapp.Workcenter do
            where (workcenter.WorkCenterId = id)
            select workcenter
    }
    |> Seq.head


type UpdateWorkCenter = WorkCenterId -> WorkCenterInfo -> Result<unit>
let updateWorkCenter workcenterId workcenterinfo = 
    let wc = getWorkCenter workcenterId
    let dbWc = toDBWorkCenterInfo workcenterinfo
    wc.Site <- dbWc.Site
    wc.Shopfloor <- dbWc.ShopFloor
    wc.WorkCenter <- dbWc.WorkCenter
    wc.StartHour <- dbWc.StartHour
    wc.EndHour <- dbWc.EndHour
    
    try 
        ctx.SubmitUpdates()
        |> Success
    with
    | ex -> Failure <| sprintf "%s" ex.Message

type DeleteWorkCenter = WorkCenterId -> Result<unit>

let deleteWorkCenter workcenterId  = 
    let wc = getWorkCenter workcenterId
    wc.Active <- 0y
    
    try 
        ctx.SubmitUpdates()
        |> Success
    with
    | ex -> Failure <| sprintf "%s" ex.Message

(* EVENT FUNCTIONS  *)

//Insert new workcenter in DB
type InsertEvent = Event -> Result<unit>

let insertEvent event = 
    let ev = ctx.Timeentryapp.Event.Create()
    let dbev = toDBEvent event
    ev.Event    <- dbev.Event
    ev.HasInfo  <- boolToSbyte dbev.HasInfo
    ev.AllowZeroPerson <- boolToSbyte dbev.AllowZeroPerson
    ev.Active <- 1y
    
    try 
        ctx.SubmitUpdates()
        |> Success
    with
    | ex -> Failure <| sprintf "%s" ex.Message

type GetEventList = unit -> string list
let getEventCodes () =
    query {
        for event in ctx.Timeentryapp.Event do
            select event.Event
    }
    |> Seq.toArray

type GetEventById = EventId -> WorkCenterInfo

let getEvent id = 
    query {
        for event in ctx.Timeentryapp.Event do
            where (event.EventId = id)
            select event
    }
    |> Seq.head

type UpdateEvent = EventId -> Event -> Result<unit>
let updateEvent eventId event = 
    let ev = getEvent eventId
    let dbEv = toDBEvent event
    ev.Event    <- dbEv.Event
    ev.HasInfo  <- boolToSbyte dbEv.HasInfo
    ev.AllowZeroPerson <- boolToSbyte dbEv.AllowZeroPerson
    ev.Active <- 1y
    
    try 
        ctx.SubmitUpdates()
        |> Success
    with
    | ex -> Failure <| sprintf "%s" ex.Message


(*
    query {
    for student in db.Student do
    join selection in db.CourseSelection
        on (student.StudentID = selection.StudentID)
    select (student, selection)
}
*)

let wo = ctx.Timeentryapp.Workorderentry.Create()
(*
let q = query {
        from event in ctx.Event 
        join eventEntry in ctx.EventEntry
        on (event.Id = eventEntry.EventId)
        select event, eventEntry
}
*)