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

type GetWorkCenterCodes = unit -> string list

let getWorkCenterCodes () =
    query {
        for workcenter in ctx.Timeentryapp.Workcenter do
            select workcenter.WorkCenter
    }
    |> Seq.toArray


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


type GetWorkCenterById = WorkCenterId -> WorkCenterInfo

let getWorkCenterById id = 
    query {
        for workcenter in ctx.Timeentryapp.Workcenter do
            where (workcenter.WorkCenterId = id)
            select workcenter
    }
    |> Seq.head

type GetWorkCenterByCode = WorkCenter -> WorkCenterInfo

let getWorkCenterByCode code =
    query {
        for workcenter in ctx.Timeentryapp.Workcenter do
            where (workcenter.WorkCenter = code)
            select workcenter
    }
    |> Seq.head


type UpdateWorkCenter = WorkCenterId -> WorkCenterInfo -> Result<unit>
let updateWorkCenter workcenterId workcenterinfo = 
    let wc = getWorkCenterById workcenterId
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
    let wc = getWorkCenterById workcenterId
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

let getEventById id = 
    query {
        for event in ctx.Timeentryapp.Event do
            where (event.EventId = id)
            select event
    }
    |> Seq.head

type GetEventByCode = Event -> EventEntry
let getEventByCode code = 
    query {
        for event in ctx.Timeentryapp.Event do
            where (event.Event = code)
            select event
    }
    |> Seq.head

type UpdateEvent = EventId -> Event -> Result<unit>
let updateEvent eventId event = 
    let ev = getEventById eventId
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


(* WorkOrderEntry Functions *)

//Insert new workcenter in DB
type InsertWorkOrder = WorkOrderEntry -> Result<unit>
let insertWorkOrderEntry workOrderEntry  = 
    let wo = ctx.Timeentryapp.Workorderentry.Create()
    let dbwo = toDBWorkOrderEntry workOrderEntry
    let workcenter = getWorkCenterByCode dbwo.WorkCenter

    wo.WorkOrder    <- dbwo.WorkOrder
    wo.ItemCode  <- dbwo.ItemCode
    wo.WorkCenterId <- workcenter.WorkCenterId
    wo.WorkOrderStatus <- dbwo.WorkOrderStatus
    wo.TotalMachineTimeHr <- (float32 0.)
    wo.TotalLabourTimeHr <- (float32 0.)
    wo.Active <- 1y
    
    try 
        ctx.SubmitUpdates()
        |> Success
    with
    | ex -> Failure <| sprintf "%s" ex.Message


type GetWorkOrderList = unit -> string list
let getWorkOrderCodes () =
    query {
        for workOrderEntry in ctx.Timeentryapp.Workorderentry do
            select workOrderEntry.WorkOrder
    }
    |> Seq.toArray

type GetWorkOrderById = WorkOrderEntryId -> WorkOrderEntry

let getWorkOrderById id = 
    query {
        for workOrderEntry in ctx.Timeentryapp.Workorderentry do
            where (workOrderEntry.WorkOrderEntryId = id)
            select workOrderEntry
    }
    |> Seq.head

(* EventEntry Functions *)

//Insert new workcenter in DB
type InsertEventEntry = EventEntry -> Result<unit>
let insertEventEntry eventEntry  = 
    let ev = ctx.Timeentryapp.Evententry.Create()
    let dbev = toDBEventEntry eventEntry
    let event = getEventByCode (dbev.Event.Event)

    ev.EventId  <- event.EventId
    ev.Machine  <- dbev.Machine
    ev.Cause    <- dbev.Cause
    ev.Solution <- dbev.Solution
    ev.Comments <- dbev.Comments
    ev.Active <- 1y
    
    try 
        ctx.SubmitUpdates()
        |> Success
    with
    | ex -> Failure <| sprintf "%s" ex.Message

let getEventEntryById id = 
    query {
        for eventEntry in ctx.Timeentryapp.Evententry do
            where (eventEntry.EventEntryId = id)
            select eventEntry
    }
    |> Seq.head


let updateEventEntry eventId eventEntry = 
    let ev = getEventEntryById eventId
    let dbev = toDBEventEntry eventEntry
    let event = getEventByCode (dbev.Event.Event)

    ev.EventId  <- event.EventId
    ev.Machine  <- dbev.Machine
    ev.Cause    <- dbev.Cause
    ev.Solution <- dbev.Solution
    ev.Comments <- dbev.Comments
    ev.Active   <- 1y
    
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

(*
let q = query {
        from event in ctx.Event 
        join eventEntry in ctx.EventEntry
        on (event.Id = eventEntry.EventId)
        select event, eventEntry
}
*)