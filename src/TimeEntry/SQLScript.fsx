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

type Activation = 
    | Activate
    | Desactivate
    with
        member x.State =
            match x with 
                | Activate -> "active"
                | Desactivate -> "inactive"

let flags = 
        function
        | Activate -> 0y, 1y
        | Desactivate -> 1y, 0y

(* SITE FUNCTIONS *)
type GetSiteCodes = unit -> string list

///Returns a list of codes of active sites
let getSiteCodes: GetSiteCodes =
    fun () ->
        let ctx = Sql.GetDataContext()
        query {
            for site in ctx.Timeentryapp.Site do
                where (site.Active = 1y)
                select site.Site
        }
        |> Seq.toList

getSiteCodes()

type InsertSite = string -> Result<unit>

///Inserts a new site
let insertSite: InsertSite =
    fun site ->
        let ctx = Sql.GetDataContext()
        let dbs = ctx.Timeentryapp.Site.Create()
        dbs.Site <- site
        dbs.Active <- 1y
        
        try
            ctx.SubmitUpdates()
            |> Success
        with 
            ex -> Failure ex.Message

insertSite ("F23")
getSiteCodes()

type ToggleSite = Activation -> Site -> Result<unit>
let toggleSite: ToggleSite =
    fun activation site -> 
        let (Site s) = site
        let ctx = Sql.GetDataContext()
        let current, future = flags activation
        let dbsOpt = 
            query {
                for site in ctx.Timeentryapp.Site do
                    where (site.Site = s && site.Active = current)
                    select site
            }
            |> Seq.tryHead
        match dbsOpt with
            | Some dbs -> 
                dbs.Active <- future
                try 
                    ctx.SubmitUpdates()
                    |> Success
                with
                    ex -> Failure ex.Message
            | None -> Failure <| sprintf "Site \'%s\' is missing or %s." s activation.State

///Desactivates a site if active
type DeleteSite = Site -> Result<unit>
let deleteSite: DeleteSite  = toggleSite Desactivate

type ActivateSite = Site -> Result<unit>
let activateSite: ActivateSite = toggleSite Activate


deleteSite (Site "F23")
activateSite(Site "F23")
getSiteCodes()


(* SHOPFLOOR FUNCTIONS *)
type GetShopfloorCodes = unit -> string list

///Returns a list of codes of active shopfloors
let getShopFloorCodes: GetShopfloorCodes =
    fun () ->
        let ctx = Sql.GetDataContext()
        query {
            for shopfloor in ctx.Timeentryapp.Shopfloor do
                where (shopfloor.Active = 1y)
                select shopfloor.Shopfloor
        }
        |> Seq.toList

getShopFloorCodes()

type GetShopFloorInfo = ShopFloor -> Result<DBShopFloorInfo>

let getShopFloorInfo: GetShopFloorInfo =
    fun shopfloor ->
        let (ShopFloor sf) = shopfloor
        let ctx = Sql.GetDataContext()
        query {
            for shopfloor in ctx.Timeentryapp.Shopfloor do
                where (shopfloor.Active = 1y && shopfloor.Shopfloor = sf)
                select shopfloor
        }
        |> Seq.map(fun dbsf -> 
            {
                DBShopFloorInfo.Site = dbsf.Site
                ShopFloor = dbsf.Shopfloor
            })
        |> Seq.toList
        |> (function 
        | [] -> Failure <| sprintf "Shopfloor not found for: %s" sf
        | [x] -> Success x
        | x::xs -> Failure <| sprintf "More than one shopfloor found for: %s" sf)

getShopFloorInfo(ShopFloor "F211A")


type InsertShopfloor = ShopFloorInfo -> Result<unit>

///Inserts a new shopfloor
let insertShopfloor: InsertShopfloor =
    fun shopfloorinfo ->
        let sf = toDBShopfloorInfo shopfloorinfo 
        let ctx = Sql.GetDataContext()
        let dbsf = ctx.Timeentryapp.Shopfloor.Create()
        dbsf.Site       <- sf.Site 
        dbsf.Shopfloor  <- sf.ShopFloor
        dbsf.Active     <- 1y
        
        try
            ctx.SubmitUpdates()
            |> Success
        with 
            ex -> Failure ex.Message

insertShopfloor ({Site = Site "F23"; ShopFloor = ShopFloor "F231A"})
getShopFloorCodes()


type ToggleShopfloor = Activation -> ShopFloor -> Result<unit>

let toggleShopfloor: ToggleShopfloor =
    fun activation shopfloor -> 
        let (ShopFloor s) = shopfloor
        let ctx = Sql.GetDataContext()
        let current, future = flags activation
        let dbsOpt = 
            query {
                for shopfloor in ctx.Timeentryapp.Shopfloor do
                    where (shopfloor.Shopfloor = s && shopfloor.Active = current)
                    select shopfloor
            }
            |> Seq.tryHead

        match dbsOpt with
            | Some dbs -> 
                dbs.Active <- future
                try 
                    ctx.SubmitUpdates()
                    |> Success
                with
                    ex -> Failure ex.Message
            | None -> Failure <| sprintf "Shopfloor \'%s\' is missing or %s." s activation.State

///Desactivates a site if active
type DeleteShopfloor = ShopFloor -> Result<unit>
let deleteShopfloor: DeleteShopfloor  = toggleShopfloor Desactivate

type ActivateShopfloor = ShopFloor -> Result<unit>
let activateShopfloor: ActivateShopfloor = toggleShopfloor Activate


deleteShopfloor(ShopFloor "F231A")
activateShopfloor(ShopFloor "F231A")

getShopFloorCodes()

(* WORKCENTER FUNCTIONS  *)

type GetWorkCenterCodes = unit -> string list
let getWorkCenterCodes: GetWorkCenterCodes =
    fun () -> 
        let ctx = Sql.GetDataContext()
        query {
            for workcenter in ctx.Timeentryapp.Workcenter do
                where (workcenter.Active = 1y)
                select workcenter.WorkCenter
        }
        |> Seq.toList

getWorkCenterCodes()


type GetWorkCenter = WorkCenter -> Result<DBWorkCenterInfo>

let getWorkCenter: GetWorkCenter =
    fun workcenter -> 
        let (WorkCenter wc) = workcenter
        let ctx = Sql.GetDataContext()
        query {
            for workcenter in ctx.Timeentryapp.Workcenter do
                join shopfloor in ctx.Timeentryapp.Shopfloor on (workcenter.Shopfloor = shopfloor.Shopfloor)
                where (workcenter.WorkCenter = wc)
                select (workcenter, shopfloor)
        }
        |> Seq.map (fun (wc, sf) -> 
            {
                        DBWorkCenterInfo.WorkCenter   = wc.WorkCenter
                        ShopFloorInfo    = { Site = sf.Site; ShopFloor = sf.Shopfloor}
                        StartHour    = wc.StartHour
                        EndHour      = wc.EndHour
            }
        )
        |> Seq.toList
        |> (function 
                | [] -> Failure <| sprintf "Workcenter not found for: %s" wc
                | [x] -> Success x
                | x::xs -> Failure <| sprintf "More than one workcenter found for: %s" wc)


getWorkCenter (WorkCenter "F1")

//Insert new workcenter in DB
type InsertWorkCenter = WorkCenterInfo -> Result<unit>

let insertWorkCenter: InsertWorkCenter =
    fun workcenterInfo -> 
        let ctx = Sql.GetDataContext()
        let wc = ctx.Timeentryapp.Workcenter.Create()
        let dbWc = toDBWorkCenterInfo workcenterInfo
        wc.Shopfloor    <- dbWc.ShopFloorInfo.ShopFloor
        wc.WorkCenter   <- dbWc.WorkCenter
        wc.StartHour    <- dbWc.StartHour
        wc.EndHour      <- dbWc.EndHour
        wc.Active       <- 1y
        
        try 
            ctx.SubmitUpdates()
            |> Success
        with
        | ex -> Failure <| sprintf "%s" ex.Message

let sf:ShopFloorInfo = {Site = Site "F21"; ShopFloor = ShopFloor "F211A"}
let wc: WorkCenterInfo = { ShopFloorInfo = sf ; WorkCenter = WorkCenter "LB3"; StartHour = Hour 4u; EndHour = Hour 23u }

insertWorkCenter wc
getWorkCenterCodes()

type UpdateWorkCenter = WorkCenter -> WorkCenterInfo -> Result<unit>
let updateWorkCenter: UpdateWorkCenter =
    fun workcenter workcenterinfo ->
        let (WorkCenter wc) = workcenter  
        let ctx = Sql.GetDataContext()
        let ctx = Sql.GetDataContext()
        let wcinfoRes = 
            query {
                for workcenter in ctx.Timeentryapp.Workcenter do
                    where (workcenter.WorkCenter = wc && workcenter.Active = 1y)
                    select workcenter
            }
            |> Seq.toList
            |> (function 
                    | [] -> Failure <| sprintf "Workcenter not found for: %s" wc
                    | [w] -> Success w
                    | x::xs -> Failure <| sprintf "More than one workcenter found for: %s" wc)


        let dbWc = toDBWorkCenterInfo workcenterinfo
        match wcinfoRes with
            | Success wcinfo -> 
                wcinfo.Shopfloor <- dbWc.ShopFloorInfo.ShopFloor
                wcinfo.WorkCenter <- dbWc.WorkCenter
                wcinfo.StartHour <- dbWc.StartHour
                wcinfo.EndHour <- dbWc.EndHour
                
                try 
                    ctx.SubmitUpdates()
                    |> Success
                with
                | ex -> Failure <| sprintf "%s" ex.Message
            | Failure msg -> Failure msg
        

type ToggleWorkCenter = Activation -> WorkCenter -> Result<unit>
let toggleWorkCenter: ToggleWorkCenter =
    fun activation workcenter -> 
        let (WorkCenter wc) = workcenter
        let ctx = Sql.GetDataContext()
        let current, future = flags activation
        let dbsOpt = 
            query {
                for workcenter in ctx.Timeentryapp.Workcenter do
                    where (workcenter.WorkCenter = wc && workcenter.Active = current)
                    select workcenter
            }
            |> Seq.tryHead

        match dbsOpt with
            | Some dbs -> 
                dbs.Active <- future
                try 
                    ctx.SubmitUpdates()
                    |> Success
                with
                    ex -> Failure ex.Message
            | None -> Failure <| sprintf "Workcenter \'%s\' is missing or %s." wc activation.State

///Desactivates a site if active
type DeleteWorkCenter = WorkCenter -> Result<unit>
let deleteWorkCenter: DeleteWorkCenter  = toggleWorkCenter Desactivate

type ActivateWorkCenter = WorkCenter -> Result<unit>
let activateWorkCenter: ActivateWorkCenter = toggleWorkCenter Activate

//Test to write

(* EVENT FUNCTIONS  *)

//Insert new workcenter in DB
type InsertEvent = Event -> Result<unit>

let insertEvent event =
    let ctx = Sql.GetDataContext()
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
    let ctx = Sql.GetDataContext()
    query {
        for event in ctx.Timeentryapp.Event do
            select event.Event
    }
    |> Seq.toArray

type GetEventById = EventId -> WorkCenterInfo

let getEventById id = 
    let ctx = Sql.GetDataContext()
    query {
        for event in ctx.Timeentryapp.Event do
            where (event.EventId = id)
            select event
    }
    |> Seq.head

type GetEventByCode = Event -> EventEntry
let getEventByCode code = 
    let ctx = Sql.GetDataContext()
    query {
        for event in ctx.Timeentryapp.Event do
            where (event.Event = code)
            select event
    }
    |> Seq.head

type UpdateEvent = EventId -> Event -> Result<unit>
let updateEvent eventId event = 
    let ctx = Sql.GetDataContext()
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
    let ctx = Sql.GetDataContext()
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
    let ctx = Sql.GetDataContext()
    query {
        for workOrderEntry in ctx.Timeentryapp.Workorderentry do
            select workOrderEntry.WorkOrder
    }
    |> Seq.toArray

type GetWorkOrderById = WorkOrderEntryId -> WorkOrderEntry

let getWorkOrderById id = 
    let ctx = Sql.GetDataContext()
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
    let ctx = Sql.GetDataContext()
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
    let ctx = Sql.GetDataContext()
    query {
        for eventEntry in ctx.Timeentryapp.Evententry do
            where (eventEntry.EventEntryId = id)
            select eventEntry
    }
    |> Seq.head


let updateEventEntry eventId eventEntry =
    let ctx = Sql.GetDataContext()
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

(* Time Record Functions *)

type InsertTimeRecord = TimeRecord -> Result<unit>
let insertTimeRecord : InsertTimeRecord =
    fun timeRecord -> 
        let ctx = Sql.GetDataContext()
        let dbrecords = toDBTimeRecord timeRecord
        dbrecords
        |> List.map(fun record -> 
            let tr = ctx.Timeentryapp.Timerecord.Create()
            tr.Site         <- record.Site
            tr.Shopfloor    <- record.ShopFloor
            tr.TimeType     <- record.TimeType
            tr.StartTime    <- record.StartTime
            tr.EndTime      <- record.EndTime
            tr.DurationHr   <- record.DurationHr
            tr.Allocation   <- record.Allocation
            tr.WorkOrderEntry

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
        )  

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