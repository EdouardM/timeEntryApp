// reference the type provider dll
#r "../../packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll"
#r "System.Configuration"
#r "../../packages/FSharp.Configuration/lib/net40/FSharp.Configuration.dll"


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



open System.Configuration
open FSharp.Configuration

let t = ConfigurationManager.ConnectionStrings.Item("Dev").ConnectionString

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
let t = ConfigurationManager.ConnectionStrings |> Seq.cast<ConnectionStringSettings>
t |> Seq.iter (fun conn -> printfn "%s" conn.ConnectionString)

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


let onlyOne name key = 
    function
        | [] -> Failure <| sprintf "%s not found for: %s" name key
        | [x] -> Success x
        | x::xs -> Failure <| sprintf "More than one %s found for: %s" name key

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
type DesactivateShopfloor = ShopFloor -> Result<unit>
let desactivateShopfloor: DesactivateShopfloor  = toggleShopfloor Desactivate

type ActivateShopfloor = ShopFloor -> Result<unit>
let activateShopfloor: ActivateShopfloor = toggleShopfloor Activate


desactivateShopfloor(ShopFloor "F231A")
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
type DesactivateWorkCenter = WorkCenter -> Result<unit>
let desactivateWorkCenter: DesactivateWorkCenter  = toggleWorkCenter Desactivate

type ActivateWorkCenter = WorkCenter -> Result<unit>
let activateWorkCenter: ActivateWorkCenter = toggleWorkCenter Activate

//Test to write
getWorkCenterCodes()
desactivateWorkCenter (WorkCenter "F1")
activateWorkCenter (WorkCenter "F1")


(* EVENT FUNCTIONS  *)
type GetEventCodes = unit -> string list
let getEventCodes: GetEventCodes =
    fun () -> 
        let ctx = Sql.GetDataContext()
        query {
            for event in ctx.Timeentryapp.Event do
                where (event.Active = 1y)
                select event.Event
        }
        |> Seq.toList

getEventCodes()

//Insert new workcenter in DB
type InsertEvent = Event -> Result<unit>

let insertEvent: InsertEvent =
    fun event -> 
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


let ev = ZeroPerson "NET"
insertEvent(ev)
getEventCodes()


type GetEvent = Event -> Result<DBEvent>

let getEvent: GetEvent =
    fun ev ->
        let  e = extractEvent ev  
        let ctx = Sql.GetDataContext()
        query {
            for event in ctx.Timeentryapp.Event do
                where (event.Event = e && event.Active = 1y)
                select event
        }
        |> Seq.map (fun (event) -> 
                {
                            DBEvent.Event   = event.Event
                            HasInfo      = sbyteTobool event.HasInfo
                            AllowZeroPerson = sbyteTobool event.AllowZeroPerson
                }
            )
        |> Seq.toList
        |> onlyOne "Event" e

//TESTS
getEvent (WithInfo "NET")
getEvent (ZeroPerson "EOD")

type UpdateEvent = Event -> Result<unit>
let updateEvent: UpdateEvent =
    fun ev -> 
        let  e = extractEvent ev  
        let ctx = Sql.GetDataContext()
        let eventRes = 
            query {
                for event in ctx.Timeentryapp.Event do
                    where (event.Event = e && event.Active = 1y)
                    select event
            }
            |> Seq.toList
            |> (function 
                    | [] -> Failure <| sprintf "Event not found for: %s" e
                    | [x] -> Success x
                    | x::xs -> Failure <| sprintf "More than one event found for: %s" e)

        let dbev = toDBEvent ev
        match eventRes with
            | Success event -> 
                event.HasInfo <- boolToSbyte  dbev.HasInfo
                event.AllowZeroPerson <- boolToSbyte dbev.AllowZeroPerson
                
                try 
                    ctx.SubmitUpdates()
                    |> Success
                with
                | ex -> Failure <| sprintf "%s" ex.Message
            | Failure msg -> Failure msg
        

updateEvent (WithoutInfo "NET")
getEvent(WithoutInfo "NET")

updateEvent (WithInfo "NET")
getEvent(WithoutInfo "NET")


type ToggleEvent = Activation -> Event -> Result<unit>
let toggleEvent: ToggleEvent =
    fun activation event -> 
        let ev = extractEvent event
        let ctx = Sql.GetDataContext()
        let current, future = flags activation
        let dbsOpt = 
            query {
                for event in ctx.Timeentryapp.Event do
                    where (event.Event = ev && event.Active = current)
                    select event
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
            | None -> Failure <| sprintf "Event \'%s\' is missing or %s." ev activation.State

type DesactivateEvent = Event -> Result<unit>
let desactivateEvent: DesactivateEvent  = toggleEvent Desactivate

type ActivateEvent = Event -> Result<unit>
let activateEvent: ActivateEvent = toggleEvent Activate

desactivateEvent (WithInfo "NET")
activateEvent    (WithInfo "NET")

updateEvent (WithoutInfo "NET")
getEvent (WithoutInfo "NET")


(* WorkOrderEntry Functions *)

type GetWorkOrderCodes = unit -> string list
let getWorkOrderCodes: GetWorkOrderCodes =
    fun () -> 
        let ctx = Sql.GetDataContext()
        query {
            for workorder in ctx.Timeentryapp.Workorderentry do
                where (workorder.Active = 1y)
                select workorder.WorkOrder
        }
        |> Seq.toList

getWorkOrderCodes()


//Insert new workcenter in DB
type InsertWorkOrder = WorkOrderEntry -> Result<unit>
let insertWorkOrderEntry workOrderEntry  =
    let ctx = Sql.GetDataContext()
    let wo = ctx.Timeentryapp.Workorderentry.Create()
    let dbwo = toDBWorkOrderEntry workOrderEntry
    let workcenterRes = getWorkCenter workOrderEntry.WorkCenter
    match workcenterRes with
        | Success wc -> 
            wo.WorkOrder            <- dbwo.WorkOrder
            wo.ItemCode             <- dbwo.ItemCode
            wo.WorkCenter           <- wc.WorkCenter
            wo.WorkOrderStatus      <- dbwo.WorkOrderStatus
            wo.TotalMachineTimeHr   <- (float32 0.)
            wo.TotalLabourTimeHr    <- (float32 0.)
            wo.Active <- 1y
            
            try 
                ctx.SubmitUpdates()
                |> Success
            with
            | ex -> Failure <| sprintf "%s" ex.Message
        | Failure msg -> Failure msg


let wo1 = { WorkOrder = WorkOrder "12243"; ItemCode = ItemCode "099148"; WorkCenter = WorkCenter "F1"; TotalMachineTimeHr = TimeHr 0.f; TotalLabourTimeHr = TimeHr 0.f; Status =  Open }
getWorkCenterCodes()
insertWorkOrderEntry wo1
getWorkOrderCodes()


type GetWorkOrder = WorkOrder -> Result<DBWorkOrderEntry>
let getWorkOrder: GetWorkOrder =
    fun workOrder ->
        let (WorkOrder wo) = workOrder 
        let ctx = Sql.GetDataContext()
        query {
            for workorder in ctx.Timeentryapp.Workorderentry do
                where (workorder.WorkOrder = wo && workorder.Active = 1y)
                select workorder
        }
        |> Seq.map(fun workorder -> 
            {
                DBWorkOrderEntry.WorkOrder   = workorder.WorkOrder
                WorkCenter                   = workorder.WorkCenter
                ItemCode                     = workorder.ItemCode
                TotalMachineTimeHr           = workorder.TotalMachineTimeHr
                TotalLabourTimeHr            = workorder.TotalLabourTimeHr
                WorkOrderStatus              =  workorder.WorkOrderStatus
            }
        )
        |> Seq.toList
        |> onlyOne "WorkOrder" wo

getWorkOrder (WorkOrder "12243")

getWorkOrder (WorkOrder "12242")


type UpdateWorkOrderEntry = WorkOrderEntry -> Result<unit>
let updateWorkOrderEntry: UpdateWorkOrderEntry =
    fun workOrderEntry -> 
        let (WorkOrder wo) =  workOrderEntry.WorkOrder
        let ctx = Sql.GetDataContext()
        let workOrderEntryRes = 
            query {
                for workorder in ctx.Timeentryapp.Workorderentry do
                    where (workorder.WorkOrder = wo && workorder.Active = 1y)
                    select workorder
            }
            |> Seq.toList
            |> onlyOne "WokOrder" wo

        let dbwo = toDBWorkOrderEntry workOrderEntry
        match workOrderEntryRes with
            | Success wo -> 
                wo.WorkCenter            <- dbwo.WorkCenter
                wo.ItemCode              <- dbwo.ItemCode
                wo.TotalMachineTimeHr    <- dbwo.TotalMachineTimeHr
                wo.TotalLabourTimeHr     <- dbwo.TotalLabourTimeHr
                wo.WorkOrderStatus       <- dbwo.WorkOrderStatus
                
                try 
                    ctx.SubmitUpdates()
                    |> Success
                with
                | ex -> Failure <| sprintf "%s" ex.Message
            | Failure msg -> Failure msg

let wo1' = {wo1 with TotalMachineTimeHr = TimeHr 1000.f}
updateWorkOrderEntry wo1'
getWorkOrder (WorkOrder "12243")


(* EventEntry Functions *)

//Insert new workcenter in DB
type InsertEventEntry = EventEntry -> Result<unit>
let insertEventEntry: InsertEventEntry =
    fun eventEntry  ->
        let e = extractEventfromEntry eventEntry
        let ctx = Sql.GetDataContext()
        let ev = ctx.Timeentryapp.Evententry.Create()
        let dbev = toDBEventEntry eventEntry
        let eventRes = getEvent e
        match eventRes with
        | Success event -> 
            ev.Event    <- event.Event
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
        | Failure msg -> Failure msg

let eventEntry = EventWithoutInfo (WithoutInfo "NET")

insertEventEntry eventEntry

type GetEventEntry = EventEntryId -> Result<DBEventEntry>

let getEventEntry: GetEventEntry = 
    fun eventId -> 
        let ctx = Sql.GetDataContext()
        query {
                for evententry in ctx.Timeentryapp.Evententry do
                    join event in ctx.Timeentryapp.Event on (evententry.Event = event.Event)
                    where (evententry.EventEntryId = eventId && evententry.Active = 1y)
                    select (event, evententry)
            }
            |> Seq.map(fun (event, evententry) -> 
                {
                    Event = { DBEvent.Event = event.Event;
                            HasInfo =  sbyteTobool event.HasInfo; 
                            AllowZeroPerson = sbyteTobool event.AllowZeroPerson }
                    Machine = evententry.Machine  
                    Cause   = evententry.Cause
                    Comments = evententry.Comments
                    Solution = evententry.Solution
                })
            |> Seq.toList
            |> onlyOne "EventEntry" (string eventId)



getEventEntry (2u)

type UpdatEventEntry = EventEntryId -> EventEntry -> Result<unit>

let updateEventEntry: UpdatEventEntry =
    fun eventId eventEntry ->
        let ctx = Sql.GetDataContext()
        let dbev = toDBEventEntry eventEntry
        
        let eventEntryRes = 
            query {
                for evententry in ctx.Timeentryapp.Evententry do
                    where (evententry.EventEntryId = eventId && evententry.Active = 1y)
                    select evententry
            }
            |> Seq.toList
            |> onlyOne "EventEntry" (string eventId)

        match eventEntryRes with
            | Success ev -> 
                    ev.Machine  <- dbev.Machine
                    ev.Cause    <- dbev.Cause
                    ev.Solution <- dbev.Solution
                    ev.Comments <- dbev.Comments
                    try 
                        ctx.SubmitUpdates()
                        |> Success
                    with
                    | ex -> Failure <| sprintf "%s" ex.Message
            | Failure msg -> Failure msg

(* Time Record Functions *)

type InsertTimeRecord = TimeRecord -> Result<unit>
let insertTimeRecord : InsertTimeRecord =
    fun timeRecord ->
        let ctx = Sql.GetDataContext()
        let dbrecords =      timeRecord
        dbrecords
        |> List.map(fun record -> 
            let tr = ctx.Timeentryapp.Timerecord.Create()
            tr.Site             <- record.Site
            tr.Shopfloor        <- record.ShopFloor
            tr.TimeType         <- record.TimeType
            tr.StartTime        <- record.StartTime
            tr.EndTime          <- record.EndTime
            tr.DurationHr       <- record.DurationHr
            tr.Allocation       <- record.Allocation
            tr.WorkOrder        <- record.DBWorkOrderEntry.WorkOrder
            try 
                        ctx.SubmitUpdates()
                        |> Success
            with
            | ex -> Failure <| sprintf "%s" ex.Message
            )
        //Update work ORder to add time...
(*
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
*)
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