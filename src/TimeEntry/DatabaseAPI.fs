namespace TimeEntry

module DBCommands =
    
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


    type DBContext = Sql.dataContext

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


    let trySubmit (ctx: DBContext) = 
        try 
            ctx.SubmitUpdates()
            Success ()
        with
            ex -> Failure ex.Message


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

    type InsertSite = Site -> Result<unit>

    ///Inserts a new site
    let insertSite: InsertSite =
        fun site ->
            let (Site s) = site
            let ctx = Sql.GetDataContext()
            let dbs = ctx.Timeentryapp.Site.Create()
            dbs.Site <- s
            dbs.Active <- 1y
            trySubmit ctx

            
    type ToggleSite = Activation -> string -> Result<unit>
    let toggleSite: ToggleSite =
        fun activation s -> 
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
    type DesactivateSite = string -> Result<unit>
    let desactivateSite: DesactivateSite  = toggleSite Desactivate

    type ActivateSite = string -> Result<unit>
    let activateSite: ActivateSite = toggleSite Activate

    let deleteSites () = 
        let ctx = Sql.GetDataContext()
        query {
            for site in ctx.Timeentryapp.Site do
                select site
        }
        |> Seq.toList
        |> List.iter(fun site -> site.Delete() )
        trySubmit ctx

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


    type GetShopFloorInfo = string -> Result<DBShopFloorInfo>
    let getShopFloorInfo: GetShopFloorInfo =
        fun sf ->
            let ctx = Sql.GetDataContext()
            query {
                for shopfloor in ctx.Timeentryapp.Shopfloor do
                    where (shopfloor.Active = 1y && shopfloor.Shopfloor = sf)
                    select shopfloor
            }
            |> Seq.map(fun dbsf -> dbsf.MapTo<DBShopFloorInfo>() )
            |> Seq.toList
            |> onlyOne "Shopfloor" sf


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
            
            trySubmit ctx

    type ToggleShopfloor = Activation -> string -> Result<unit>

    let toggleShopfloor: ToggleShopfloor =
        fun activation s -> 
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
    type DesactivateShopfloor = string -> Result<unit>
    let desactivateShopfloor: DesactivateShopfloor  = toggleShopfloor Desactivate

    type ActivateShopfloor = string -> Result<unit>
    let activateShopfloor: ActivateShopfloor = toggleShopfloor Activate

    let deleteShopfloors () = 
        let ctx = Sql.GetDataContext()
        query {
            for shopfloor in ctx.Timeentryapp.Shopfloor do
                select shopfloor
        }
        |> Seq.toList
        |> List.iter(fun shopfloor -> shopfloor.Delete() )
        trySubmit ctx

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


    type GetWorkCenter = string -> Result<DBWorkCenterInfo>

    let getWorkCenter: GetWorkCenter =
        fun wc -> 
            let ctx = Sql.GetDataContext()
            query {
                for workcenter in ctx.Timeentryapp.Workcenter do
                    join shopfloor in ctx.Timeentryapp.Shopfloor on (workcenter.Shopfloor = shopfloor.Shopfloor)
                    where (workcenter.WorkCenter = wc && workcenter.Active = 1y)
                    select (workcenter, shopfloor)
            }
            |> Seq.map (fun (wc, sf) -> 
                {
                            DBWorkCenterInfo.WorkCenter     = wc.WorkCenter
                            ShopFloorInfo                   = { Site = sf.Site; ShopFloor = sf.Shopfloor}
                            StartHour                       = wc.StartHour
                            EndHour                         = wc.EndHour
                }
            )
            |> Seq.toList
            |> onlyOne "Workcenter" wc

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

    type UpdateWorkCenter = WorkCenterInfo -> Result<unit>
    let updateWorkCenter: UpdateWorkCenter =
        fun workcenterinfo ->
            let ctx = Sql.GetDataContext()
            let wcinfoRes =
                query {
                    for workcenter in ctx.Timeentryapp.Workcenter do
                        where (workcenter.WorkCenter = workcenterinfo.WorkCenter && workcenter.Active = 1y)
                        select workcenter
                }
                |> Seq.toList
                |> onlyOne "Workcenter" wc

            let dbWc = toDBWorkCenterInfo workcenterinfo
            match wcinfoRes with
                | Success wcinfo -> 
                    wcinfo.Shopfloor <- dbWc.ShopFloorInfo.ShopFloor
                    wcinfo.StartHour <- dbWc.StartHour
                    wcinfo.EndHour <- dbWc.EndHour
                    
                    try 
                        ctx.SubmitUpdates()
                        |> Success
                    with
                    | ex -> Failure      <| sprintf "Update Workcenter: %s" ex.Message
                | Failure msg -> Failure <| sprintf "Upate Workcenter: %s" msg
            

    type ToggleWorkCenter = Activation -> string -> Result<unit>
    let toggleWorkCenter: ToggleWorkCenter =
        fun activation wc -> 
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
    type DesactivateWorkCenter = string -> Result<unit>
    let desactivateWorkCenter: DesactivateWorkCenter  = toggleWorkCenter Desactivate

    type ActivateWorkCenter = string -> Result<unit>
    let activateWorkCenter: ActivateWorkCenter = toggleWorkCenter Activate

    let deleteWorkCenters () = 
        let ctx = Sql.GetDataContext()
        query {
            for workcenter in ctx.Timeentryapp.Workcenter do
                select workcenter
        }
        |> Seq.toList
        |> List.iter(fun workcenter -> workcenter.Delete() )
        trySubmit ctx

    (* MACHINE FUNCTIONS *)
    type GetMachineCodes = unit -> string list

    ///Returns a list of codes of active sites
    let getMachineCodes: GetMachineCodes =
        fun () ->
            let ctx = Sql.GetDataContext()
            query {
                for machine in ctx.Timeentryapp.Machine do
                    where (machine.Active = 1y)
                    select machine.Machine
            }
            |> Seq.toList

    type InsertMachine = MachineInfo -> Result<unit>

    ///Inserts a new site
    let insertMachine: InsertMachine =
        fun machineInfo ->
            let mach = toDBMachineInfo machineInfo
            let ctx = Sql.GetDataContext()
            let dbmach = ctx.Timeentryapp.Machine.Create()
            dbmach.Machine      <- mach.Machine
            dbmach.Shopfloor    <- mach.ShopFloorInfo.ShopFloor
            dbmach.Active       <- 1y
            
            trySubmit ctx

    type ToggleMachine = Activation -> string -> Result<unit>
    let toggleMachine: ToggleMachine =
        fun activation m -> 
            let ctx = Sql.GetDataContext()
            let current, future = flags activation
            let dbmOpt = 
                query {
                    for machine in ctx.Timeentryapp.Machine do
                        where (machine.Machine = m && machine.Active = current)
                        select machine
                }
                |> Seq.tryHead
            match dbmOpt with
                | Some dbm -> 
                    dbm.Active <- future
                    try 
                        ctx.SubmitUpdates()
                        |> Success
                    with
                        ex -> Failure ex.Message
                | None -> Failure <| sprintf "Machine \'%s\' is missing or %s." m activation.State

    ///Desactivates a site if active
    type DesactivateMachine = string -> Result<unit>
    let desactivateMachine: DesactivateMachine  = toggleMachine Desactivate

    type ActivateMachine = string -> Result<unit>
    let activateMachine: ActivateMachine = toggleMachine Activate

    let deleteMachines () = 
        let ctx = Sql.GetDataContext()
        query {
            for machine in ctx.Timeentryapp.Machine do
                select machine
        }
        |> Seq.toList
        |> List.iter(fun machine -> machine.Delete() )
        trySubmit ctx


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

    type GetEvent = string -> Result<DBEvent>

    let getEvent: GetEvent =
        fun ev ->  
            let ctx = Sql.GetDataContext()
            query {
                for event in ctx.Timeentryapp.Event do
                    where (event.Event = ev && event.Active = 1y)
                    select event
            }
            |> Seq.map (fun (event) -> event.MapTo<DBEvent>() )
            |> Seq.toList
            |> onlyOne "Event" ev
    
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
                |> onlyOne "Event" e

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
            

    type ToggleEvent = Activation -> string -> Result<unit>
    let toggleEvent: ToggleEvent =
        fun activation ev -> 
            let ctx = Sql.GetDataContext()
            let current, future = flags activation
            let dbsOpt = 
                query {
                    for event in ctx.Timeentryapp.Event do
                        where (event.Event = ev && event.Active = current)
                        select event }
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

    type DesactivateEvent = string -> Result<unit>
    let desactivateEvent: DesactivateEvent  = toggleEvent Desactivate

    type ActivateEvent = string -> Result<unit>
    let activateEvent: ActivateEvent = toggleEvent Activate

    let deleteEvents () = 
        let ctx = Sql.GetDataContext()
        query {
            for event in ctx.Timeentryapp.Event do
                select event
        }
        |> Seq.toList
        |> List.iter(fun event -> event.Delete() )
        trySubmit ctx


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

    //Insert new workcenter in DB
    type InsertWorkOrder = WorkOrderEntry -> Result<unit>
    let insertWorkOrderEntry workOrderEntry  =
        let ctx = Sql.GetDataContext()
        let wo = ctx.Timeentryapp.Workorderentry.Create()
        let dbwo = toDBWorkOrderEntry workOrderEntry
        let (WorkCenter wc) = workOrderEntry.WorkCenter
        let workcenterRes = getWorkCenter wc
        match workcenterRes with
            | Success wc -> 
                wo.WorkOrder            <- dbwo.WorkOrder
                wo.ItemCode             <- dbwo.ItemCode
                wo.WorkCenter           <- wc.WorkCenter
                wo.WorkOrderStatus      <- dbwo.WorkOrderStatus
                wo.TotalMachineTimeHr   <- (float32 0.)
                wo.TotalLabourTimeHr    <- (float32 0.)
                wo.Active <- 1y
                
                trySubmit ctx

            | Failure msg -> Failure msg


    type GetWorkOrder = string -> Result<DBWorkOrderEntry>
    let getWorkOrder: GetWorkOrder =
        fun wo ->
            //let (WorkOrder wo) = workOrder 
            let ctx = Sql.GetDataContext()
            query {
                for workorder in ctx.Timeentryapp.Workorderentry do
                    where (workorder.WorkOrder = wo && workorder.Active = 1y)
                    select workorder
            }
            |> Seq.map(fun workorder -> workorder.MapTo<DBWorkOrderEntry>() )
            |> Seq.toList
            |> onlyOne "WorkOrder" wo
    
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
                    
                    trySubmit ctx 

                | Failure msg -> Failure msg

    let deleteWorkOrders () = 
        let ctx = Sql.GetDataContext()
        query {
            for workorder in ctx.Timeentryapp.Workorderentry do
                select workorder
        }
        |> Seq.toList
        |> List.iter(fun workorder -> workorder.Delete() )
        
        trySubmit ctx

    (* EventEntry Functions *)


    ///Insert new workcenter in DB and return Id of inserted record if it succeded
    type InsertEventEntry = EventEntry -> Result<uint32>
    let insertEventEntry: InsertEventEntry =
        fun eventEntry  ->
            let e = 
                eventEntry
                |> extractEventfromEntry
                |> extractEvent
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
                    //Return id of inserted record
                    ev.EventEntryId
                    |> Success
                with
                | ex -> Failure <| sprintf "%s" ex.Message
            | Failure msg -> Failure msg

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
                        trySubmit ctx

                | Failure msg -> Failure msg

    let deleteEventEntries () = 
        let ctx = Sql.GetDataContext()
        query {
            for evententry in ctx.Timeentryapp.Evententry do
                select evententry
        }
        |> Seq.toList
        |> List.iter(fun evententry -> evententry.Delete() )
        trySubmit ctx


    (* Time Record Functions *)

    type GetTimeRecord = TimeRecordId -> Result<DBTimeRecord>
    let getTimeRecord: GetTimeRecord = 
        fun id -> 
            let ctx = Sql.GetDataContext()

            query {
                    for timerecord in ctx.Timeentryapp.Timerecord do
                        where (timerecord.TimeRecordId = id && timerecord.Active = 1y)
                        select timerecord
                }
            |> Seq.map(fun record ->
                    let dbWogetWorkOrderRes = 
                        record.WorkOrder  
                        |> Option.map getWorkOrder
                        |> unwrapResOpt
                    let dbEventRes = 
                        record.EventEntryId  
                        |> Option.map getEventEntry
                        |> unwrapResOpt

                    {
                        DBTimeRecord.Site   = record.Site
                        ShopFloor           = record.Shopfloor
                        WorkCenter          = record.WorkCenter
                        TimeType            = record.TimeType
                        StartTime           = record.StartTime
                        EndTime             = record.EndTime
                        DurationHr          = record.DurationHr
                        Allocation          = record.Allocation
                        NbPeople            = record.NbPeople
                        WorkOrderEntry      = dbWogetWorkOrderRes
                        EventEntry          = dbEventRes
                        Status              = record.RecordStatus
                    })
            |> Seq.toList
            |> onlyOne "TimeRecord" (string id)

    let insertDBTimeRecord : Sql.dataContext -> DBTimeRecord ->  EventEntryId option -> Result<uint32> = 
        fun ctx record eventId -> 
            let wo = Option.map(fun dbwo -> dbwo.WorkOrder) record.WorkOrderEntry
            let tr = ctx.Timeentryapp.Timerecord.Create()
            tr.Site             <- record.Site
            tr.Shopfloor        <- record.ShopFloor
            tr.WorkCenter       <- record.WorkCenter
            tr.TimeType         <- record.TimeType
            tr.StartTime        <- record.StartTime
            tr.EndTime          <- record.EndTime
            tr.DurationHr       <- record.DurationHr
            tr.Allocation       <- record.Allocation
            tr.NbPeople         <- record.NbPeople
            tr.WorkOrder        <- wo
            tr.EventEntryId     <- eventId
            tr.RecordStatus     <- record.Status
            tr.LastUpdate       <- System.DateTime.Now
            tr.Active           <- 1y
            try 
                        ctx.SubmitUpdates()
                        tr.TimeRecordId
                        |> Success
            with
            | ex -> Failure <| sprintf "%s" ex.Message

    type InsertTimeRecord = TimeRecord -> Result<uint32> list

    let insertTimeRecord : InsertTimeRecord =
        fun timeRecord ->
            let ctx = Sql.GetDataContext()
            match timeRecord.Allocation with
                | WorkOrderEntry workorderEntry ->
                    
                    let dbrecords = toDBTimeRecord timeRecord
                    dbrecords
                    |> List.map (fun record ->
                    insertDBTimeRecord ctx record (None) )

                | EventEntry eventEntry -> 
                    let dbrecords = toDBTimeRecord timeRecord
                    dbrecords
                    |> List.map (fun record ->
                            insertEventEntry eventEntry 
                            //Add User in EventEntry to be sure to get the correct ID!
                            //|> map lastEventEntryId
                            |> bind (fun id -> insertDBTimeRecord ctx record (Some id)
                    ))
            //Update work ORder to add time...
    
    let deleteTimeRecords () = 
        let ctx = Sql.GetDataContext()
        query {
            for timerecord in ctx.Timeentryapp.Timerecord do
                select timerecord
        }
        |> Seq.toList
        |> List.iter(fun timerecord -> timerecord.Delete() )
        trySubmit ctx


    (* USER AUTHORIZATION FUNCTIONS *)
    type GetUserAuth = string -> string list
    
    let getUserAuth: GetUserAuth =
        fun login -> 
                let ctx = Sql.GetDataContext()         
                query {
                    for user in ctx.Timeentryapp.User do
                        join authsite in ctx.Timeentryapp.Userauthorization on (user.Login = authsite.Login)
                        where (user.Login = login && user.Active = 1y && authsite.Active = 1y)
                        select (authsite.Site)
                }
                |> Seq.toList

    //Insert new User Authorization in DB
    type InsertUserAuth = Login -> Site -> Result<unit>
    let insertUserAuth: InsertUserAuth =
        fun login site ->
            let (Login usrlogin) = login
            let (Site authsite)  =  site
            let ctx             = Sql.GetDataContext() 
            let auth = ctx.Timeentryapp.Userauthorization.Create()
            auth.Login  <- usrlogin
            auth.Site   <- authsite
            auth.Active <- 1y

            trySubmit ctx

    type ToggleUserAuth = Activation -> string -> string -> Result<unit>
    let toggleUserAuth: ToggleUserAuth =
        fun activation login site -> 
            let ctx = Sql.GetDataContext()
            let current, future = flags activation
            let dbsOpt = 
                query {
                    for authsite in ctx.Timeentryapp.Userauthorization do
                        where (authsite.Login = login && authsite.Site = site && authsite.Active = current)
                        select authsite }
                |> Seq.tryHead

            match dbsOpt with
                | Some dbs -> 
                    dbs.Active <- future
                    try 
                        ctx.SubmitUpdates()
                        |> Success
                    with
                        ex -> Failure ex.Message
                | None -> Failure <| sprintf "Authorization for user \'%s\' on site %s is missing or %s." login site  activation.State

    type DesactivateUserAuth = string -> Result<unit>
    let desactivateUserAuth: DesactivateUserAuth  = toggleUserAuth Desactivate

    type ActivateUserAuth = string -> Result<unit>
    let activateUserAuth: ActivateUserAuth = toggleUserAuth Activate

    let deleteUserAuth () = 
        let ctx = Sql.GetDataContext()
        query {
            for userauth in ctx.Timeentryapp.Userauthorization do
                select userauth
        }
        |> Seq.toList
        |> List.iter(fun userauth -> userauth.Delete() )
        trySubmit ctx


     (* USER FUNCTIONS  *)

    type GetUserLogins = unit -> string list
    let getUserLogins: GetUserLogins =
        fun () -> 
            let ctx = Sql.GetDataContext()
            query {
                for user in ctx.Timeentryapp.User do
                    where (user.Active = 1y)
                    select user.Login
            }
            |> Seq.toList


    type GetUser = string -> Result<DBUser>

    let getUser: GetUser =
        fun login -> 
            let ctx = Sql.GetDataContext()
            //Get Authorized Sites first
            let sites =  getUserAuth login
            query {
                for user in ctx.Timeentryapp.User do
                where (user.Login = login && user.Active = 1y)
                select user
            }
            |> Seq.map (fun (user) ->
                {
                            DBUser.Login    = user.Login
                            Name            = user.UserRealName
                            Level           = user.AuthLevel
                            AllSites        = sbyteTobool user.AllSites
                            SiteList        = sites
                }
            )
            |> Seq.toList
            |> onlyOne "User" login

    //Insert new user in DB
    type InsertUser = User -> Result<unit list>

    let insertUser: InsertUser =
        fun user -> 
            let ctx     = Sql.GetDataContext()
            let usr     = ctx.Timeentryapp.User.Create()
            let dbUs    = toDBUser user
            usr.Login         <- dbUs.Login
            usr.UserRealName  <- dbUs.Name
            usr.AuthLevel     <- dbUs.Level
            usr.AllSites      <- boolToSbyte dbUs.AllSites
            usr.Active        <- 1y
            try 
                ctx.SubmitUpdates()
                |> Success
            with
            | ex -> Failure <| sprintf "%s" ex.Message

    type UpdatUser = User -> Result<unit>

    let updateUser: User =
        fun newuser ->
            let ctx = Sql.GetDataContext()
            let dbus = toDBUSer newuser
            
            let userRes = 
                query {
                    for user in ctx.Timeentryapp.User do
                        where (user.Login = newuser.Login  && user.Active = 1y)
                        select user
                }
                |> Seq.toList
                |> onlyOne "EventEntry" (string eventId)

            match userRes with
                | Success user -> 
                        user.UserRealName   <- dbus.Name
                        user.AllSites       <- dbus.AllSites
                        user.AuthLevel      <- dbus.Level
                        trySubmit ctx

                | Failure msg -> Failure msg

    type ToggleUser = Activation -> string -> Result<unit>
    let toggleUser: ToggleUser =
        fun activation login -> 
            let ctx = Sql.GetDataContext()
            let current, future = flags activation
            let dbsOpt = 
                query {
                    for user in ctx.Timeentryapp.User do
                        where (user.Login = login && user.Active = current)
                        select user }
                |> Seq.tryHead

            match dbsOpt with
                | Some dbs -> 
                    dbs.Active <- future
                    try 
                        ctx.SubmitUpdates()
                        |> Success
                    with
                        ex -> Failure ex.Message
                | None -> Failure <| sprintf "User \'%s\' is missing or %s." login site  activation.State

    type DesactivateUser = string -> Result<unit>
    let desactivateUser: DesactivateUser  = toggleUser Desactivate

    type ActivateUser = string -> Result<unit>
    let activateUser: ActivateUser = toggleUser Activate

    let deleteUser () = 
        let ctx = Sql.GetDataContext()
        query {
            for user in ctx.Timeentryapp.User do
                select user
        }
        |> Seq.toList
        |> List.iter(fun user -> user.Delete() )
        trySubmit ctx
