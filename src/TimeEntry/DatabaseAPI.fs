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
            let (WorkCenter w) =  workcenterinfo.WorkCenter
            let ctx = Sql.GetDataContext()
            let wcinfoRes =
                query {
                    for workcenter in ctx.Timeentryapp.Workcenter do
                        where (workcenter.WorkCenter = w && workcenter.Active = 1y)
                        select workcenter
                }
                |> Seq.toList
                |> onlyOne "Workcenter" w

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


   (* ACTIVITY WORKCENTER ACCESS FUNCTIONS *)
    type GetActivityWorkCenters = string -> string list
    
    let getActivityWorkCenters: GetActivityWorkCenters =
        fun code -> 
                let ctx = Sql.GetDataContext()         
                query {
                    for activity in ctx.Timeentryapp.Activity do
                        join authActivity in ctx.Timeentryapp.Activityworkcenteraccess on (activity.Code = authActivity.Activity)
                        where (activity.Code = code && activity.Active = 1y && authActivity.Active = 1y)
                        select (authActivity.WorkCenter)
                }
                |> Seq.toList

    type InsertActivityWorkCenters = ActivityCode -> WorkCenter -> Result<unit>
    let insertActivityWorkCenters: InsertActivityWorkCenters =
        fun activity workcenter ->
            let (ActivityCode code) = activity
            let (WorkCenter authworkcenter)  =  workcenter
            let ctx             = Sql.GetDataContext() 
            let auth = ctx.Timeentryapp.Activityworkcenteraccess.Create()
            auth.Activity       <- code
            auth.WorkCenter     <- authworkcenter
            auth.Active         <- 1y

            trySubmit ctx

    type ToggleActivityWorkCenters = Activation -> string -> string -> Result<unit>
    let toggleActivityWorkCenters: ToggleActivityWorkCenters =
        fun activation code workcenter -> 
            let ctx = Sql.GetDataContext()
            let current, future = flags activation
            let dbsOpt = 
                query {
                    for authwc in ctx.Timeentryapp.Activityworkcenteraccess do
                        where (authwc.Activity = code && authwc.WorkCenter = workcenter && authwc.Active = current)
                        select authwc }
                |> Seq.tryHead

            match dbsOpt with
                | Some dbs -> 
                    dbs.Active <- future
                    try 
                        ctx.SubmitUpdates()
                        |> Success
                    with
                        ex -> Failure ex.Message
                | None -> Failure <| sprintf "Authorization for activity \'%s\' on workcenter %s is missing or %s." code workcenter  activation.State

    type DesactivateActivityWorkCenters = string -> string -> Result<unit>
    let desactivateActivityWorkCenters: DesactivateActivityWorkCenters  = toggleActivityWorkCenters Desactivate

    type ActivateActivityWorkCenters = string -> string -> Result<unit>
    let activateActivityWorkCenters: ActivateActivityWorkCenters = toggleActivityWorkCenters Activate

    let deleteActivityWorkCenters () = 
        let ctx = Sql.GetDataContext()
        query {
            for authwc in ctx.Timeentryapp.Activityworkcenteraccess do
                select authwc
        }
        |> Seq.toList
        |> List.iter(fun authwc -> authwc.Delete() )
        trySubmit ctx

   (* ACTIVITY SHOPFLOOR ACCESS FUNCTIONS *)
    type GetActivityShopFloors = string -> string list
    
    let getActivityShopFloors: GetActivityShopFloors =
        fun code -> 
                let ctx = Sql.GetDataContext()         
                query {
                    for activity in ctx.Timeentryapp.Activity do
                        join authActivity in ctx.Timeentryapp.Activityshopflooraccess on (activity.Code = authActivity.Activity)
                        where (activity.Code = code && activity.Active = 1y && authActivity.Active = 1y)
                        select (authActivity.ShopFloor)
                }
                |> Seq.toList

    type InsertActivityShopFloors = ActivityCode -> ShopFloor -> Result<unit>
    let insertActivityShopFloors: InsertActivityShopFloors =
        fun activity shopfloor ->
            let (ActivityCode code) = activity
            let (ShopFloor authshopfloor)  =  shopfloor
            let ctx             = Sql.GetDataContext() 
            let auth = ctx.Timeentryapp.Activityshopflooraccess.Create()
            auth.Activity       <- code
            auth.ShopFloor     <- authshopfloor
            auth.Active         <- 1y

            trySubmit ctx

    type ToggleActivityShopFloors = Activation -> string -> string -> Result<unit>
    let toggleActivityShopFloors: ToggleActivityShopFloors =
        fun activation code shopfloor -> 
            let ctx = Sql.GetDataContext()
            let current, future = flags activation
            let dbsOpt = 
                query {
                    for authwc in ctx.Timeentryapp.Activityshopflooraccess do
                        where (authwc.Activity = code && authwc.ShopFloor = shopfloor && authwc.Active = current)
                        select authwc }
                |> Seq.tryHead

            match dbsOpt with
                | Some dbs -> 
                    dbs.Active <- future
                    try 
                        ctx.SubmitUpdates()
                        |> Success
                    with
                        ex -> Failure ex.Message
                | None -> Failure <| sprintf "Authorization for activity \'%s\' on shopfloor %s is missing or %s." code shopfloor  activation.State

    type DesactivateActivityShopFloors = string -> string -> Result<unit>
    let desactivateActivityShopFloors: DesactivateActivityShopFloors  = toggleActivityShopFloors Desactivate

    type ActivateActivityShopFloors = string -> string -> Result<unit>
    let activateActivityShopFloors: ActivateActivityShopFloors = toggleActivityShopFloors Activate

    let deleteActivityshopfloors () = 
        let ctx = Sql.GetDataContext()
        query {
            for authwc in ctx.Timeentryapp.Activityworkcenteraccess do
                select authwc
        }
        |> Seq.toList
        |> List.iter(fun authwc -> authwc.Delete() )
        trySubmit ctx

    (* ACTIVITY FUNCTIONS  *)
    type GetActivityCodes = unit -> string list
    let getActivityCodes: GetActivityCodes =
        fun () -> 
            let ctx = Sql.GetDataContext()
            query {
                for activity in ctx.Timeentryapp.Activity do
                    where (activity.Active = 1y)
                    select activity.Code
            }
            |> Seq.toList

    ///Insert new activity in DB
    type InsertActivity = Activity -> Result<unit>
    let insertActivity: InsertActivity =
        fun activity -> 
            let ctx = Sql.GetDataContext()
            let ac = ctx.Timeentryapp.Activity.Create()
            let dbac = toDBActivity activity
            ac.Code             <- dbac.Code
            ac.Site             <- dbac.Site
            ac.AccessAll        <- boolToSbyte dbac.AccessAll
            ac.ExtraInfo        <- dbac.ExtraInfo
            ac.RecordLevel      <- dbac.RecordLevel
            ac.TimeType         <- dbac.TimeType
            ac.IsLinked         <- boolToSbyte dbac.isLinked
            ac.LinkedActivity   <- dbac.LinkedActivity
            ac.Active           <- 1y
            
            try 
                ctx.SubmitUpdates()
                |> Success
            with
            | ex -> Failure <| sprintf "%s" ex.Message

    type GetActivity = string -> Result<DBActivity>

    let private getActivityEntity code = 
            let ctx = Sql.GetDataContext() 
            query {
                for activity in ctx.Timeentryapp.Activity do
                    where (activity.Code = code && activity.Active = 1y)
                    select activity
            }
            |> Seq.toList
            |> onlyOne "Activity" code

    let getActivity: GetActivity =
            getActivityEntity
            >> Result.map (fun (record) ->
                let accessList = 
                    match record.RecordLevel with
                        | "workcenter"  -> getActivityWorkCenters record.Code
                        | "shopfloor"   -> getActivityShopFloors record.Code
                        | _             -> []

                {
                    DBActivity.Site     = record.Site
                    Code                = record.Code
                    AccessAll           = sbyteTobool record.AccessAll
                    AccessList          = accessList
                    RecordLevel         = record.RecordLevel
                    isLinked            = sbyteTobool record.IsLinked
                    TimeType            = record.TimeType
                    ExtraInfo           = record.ExtraInfo
                    LinkedActivity      = record.LinkedActivity
                })

    type UpdateActivity = Activity -> Result<unit>
    
    let updateActivity: UpdateActivity =
        fun activity -> 
            let (ActivityCode code) = activity.Code 
            let ctx = Sql.GetDataContext()
            let activityRes = getActivityEntity code
            let dbac = toDBActivity activity
            match activityRes with
                | Success ac -> 
                        ac.Site             <- dbac.Site
                        ac.AccessAll        <- boolToSbyte dbac.AccessAll
                        ac.ExtraInfo        <- dbac.ExtraInfo
                        ac.RecordLevel      <- dbac.RecordLevel
                        ac.TimeType         <- dbac.TimeType
                        ac.IsLinked         <- boolToSbyte dbac.isLinked
                        ac.LinkedActivity   <- dbac.LinkedActivity
                        ac.Active           <- 1y
                        try 
                            ctx.SubmitUpdates()
                            |> Success
                        with
                        | ex -> Failure <| sprintf "%s" ex.Message
                
                | Failure msg -> Failure msg
            

    type ToggleActivity = Activation -> string -> Result<unit>
    let toggleActivity: ToggleActivity =
        fun activation ev -> 
            let ctx = Sql.GetDataContext()
            let current, future = flags activation
            let dbsOpt = 
                query {
                    for activity in ctx.Timeentryapp.Activity do
                        where (activity.Code = ev && activity.Active = current)
                        select activity }
                |> Seq.tryHead

            match dbsOpt with
                | Some dbs -> 
                    dbs.Active <- future
                    try 
                        ctx.SubmitUpdates()
                        |> Success
                    with
                        ex -> Failure ex.Message
                | None -> Failure <| sprintf "Activity \'%s\' is missing or %s." ev activation.State

    type DesactivateActivity = string -> Result<unit>
    let desactivateActivity: DesactivateActivity  = toggleActivity Desactivate

    type ActivateActivity = string -> Result<unit>
    let activateActivity: ActivateActivity = toggleActivity Activate

    let deleteActivities () = 
        let ctx = Sql.GetDataContext()
        query {
            for activity in ctx.Timeentryapp.Activity do
                select activity
        }
        |> Seq.toList
        |> List.iter(fun activity -> activity.Delete() )
        trySubmit ctx

    (* ActivityInfo Functions *)

    ///Insert new workcenter in DB and return Id of inserted record if it succeded
    type InsertActivityInfo = ActivityInfo -> Result<uint32>
    let insertActivityInfo: InsertActivityInfo =
        fun activityInfo  ->
            let ctx = Sql.GetDataContext()
            let ac = ctx.Timeentryapp.Activityinfo.Create()
            let dbac = toDBActivityInfo activityInfo
            ac.Activity <- dbac.Activity
            ac.Machine  <- dbac.Machine
            ac.Cause    <- dbac.Cause
            ac.Solution <- dbac.Solution
            ac.Comments <- dbac.Comments
            ac.Active   <- 1y
            
            try 
                ctx.SubmitUpdates()
                //Return id of inserted record
                ac.ActivityInfoId
                |> Success
            with
            | ex -> Failure <| sprintf "%s" ex.Message

    type GetActivityInfo = ActivityInfoId -> Result<DBActivityInfo>

    let private getActivityInfoEntity = 
        fun activityInfoId -> 
            let ctx = Sql.GetDataContext()
            query {
                    for activityInfo in ctx.Timeentryapp.Activityinfo do
                        where (activityInfo.ActivityInfoId = activityInfoId && activityInfo.Active = 1y)
                        select activityInfo
                }
                |> Seq.toList
                |> onlyOne "ActivityInfo" (string activityInfoId)
   
    let getActivityInfo: GetActivityInfo = 
            getActivityInfoEntity
            >> Result.map (fun (activityinfo) -> activityinfo.MapTo<DBActivityInfo>() )
    
    type UpdatActivityInfo = ActivityInfoId -> ActivityInfo -> Result<unit>

    let updateActivityInfo: UpdatActivityInfo =
        fun actInfoId activityInfo ->
            let ctx = Sql.GetDataContext()
            let dbac = toDBActivityInfo activityInfo
            
            let activityInfoRes = getActivityInfoEntity actInfoId

            match activityInfoRes with
                | Success ac -> 
                        ac.Activity <- dbac.Activity
                        ac.Machine  <- dbac.Machine
                        ac.Cause    <- dbac.Cause
                        ac.Solution <- dbac.Solution
                        ac.Comments <- dbac.Comments
                        trySubmit ctx

                | Failure msg -> Failure msg

    let deleteActivityInfo () = 
        let ctx = Sql.GetDataContext()
        query {
            for activityInfo in ctx.Timeentryapp.Activityinfo do
                select activityInfo
        }
        |> Seq.toList
        |> List.iter(fun activityInfo -> activityInfo.Delete() )
        trySubmit ctx

    (* workOrderInfo Functions *)

    type GetWorkOrderCodes = unit -> string list
    let getWorkOrderCodes: GetWorkOrderCodes =
        fun () -> 
            let ctx = Sql.GetDataContext()
            query {
                for workorder in ctx.Timeentryapp.Workorderinfo do
                    where (workorder.Active = 1y)
                    select workorder.WorkOrder
            }
            |> Seq.toList

    //Insert new workcenter in DB
    type InsertWorkOrder = WorkOrderInfo -> Result<unit>
    let insertWorkOrderInfo workOrderInfo  =
        let ctx = Sql.GetDataContext()
        let wo = ctx.Timeentryapp.Workorderinfo.Create()
        let dbwo = toDBWorkOrderInfo workOrderInfo
        let (WorkCenter wc) = workOrderInfo.WorkCenter
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


    type GetWorkOrder = string -> Result<DBWorkOrderInfo>
    let getWorkOrder: GetWorkOrder =
        fun wo ->
            //let (WorkOrder wo) = workOrder 
            let ctx = Sql.GetDataContext()
            query {
                for workorder in ctx.Timeentryapp.Workorderinfo do
                    where (workorder.WorkOrder = wo && workorder.Active = 1y)
                    select workorder
            }
            |> Seq.map(fun workorder -> workorder.MapTo<DBWorkOrderInfo>() )
            |> Seq.toList
            |> onlyOne "WorkOrder" wo
    
    type UpdateworkOrderInfo = WorkOrderInfo -> Result<unit>
    let updateWorkOrderInfo: UpdateworkOrderInfo =
        fun workOrderInfo -> 
            let (WorkOrder wo) =  workOrderInfo.WorkOrder
            let ctx = Sql.GetDataContext()
            let workOrderInfoRes = 
                query {
                    for workorder in ctx.Timeentryapp.Workorderinfo do
                        where (workorder.WorkOrder = wo && workorder.Active = 1y)
                        select workorder
                }
                |> Seq.toList
                |> onlyOne "WokOrder" wo

            let dbwo = toDBWorkOrderInfo workOrderInfo
            match workOrderInfoRes with
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
            for workorder in ctx.Timeentryapp.Workorderinfo do
                select workorder
        }
        |> Seq.toList
        |> List.iter(fun workorder -> workorder.Delete() )
        
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
                    let dbWorkOrderRes = 
                        record.WorkOrder  
                        |> Option.map getWorkOrder
                        |> unwrapResOpt
                    let dbActivityRes = 
                        record.ActivityInfoId  
                        |> Option.map getActivityInfo
                        |> unwrapResOpt

                    {
                        DBTimeRecord.Site   = record.Site
                        ShopFloor           = record.Shopfloor
                        WorkCenter          = record.WorkCenter
                        TimeType            = record.TimeType
                        StartTime           = record.StartTime
                        EndTime             = record.EndTime
                        TimeHr              = record.TimeHr
                        NbPeople            = record.NbPeople
                        Attribution         = record.Attribution
                        WorkOrderEntry      = dbWorkOrderRes
                        ActivityEntry       = dbActivityRes
                        Status              = record.RecordStatus
                    })
            |> Seq.toList
            |> onlyOne "TimeRecord" (string id)

    let insertDBTimeRecord : Sql.dataContext -> ActivityInfoId option -> DBTimeRecord -> Result<uint32> = 
        fun ctx actInfoId record -> 
            let wo = Option.map(fun dbwo -> dbwo.WorkOrder) record.WorkOrderEntry
            let tr = ctx.Timeentryapp.Timerecord.Create()
            tr.Site             <- record.Site
            tr.Shopfloor        <- record.ShopFloor
            tr.WorkCenter       <- record.WorkCenter
            tr.TimeType         <- record.TimeType
            tr.StartTime        <- record.StartTime
            tr.EndTime          <- record.EndTime
            tr.TimeHr           <- record.TimeHr
            tr.Attribution      <- record.Attribution
            tr.NbPeople         <- record.NbPeople
            tr.WorkOrder        <- wo
            tr.ActivityInfoId   <- actInfoId
            tr.RecordStatus     <- record.Status
            tr.LastUpdate       <- System.DateTime.Now
            tr.Active           <- 1y
            try 
                        ctx.SubmitUpdates()
                        tr.TimeRecordId
                        |> Success
            with
            | ex -> Failure <| sprintf "%s" ex.Message

    type InsertTimeRecord = TimeRecord -> Result<uint32>

    let insertTimeRecord : InsertTimeRecord =
        fun timeRecord ->
            let ctx = Sql.GetDataContext()
            match timeRecord.Attribution with
                | WorkOrderEntry workOrderInfo ->
                    
                    let dbrecords = toDBTimeRecord timeRecord
                    dbrecords
                    |> insertDBTimeRecord ctx (None)

                | ActivityEntry activityInfo -> 
                    let dbrecords = toDBTimeRecord timeRecord
                    dbrecords
                    |> fun record -> 
                            insertActivityInfo activityInfo 
                            //Add User in EventEntry to be sure to get the correct ID!
                            //|> map lastEventEntryId
                            |> bind (fun id -> insertDBTimeRecord ctx (Some id) record)
                    
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

    type DesactivateUserAuth = string -> string -> Result<unit>
    let desactivateUserAuth: DesactivateUserAuth  = toggleUserAuth Desactivate

    type ActivateUserAuth = string -> string -> Result<unit>
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


    type GetUser = string -> Result<DBUserInfo>

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
                            Login       = user.Login
                            Name        = user.UserRealName
                            Level       = user.AuthLevel
                            AllSites    = sbyteTobool user.AllSites
                            SiteList    = sites
                }
            )
            |> Seq.toList
            |> onlyOne "User" login

    //Insert new user in DB
    type InsertUser = UserInfo -> Result<unit>

    let insertUser: InsertUser =
        fun user -> 
            let ctx     = Sql.GetDataContext()
            let usr     = ctx.Timeentryapp.User.Create()
            let dbUser    = toDBUserInfo user
            usr.Login         <- dbUser.Login
            usr.UserRealName  <- dbUser.Name
            usr.AuthLevel     <- dbUser.Level
            usr.AllSites      <- boolToSbyte dbUser.AllSites
            usr.Active        <- 1y
            try 
                ctx.SubmitUpdates()
                |> Success
            with
            | ex -> Failure <| sprintf "%s" ex.Message

    type UpdateUser = UserInfo -> Result<unit>

    let updateUser: UpdateUser =
        fun newuser ->
            let ctx = Sql.GetDataContext()
            let (Login login) = newuser.Login
            let dbus = toDBUserInfo newuser
            
            let userRes = 
                query {
                    for user in ctx.Timeentryapp.User do
                        where (user.Login = login  && user.Active = 1y)
                        select user
                }
                |> Seq.toList
                |> onlyOne "Login" login

            match userRes with
                | Success user -> 
                        user.UserRealName   <- dbus.Name
                        user.AllSites       <- boolToSbyte dbus.AllSites
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
                | None -> Failure <| sprintf "User \'%s\' is missing or %s." login  activation.State

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
