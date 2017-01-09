namespace TimeEntry
    open System
    open TimeEntry.Result
    open TimeEntry.Option
    open TimeEntry.DomainTypes
    open TimeEntry.Constructors

    module DataBase =
            (* SHOPFLOOR CONVERSION FUNCTIONS *)
            type DBShopFloorInfo = 
                {
                    Site         : string
                    ShopFloor    : string
                }
            
            let toDBShopfloorInfo (sfinfo: ShopFloorInfo) =
                let (Site site)     = sfinfo.Site
                let (ShopFloor sf)  = sfinfo.ShopFloor
                { Site = site; ShopFloor = sf }
         
            let fromDBShopfloorInfo 
                sites
                shopfloors
                (sfinfo: DBShopFloorInfo) =
                    let siteRes = createSite sites sfinfo.Site
                    let shopfloorRes = createShopFloor shopfloors sfinfo.ShopFloor
                    createShopFloorInfo 
                    <!> siteRes
                    <*> shopfloorRes
            
            (* WORKCENTER CONVERSION FUNCTIONS *)
            type DBWorkCenterInfo =
                {
                    WorkCenter      : string
                    ShopFloorInfo   : DBShopFloorInfo
                    StartHour       : uint32
                    EndHour         : uint32
                }
            
            let toDBWorkCenterInfo (wcInfo: WorkCenterInfo) =
                let (WorkCenter wc) = wcInfo.WorkCenter
                let sf              = toDBShopfloorInfo wcInfo.ShopFloorInfo
                let (Hour startH)   = wcInfo.StartHour
                let (Hour endH)     = wcInfo.EndHour
                { WorkCenter = wc; ShopFloorInfo = sf; StartHour = startH; EndHour = endH}
         
            let fromDBWorkCenterInfo 
                fromDBShopfloorInfo
                workcenters
                (wcInfo: DBWorkCenterInfo) =
                    let shopfloorInfoRes = fromDBShopfloorInfo wcInfo.ShopFloorInfo 
                    let workcenterRes = createWorkCenter workcenters wcInfo.WorkCenter
                    let starthourRes = createHour wcInfo.StartHour
                    let endhourRes   = createHour wcInfo.EndHour
                    createWorkCenterInfo
                    <!> shopfloorInfoRes
                    <*> workcenterRes
                    <*> starthourRes
                    <*> endhourRes 
                    
            (* MACHINE CONVERSION FUNCTIONS *)
            type DBMachineInfo =
                {
                    ShopFloorInfo   : DBShopFloorInfo
                    Machine         : string
                }

            let toDBMachineInfo (machInfo: MachineInfo) =
                let (Machine mach) = machInfo.Machine
                let sf             = toDBShopfloorInfo machInfo.ShopFloorInfo
                { Machine = mach; ShopFloorInfo = sf}
         
            let fromDBMachineInfo 
                machines
                sites
                shopfloors
                (machInfo: DBMachineInfo) =
                let machineRes = createMachine machines machInfo.Machine
                let shopfloorInfoRes = fromDBShopfloorInfo sites shopfloors machInfo.ShopFloorInfo 
                createMachineInfo
                <!> machineRes
                <*> shopfloorInfoRes

            (* ACTIVITY CONVERSION FUNCTIONS *)
            type DBActivity =
                {
                    Site            : string
                    Code            : string
                    AccessAll       : bool
                    AccessList      : string list
                    RecordLevel     : string
                    TimeType        : string
                    isLinked        : bool
                    LinkedActivity  : string option
                }

            let toDBActivity (activity: Activity) =
                let (ActivityCode code) = activity.Code
                let (Site s) = activity.Site
                let timetype = activity.TimeType.ToString()
                let level, accessall, accessList = 
                    match activity.RecordLevel with
                        | ShopFloorLevel (AllShopFloors)    -> ("shopfloor", true, [])
                        | ShopFloorLevel (ShopFloorList sf) -> 
                            let sfcodes = sf |> List.map(fun (ShopFloor s) -> s) 
                            ("shopfloor", false, sfcodes)
                        | WorkCenterLevel (AllWorkCenters)  -> ("workcenter", true, [])
                        | WorkCenterLevel (WorkCenterList wc) -> 
                            let wccodes = wc |> List.map(fun (WorkCenter w) -> w) 
                            ("shopfloor", false, wccodes)
                let islinked, linkedact = 
                    match activity.ActivityLink with
                        | Linked (ActivityCode act) -> true, Some act
                        | NotLinked                 -> false,None
                { 
                    Site        = s
                    Code        = code
                    AccessAll   = accessall
                    AccessList  = accessList 
                    RecordLevel = level
                    TimeType    = timetype
                    isLinked    = islinked
                    LinkedActivity  = linkedact
                }

            let fromDBActivity
                sites 
                shopfloors
                workcenters
                activities 
                (dbActivity: DBActivity) =
                let siteRes     = createSite sites dbActivity.Site
                let codeRes     = createActivityCode activities dbActivity.Code
                let timetypeRes = createTimeType dbActivity.TimeType
                
                let levelRes = 
                    match dbActivity.RecordLevel with
                        | "shopfloor" -> 
                            let sfRes  = dbActivity.AccessList |> List.map (createShopFloor shopfloors) |> sequence
                            createShopFloorAccess 
                            <!> (Success dbActivity.AccessAll) 
                            <*> sfRes
                            |> flatten
                            |> ( Result.map ShopFloorLevel )
                        | "workshop" -> 
                            let wcRes  = dbActivity.AccessList |> List.map (createWorkCenter workcenters) |> sequence
                            createWorkCenterAccess 
                            <!> (Success dbActivity.AccessAll) 
                            <*> wcRes
                            |> flatten
                            |> ( Result.map WorkCenterLevel )
                        | lvl -> Failure <| sprintf "Unexpected level : %s" lvl

                let activityLinkRes = createActivityLink (createActivityCode activities) dbActivity.isLinked dbActivity.LinkedActivity

                createActivity
                <!> siteRes
                <*> codeRes
                <*> levelRes
                <*> timetypeRes
                <*> activityLinkRes

            (* USER CONVERSION FUNCTIONS *)
            type DBUserInfo =
                {
                    Login       : string
                    Name        : string
                    Level       : string
                    AllSites    : bool
                    SiteList    : string list
                }
            
            let authLevelToString =
                function
                    | User      -> "user"
                    | KeyUser   -> "keyuser"
                    | Admin     -> "admin"
            
            let toDBUserfo  (user : UserInfo) = 
                let (Login login)   = user.Login
                let (UserName name) = user.Name
                let level = authLevelToString user.Level
                match user.SiteAccess with
                    | AllSites -> 
                        {
                            Login       = login
                            Name        = name
                            Level       = level
                            AllSites    = true
                            SiteList    = []
                        }
                    | SiteList l -> 
                        let sites = l |> List.map (fun (Site s) -> s)
                        {
                            Login       = login
                            Name        = name
                            Level       = level
                            AllSites    = false
                            SiteList    = sites
                        }  

            let fromDBUserInfo  
                sites
                names
                logins
                (dbuser : DBUserInfo) = 
                    let loginRes  = createLogin logins dbuser.Login
                    let nameRes   = createUserName names dbuser.Name
                    let sitesRes  = dbuser.SiteList |> List.map (createSite sites) |> sequence
                    let accessRes = 
                        createSiteAccess 
                        <!> (Success dbuser.AllSites) 
                        <*> sitesRes
                        |> flatten
                    let levelRes  = createLevel dbuser.Level
                    createUser  
                    <!> loginRes
                    <*> nameRes
                    <*> accessRes
                    <*> levelRes

            (* WORK ORDER CONVERSION FUNCTIONS *)
            type DBWorkOrderInfo =
                {
                    WorkOrder : string
                    WorkCenter : string
                    ItemCode  : string
                    TotalMachineTimeHr : float32
                    TotalLabourTimeHr : float32
                    WorkOrderStatus    : string
                }

            let toDBWorkOrderInfo (wo: WorkOrderInfo) =
                let (WorkOrder strWo) = wo.WorkOrder
                let (WorkCenter strWc) = wo.WorkCenter
                let (ItemCode strItem) = wo.ItemCode
                let status =
                    match wo.Status with
                        | Open   -> "open"
                        | Closed -> "closed"

                let (TimeHr totalMachine) = wo.TotalMachineTimeHr
                let (TimeHr totalLabour)  = wo.TotalLabourTimeHr

                { 
                    WorkOrder = strWo; 
                    WorkCenter = strWc;
                    ItemCode = strItem; 
                    TotalMachineTimeHr = totalMachine;
                    TotalLabourTimeHr = totalLabour;
                    WorkOrderStatus = status
                }

            let fromDBWorkOrderInfo
                workOrders
                workCenters
                itemCodes
                (wo: DBWorkOrderInfo) =
                let workOrderRes = createWorkOrder workOrders wo.WorkOrder
                let workCenterRes = createWorkCenter workCenters wo.WorkCenter
                let itemCodeRes  = createItemCode itemCodes wo.ItemCode
                let totalMachineTimeHrRes = createTimeHr wo.TotalMachineTimeHr
                let totalLabourTimeHrRes = createTimeHr wo.TotalLabourTimeHr
                let statusRes = createWorkOrderStatus wo.WorkOrderStatus
                createWorkOrderInfo 
                <!> workOrderRes
                <*> workCenterRes
                <*> itemCodeRes
                <*> totalMachineTimeHrRes
                <*> totalLabourTimeHrRes
                <*> statusRes  


            (* EVENT CONVERSION FUNCTIONS *)
            type DBEvent =
                {
                    Event : string
                    HasInfo : bool
                    AllowZeroPerson : bool
                }

            let toDBEvent =
                function
                    | WithInfo event    -> { Event = event; HasInfo = true; AllowZeroPerson = false }
                    | WithoutInfo event -> { Event = event; HasInfo = false; AllowZeroPerson = false }
                    | ZeroPerson event  ->  { Event = event; HasInfo = false; AllowZeroPerson = true }
            
            let fromDBEvent (event: DBEvent) =
                match event.HasInfo, event.AllowZeroPerson with
                    | true, false   -> Success (WithInfo event.Event)
                    | false, true   -> Success (ZeroPerson event.Event)
                    | false, false  -> Success (WithoutInfo event.Event)
                    | true, true    -> Failure "Event cannot require information and allow zero person."

            type DBEventEntry =
                {
                    Event       : DBEvent 
                    Machine     : string option
                    Cause       : string option
                    Solution    : string option
                    Comments    : string option
                }
            let toDBEventEntry = 
                function
                    | EventWithInfo (ev, info) -> 
                        let  (Machine machine) = info.Machine
                        let cause   = info.Cause
                        let solution = info.Solution
                        let comments = info.Comments
                        {   Event = toDBEvent ev; 
                            Machine = Some machine; 
                            Cause = Some cause; 
                            Solution = Some solution; 
                            Comments = Some comments
                        }
                    | EventWithoutInfo (ev) -> 
                        {   Event = toDBEvent ev; 
                            Machine = None; 
                            Cause = None; 
                            Solution = None; 
                            Comments = None
                        }
                    | EventZeroPerson (ev) ->
                        {   Event = toDBEvent ev; 
                            Machine = None; 
                            Cause = None; 
                            Solution = None; 
                            Comments = None
                        }
                     
            let fromDBEventEntry  
                machines
                (eventEntry: DBEventEntry) =
                let eventRes = fromDBEvent eventEntry.Event
   
                let machineRes   = 
                        eventEntry.Machine
                        |> fromOption "Machine missing"
                        |> bind (createMachine machines)

                let causeRes     = fromOption "Cause missing" eventEntry.Cause
                let solutionRes  = fromOption "Cause missing" eventEntry.Solution
                let commentsRes  = fromOption "Comments missing" eventEntry.Comments
                
                let eventInfoResOpt = 
                            createEventInfo <!> machineRes <*> causeRes <*> solutionRes <*> commentsRes
                            |> Result.map Some
                                         
                createEventEntry <!> eventRes <*> eventInfoResOpt
                |> Result.flatten

            (* TIME RECORD CONVERSION FUNCTIONS *)
            type DBTimeRecord =
                {
                    Site            : string
                    ShopFloor       : string
                    WorkCenter      : string
                    TimeType        : string
                    StartTime       : DateTime
                    EndTime         : DateTime
                    DurationHr      : float32
                    NbPeople        : float32
                    Allocation      : string
                    WorkOrderEntry  : DBWorkOrderInfo option
                    EventEntry      : DBEventEntry option
                    Status          : string
                }
            
            let allocationToString =
                function
                    | WorkOrderEntry wo -> "workorder"
                    | ActivityEntry act     -> "activity"

            let updateAllocation allocation timeRecord = 
                match allocation with
                    | WorkOrderEntry wo ->
                        { timeRecord with WorkOrderEntry = toDBWorkOrderInfo wo |> Some}
                    | ActivityEntry act ->
                        { timeRecord with EventEntry = toDBActivityInfo act |> Some}

            let recordStatusToString = 
                function
                    | Entered   -> "entered"
                    | Validated -> "validated"

            ///Function to convert one Domain Record into records to insert into Database
            let toDBTimeRecord  (time : TimeRecord) = 
                let (Site site) = time.Site
                let (ShopFloor shopfloor) = time.ShopFloor
                let (WorkCenter workcenter) = time.WorkCenter
                let status = recordStatusToString time.Status

                match time.TimeEntry with
                    //List of one record: Machine time
                    | MachineOnly duration ->
                        let machineRecord = {   
                            Site            = site
                            ShopFloor       = shopfloor 
                            WorkCenter      = workcenter 
                            TimeType        = "machine"
                            StartTime       = duration.StartTime
                            EndTime         = duration.EndTime
                            DurationHr      = float32 duration.ToHr
                            NbPeople        = 0.f
                            Allocation      = allocationToString time.Allocation
                            WorkOrderEntry  = None
                            EventEntry      = None
                            Status          = status
                             }
                        let machineRecord' = updateAllocation time.Allocation machineRecord
                        [machineRecord']

                    //List of two records: Machine & Labour time
                    | MachineAndLabour (duration, nbPeople) ->
                        let (NbPeople nb) = nbPeople
                        let machineRecord = 
                            { 
                            Site        = site
                            ShopFloor   = shopfloor 
                            WorkCenter  = workcenter
                            TimeType    = "machine" 
                            StartTime   = duration.StartTime
                            EndTime     = duration.EndTime
                            DurationHr  = float32 duration.ToHr
                            NbPeople    = 0.f
                            Allocation  = allocationToString time.Allocation
                            WorkOrderEntry = None
                            EventEntry  = None
                            Status      =  status }
                        let labourRecord = {machineRecord with TimeType = "labour"; NbPeople = nb}

                        [machineRecord; labourRecord] 
                        |> List.map (updateAllocation time.Allocation)

                    //List of one record: Labour time
                    | LabourOnly (duration, nbPeople) -> 
                        let (NbPeople nb) = nbPeople
                        let labourRecord = 
                            { 
                            Site        = site
                            ShopFloor   = shopfloor 
                            WorkCenter  = workcenter
                            TimeType    = "labour" 
                            StartTime   = duration.StartTime
                            EndTime     = duration.EndTime
                            DurationHr  = float32 duration.ToHr
                            NbPeople    = nb
                            Allocation  = allocationToString time.Allocation
                            WorkOrderEntry = None
                            EventEntry  = None
                            Status      =  status }
                        let labourRecord' = updateAllocation time.Allocation labourRecord
                        [labourRecord']    
            
            let fromTimeRecordDB 
                sites
                shopfloors
                workcenters
                workorders
                machines
                itemcodes
                (time: DBTimeRecord) =
                    let siteRes = createSite sites time.Site
                    let shopFloorRes = createShopFloor shopfloors time.ShopFloor
                    let workCenterRes = createWorkCenter workcenters time.WorkCenter
                    
                    let timeTypeRes = createTimeType time.TimeType
                    let durationRes = createDuration time.StartTime time.EndTime
                    let nbPeopleRes = createNbPeople time.NbPeople

                    let timeEntryRes = createTimeEntry <!> timeTypeRes <*> nbPeopleRes <*> durationRes

                    match time.WorkOrderEntry, time.EventEntry with
                        | Some wo, None -> 
                            let workOrderEntryRes = (fromDBWorkOrderEntry workorders workcenters itemcodes) wo
                            let allocationRes = Result.map WorkOrderEntry workOrderEntryRes
                            createTimeRecord
                            <!> siteRes
                            <*> shopFloorRes
                            <*> workCenterRes
                            <*> allocationRes
                            <*> timeEntryRes
                        | None, Some ev -> 
                            let eventEntryRes = (fromDBEventEntry machines) ev
                            let allocationRes = Result.map EventEntry eventEntryRes
                            createTimeRecord
                            <!> siteRes
                            <*> shopFloorRes
                            <*> workCenterRes
                            <*> allocationRes
                            <*> timeEntryRes
                        | Some wo, Some ev -> Failure "Both Workorder and Event entry are set."
                        | None, None       -> Failure "Both Workorder and Event entry are missing."

