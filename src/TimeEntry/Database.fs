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
                    ExtraInfo       : string
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

                let extrainfo = activity.ExtraInfo.ToString()
                        
                { 
                    Site        = s
                    Code        = code
                    AccessAll   = accessall
                    AccessList  = accessList 
                    RecordLevel = level
                    TimeType    = timetype
                    isLinked    = islinked
                    LinkedActivity  = linkedact
                    ExtraInfo   = extrainfo
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
                let extraInfoRes = createExtraInfo dbActivity.ExtraInfo

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

            type DBActivityInfo =
                {
                    Activity    : string 
                    Machine     : string option
                    Cause       : string option
                    Solution    : string option
                    Comments    : string option
                }

            let toDBActivityInfo = 
                function
                    | Detailed (act, info) -> 
                        let (ActivityCode code) = act
                        let  (Machine machine) = info.Machine
                        let cause   = info.Cause
                        let solution = info.Solution
                        let comments = info.Comments
                        {   Activity    = code
                            Machine     = Some machine
                            Cause       = Some cause
                            Solution    = Some solution 
                            Comments    = Some comments
                        }
                    | Normal (act) -> 
                        let (ActivityCode code) = act
                        {   Activity    = code
                            Machine     = None
                            Cause       = None
                            Solution    = None 
                            Comments    = None
                        }

            let fromDBActivityInfo  
                activities
                machines
                (activityInfo: DBActivityInfo) =
                let activityRes = createActivityCode activities activityInfo.Activity
   
                let machineRes   = 
                        activityInfo.Machine
                        |> failIfMissing "Machine missing"
                        |> bind (createMachine machines)

                let causeRes     = failIfMissing "Cause missing" activityInfo.Cause
                let solutionRes  = failIfMissing "Solution missing" activityInfo.Solution
                let commentsRes  = failIfMissing "Comments missing" activityInfo.Comments
                
                let activityDetailsResOpt = 
                    createActivityDetails 
                    <!> machineRes 
                    <*> causeRes 
                    <*> solutionRes 
                    <*> commentsRes
                    |> Result.map Some

                createActivityEntry <!> activityRes <*> activityDetailsResOpt
                |> flatten
            (* USER CONVERSION FUNCTIONS *)
            type DBUserInfo =
                {
                    Login       : string
                    Name        : string
                    Password    : string
                    Level       : string
                    AllSites    : bool
                    SiteList    : string list
                }
            
            let authLevelToString =
                function
                    | User      -> "user"
                    | KeyUser   -> "keyuser"
                    | Admin     -> "admin"
            
            let toDBUserInfo  (user : UserInfo) = 
                let (Login login)   = user.Login
                let (UserName name) = user.Name
                let (Password pwd)  = user.Password
                let level = authLevelToString user.Level
                match user.SiteAccess with
                    | AllSites -> 
                        {
                            Login       = login
                            Name        = name
                            Password    = pwd
                            Level       = level
                            AllSites    = true
                            SiteList    = []
                        }
                    | SiteList l -> 
                        let sites = l |> List.map (fun (Site s) -> s)
                        {
                            Login       = login
                            Name        = name
                            Password    = pwd
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
                    let levelRes  = createAuthLevel dbuser.Level
                    createUser  
                    <!> loginRes
                    <*> nameRes
                    <*> (Success dbuser.Password)
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

            (* TIME RECORD CONVERSION FUNCTIONS *)
            type DBTimeRecord =
                {
                    Site            : string
                    ShopFloor       : string
                    WorkCenter      : string option
                    TimeType        : string
                    StartTime       : DateTime
                    EndTime         : DateTime
                    TimeHr          : float32
                    NbPeople        : float32
                    Attribution     : string
                    WorkOrderEntry  : DBWorkOrderInfo option
                    ActivityEntry   : DBActivityInfo option
                    Status          : string
                }
            
            let attributionToString =
                function
                    | WorkOrderEntry wo     -> "workorder"
                    | ActivityEntry act     -> "activity"

            let updateAttribution attribution timeRecord = 
                match attribution with
                    | WorkOrderEntry wo ->
                        { timeRecord with WorkOrderEntry = toDBWorkOrderInfo wo |> Some}
                    | ActivityEntry act ->
                        { timeRecord with ActivityEntry = toDBActivityInfo act |> Some}

            let recordStatusToString = 
                function
                    | Entered   -> "entered"
                    | Validated -> "validated"

            ///Function to convert one Domain Record into records to insert into Database
            let toDBTimeRecord  (time : TimeRecord) = 
                let (Site site) = time.Site
                let (ShopFloor shopfloor) = time.ShopFloor
                
                let workcenter = 
                    match time.WorkCenter with
                        | Some (WorkCenter wc) -> Some wc
                        | None -> None 
                            
                let status = recordStatusToString time.Status

                let timetype = time.TimeType.ToString()
                let (TimeHr t)  = time.Duration.ToTimeHr()
                
                {   
                                Site            = site
                                ShopFloor       = shopfloor 
                                WorkCenter      = workcenter 
                                TimeType        = "machine"
                                StartTime       = time.Duration.StartTime
                                EndTime         = time.Duration.EndTime
                                TimeHr          = t
                                NbPeople        = 0.f
                                Attribution     = attributionToString time.Attribution
                                WorkOrderEntry  = None
                                ActivityEntry   = None
                                Status          = status
                }
                |> updateAttribution time.Attribution

            let fromTimeRecordDB 
                sites
                shopfloors
                workcenters
                workorders
                activities
                machines
                itemcodes
                (time: DBTimeRecord) =
                    let siteRes = createSite sites time.Site
                    let shopFloorRes = createShopFloor shopfloors time.ShopFloor
                    
                    let workCenterRes = 
                        time.WorkCenter 
                        |> Option.map (createWorkCenter workcenters)
                        |> switchResOpt
                    
                    let timeTypeRes = createTimeType time.TimeType
                    let durationRes = createDuration time.StartTime time.EndTime
                    let nbPeopleRes = createNbPeople time.NbPeople
                    let statusRes   = createRecordStatus time.Status


                    match time.WorkOrderEntry, time.ActivityEntry with
                        | Some wo, None -> 
                            let workOrderEntryRes = (fromDBWorkOrderInfo workorders workcenters itemcodes) wo
                            let attributionRes = Result.map WorkOrderEntry workOrderEntryRes
                            createTimeRecord
                            <!> siteRes
                            <*> shopFloorRes
                            <*> workCenterRes
                            <*> attributionRes
                            <*> timeTypeRes
                            <*> durationRes
                            <*> statusRes
                            
                        | None, Some act -> 
                            let activityEntryRes = (fromDBActivityInfo activities machines) act
                            let attributionRes   = Result.map ActivityEntry activityEntryRes
                            createTimeRecord
                            <!> siteRes
                            <*> shopFloorRes
                            <*> workCenterRes
                            <*> attributionRes
                            <*> timeTypeRes
                            <*> durationRes
                            <*> statusRes

                        | Some wo, Some ev -> Failure "Both Workorder and Event entry are set."
                        | None, None       -> Failure "Both Workorder and Event entry are missing."

