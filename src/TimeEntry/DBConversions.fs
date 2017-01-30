namespace TimeEntry
    open System
    open TimeEntry.Result
    open TimeEntry.ConstrainedTypes
    open TimeEntry.DomainTypes
    open TimeEntry.Constructors

    module DBConversions =
            (* SHOPFLOOR CONVERSION FUNCTIONS *)
            module ShopFloor =

                type DBShopFloorInfo = 
                    {
                        Site         : string
                        ShopFloor    : string
                    }
                
                let toDB : ShopFloorInfo -> DBShopFloorInfo =
                    fun sfinfo -> 
                        let (Site (String3 site) )     = sfinfo.Site
                        let (ShopFloor (String5 sf) )  = sfinfo.ShopFloor
                        { Site = site; ShopFloor = sf }
            
                let fromDB 
                    sites
                    shopfloors
                    (sfinfo: DBShopFloorInfo) =
                        let siteRes = validateSite sites sfinfo.Site
                        let shopfloorRes = validateShopFloor shopfloors sfinfo.ShopFloor
                        createShopFloorInfo 
                        <!> siteRes
                        <*> shopfloorRes
                
            (* WORKCENTER CONVERSION FUNCTIONS *)
            module WorkCenter =
                open ShopFloor

                type DBWorkCenterInfo =
                    {
                        WorkCenter      : string
                        ShopFloorInfo   : DBShopFloorInfo
                        StartHour       : uint32
                        EndHour         : uint32
                    }
                
                let toDB: WorkCenterInfo -> DBWorkCenterInfo =
                    fun wcInfo -> 
                        let (WorkCenter (String5 wc)) = wcInfo.WorkCenter
                        let sf              = ShopFloor.toDB wcInfo.ShopFloorInfo
                        let (Hour startH)   = wcInfo.StartHour
                        let (Hour endH)     = wcInfo.EndHour
                        { WorkCenter = wc; ShopFloorInfo = sf; StartHour = startH; EndHour = endH}
                
                let fromDB 
                    fromDBShopfloorInfo
                    workcenters
                    (wcInfo: DBWorkCenterInfo) =
                        let shopfloorInfoRes = fromDBShopfloorInfo wcInfo.ShopFloorInfo 
                        let workcenterRes = validateWorkCenter workcenters wcInfo.WorkCenter
                        let starthourRes = validateHour wcInfo.StartHour
                        let endhourRes   = validateHour wcInfo.EndHour
                        createWorkCenterInfo
                        <!> shopfloorInfoRes
                        <*> workcenterRes
                        <*> starthourRes
                        <*> endhourRes 
                        
            (* MACHINE CONVERSION FUNCTIONS *)
            module Machine =
                open ShopFloor

                type DBMachineInfo =
                    {
                        ShopFloorInfo   : DBShopFloorInfo
                        Machine         : string
                    }

                let toDB
                    (machInfo: MachineInfo) =
                    let (Machine (String10 mach)) = machInfo.Machine
                    let sf                        = ShopFloor.toDB machInfo.ShopFloorInfo
                    { DBMachineInfo.Machine = mach; ShopFloorInfo = sf}
            
                let fromDB
                    fromDBShopfloorInfo
                    machines
                    sites
                    shopfloors
                    (machInfo: DBMachineInfo) =
                    let machineRes = validateMachine machines machInfo.Machine
                    let shopfloorInfoRes = fromDBShopfloorInfo sites shopfloors machInfo.ShopFloorInfo 
                    createMachineInfo
                    <!> machineRes
                    <*> shopfloorInfoRes

            (* ACTIVITY CONVERSION FUNCTIONS *)
            module Activity = 
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

                let toDB (activity: Activity) =
                    let (ActivityCode (String4 code) )  = activity.Code
                    let (Site (String3 s))              = activity.Site
                    let timetype = activity.TimeType.ToString()
                    let level, accessall, accessList = 
                        match activity.RecordLevel with
                            | ShopFloorLevel (AllShopFloors)    -> ("shopfloor", true, [])
                            | ShopFloorLevel (ShopFloorList sf) -> 
                                let sfcodes = sf |> List.map(fun (ShopFloor (String5 s)) -> s) 
                                ("shopfloor", false, sfcodes)
                            | WorkCenterLevel (AllWorkCenters)  -> ("workcenter", true, [])
                            | WorkCenterLevel (WorkCenterList wc) -> 
                                let wccodes = wc |> List.map(fun (WorkCenter (String5 w)) -> w) 
                                ("shopfloor", false, wccodes)
                    let islinked, linkedact = 
                        match activity.ActivityLink with
                            | Linked (ActivityCode (String4 act))   -> true, Some act
                            | NotLinked                             -> false,None

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

                let fremDB
                    sites 
                    shopfloors
                    workcenters
                    activities 
                    (dbActivity: DBActivity) =
                    let siteRes     = validateSite sites dbActivity.Site
                    let codeRes     = validateActivityCode activities dbActivity.Code
                    let timetypeRes = validateTimeType dbActivity.TimeType
                    let extraInfoRes = validateExtraInfo dbActivity.ExtraInfo

                    let levelRes = 
                        match dbActivity.RecordLevel with
                            | "shopfloor" -> 
                                let sfRes  = dbActivity.AccessList |> List.map (validateShopFloor shopfloors) |> sequence
                                validateShopFloorAccess 
                                <!> (Success dbActivity.AccessAll) 
                                <*> sfRes
                                |> flatten
                                |> ( Result.map ShopFloorLevel )
                            | "workshop" -> 
                                let wcRes  = dbActivity.AccessList |> List.map (validateWorkCenter workcenters) |> sequence
                                validateWorkCenterAccess 
                                <!> (Success dbActivity.AccessAll) 
                                <*> wcRes
                                |> flatten
                                |> ( Result.map WorkCenterLevel )
                            | lvl -> Failure <| sprintf "Unexpected level : %s" lvl

                    let activityLinkRes = validateActivityLink (validateActivityCode activities) dbActivity.isLinked dbActivity.LinkedActivity

                    validateActivity
                    <!> siteRes
                    <*> codeRes
                    <*> levelRes
                    <*> timetypeRes
                    <*> activityLinkRes

            (* ACTIVITY INFO CONVERSION FUNCTIONS *)
            module ActivityInfo = 
                type DBActivityInfo =
                    {
                        Activity    : string 
                        Machine     : string option
                        Cause       : string option
                        Solution    : string option
                        Comments    : string option
                    }

                let toDB = 
                    function
                        | Detailed (act, info) -> 
                            let (ActivityCode (String4 code))   = act
                            let (Machine (String10 machine))    = info.Machine
                            let (String50 cause)                = info.Cause
                            let (String50 solution)             = info.Solution
                            let (String200 comments)            = info.Comments
                            {   Activity    = code
                                Machine     = Some machine
                                Cause       = Some cause
                                Solution    = Some solution 
                                Comments    = Some comments
                            }
                        | Normal (act) -> 
                            let (ActivityCode (String4 code)) = act
                            {   Activity    = code
                                Machine     = None
                                Cause       = None
                                Solution    = None 
                                Comments    = None
                            }

                let fromDB  
                    activities
                    machines
                    (activityInfo: DBActivityInfo) =
                    let activityRes = validateActivityCode activities activityInfo.Activity
    
                    let machineRes   = 
                            activityInfo.Machine
                            |> failIfMissing "Machine missing"
                            |> bind (validateMachine machines)

                    let causeRes     = (failIfMissing "Cause missing"    >=> validateCause) activityInfo.Cause
                    let solutionRes  = (failIfMissing "Solution missing" >=> validateSolution) activityInfo.Solution
                    let commentsRes  = (failIfMissing "Comments missing" >=> validateComments) activityInfo.Comments
                    
                    let activityDetailsResOpt = 
                        validateActivityDetails 
                        <!> machineRes 
                        <*> causeRes 
                        <*> solutionRes 
                        <*> commentsRes
                        |> Result.map Some

                    validateActivityEntry <!> activityRes <*> activityDetailsResOpt
                    |> flatten
            (* USER CONVERSION FUNCTIONS *)
            module UserInfo = 
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
                
                let toDB  (user : UserInfo) = 
                    let (Login (String8 login))   = user.Login
                    let (UserName (String50 name)) = user.Name
                    let (Password (String50 pwd))  = user.Password
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
                            let sites = l |> List.map (fun (Site (String3 s)) -> s)
                            {
                                Login       = login
                                Name        = name
                                Password    = pwd
                                Level       = level
                                AllSites    = false
                                SiteList    = sites
                            }  

                let fromDB  
                    sites
                    logins
                    (dbuser : DBUserInfo) = 
                        let loginRes  = validateLogin logins dbuser.Login
                        let nameRes   = createUserName dbuser.Name
                        let passworRes = createPassword dbuser.Password
                        let sitesRes  = dbuser.SiteList |> List.map (validateSite sites) |> sequence
                        let accessRes = 
                            validateSiteAccess 
                            <!> (Success dbuser.AllSites) 
                            <*> sitesRes
                            |> flatten
                        let levelRes  = validateAuthLevel dbuser.Level
                        validateUser  
                        <!> loginRes
                        <*> nameRes
                        <*> passworRes
                        <*> accessRes
                        <*> levelRes

            (* WORK ORDER CONVERSION FUNCTIONS *)
            module WorkOrderInfo = 
                type DBWorkOrderInfo =
                    {
                        WorkOrder : string
                        WorkCenter : string
                        ItemCode  : string
                        TotalMachineTimeHr : float32
                        TotalLabourTimeHr : float32
                        WorkOrderStatus    : string
                    }

                let toDB (wo: WorkOrderInfo) =
                    let (WorkOrder (String10 strWo))     = wo.WorkOrder
                    let (WorkCenter (String5 strWc))     = wo.WorkCenter
                    let (ItemCode (String6 strItem))     = wo.ItemCode
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

                let fromDB
                    workOrders
                    workCenters
                    itemCodes
                    (wo: DBWorkOrderInfo) =
                    let workOrderRes = validateWorkOrder workOrders wo.WorkOrder
                    let workCenterRes = validateWorkCenter workCenters wo.WorkCenter
                    let itemCodeRes  = validateItemCode itemCodes wo.ItemCode
                    let totalMachineTimeHrRes = validateTimeHr wo.TotalMachineTimeHr
                    let totalLabourTimeHrRes = validateTimeHr wo.TotalLabourTimeHr
                    let statusRes = validateWorkOrderStatus wo.WorkOrderStatus
                    validateWorkOrderInfo 
                    <!> workOrderRes
                    <*> workCenterRes
                    <*> itemCodeRes
                    <*> totalMachineTimeHrRes
                    <*> totalLabourTimeHrRes
                    <*> statusRes  

            (* TIME RECORD CONVERSION FUNCTIONS *)
            module TimeRecord =
                open ActivityInfo
                open WorkOrderInfo

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
                
                let private attributionToString =
                    function
                        | WorkOrderEntry wo     -> "workorder"
                        | ActivityEntry act     -> "activity"

                let private updateAttribution attribution timeRecord = 
                    match attribution with
                        | WorkOrderEntry wo ->
                            { timeRecord with WorkOrderEntry = WorkOrderInfo.toDB wo |> Some}
                        | ActivityEntry act ->
                            { timeRecord with ActivityEntry = ActivityInfo.toDB act |> Some}

                let private recordStatusToString = 
                    function
                        | Entered   -> "entered"
                        | Validated -> "validated"

                ///Function to convert one Domain Record into records to insert into Database
                let toDB (time : TimeRecord) = 
                    let (Site (String3 site)) = time.Site
                    let (ShopFloor (String5 shopfloor)) = time.ShopFloor
                    
                    let workcenter = 
                        match time.WorkCenter with
                            | Some (WorkCenter (String5 wc)) -> Some wc
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

                let fromDB 
                    sites
                    shopfloors
                    workcenters
                    workorders
                    activities
                    machines
                    itemcodes
                    (time: DBTimeRecord) =
                        let siteRes = validateSite sites time.Site
                        let shopFloorRes = validateShopFloor shopfloors time.ShopFloor
                        
                        let workCenterRes = 
                            time.WorkCenter 
                            |> Option.map (validateWorkCenter workcenters)
                            |> switchResOpt
                        
                        let timeTypeRes = validateTimeType time.TimeType
                        let durationRes = validateDuration time.StartTime time.EndTime
                        let nbPeopleRes = validateNbPeople time.NbPeople
                        let statusRes   = validateRecordStatus time.Status


                        match time.WorkOrderEntry, time.ActivityEntry with
                            | Some wo, None -> 
                                let workOrderEntryRes = (WorkOrderInfo.fromDB workorders workcenters itemcodes) wo
                                let attributionRes = Result.map WorkOrderEntry workOrderEntryRes
                                validateTimeRecord
                                <!> siteRes
                                <*> shopFloorRes
                                <*> workCenterRes
                                <*> attributionRes
                                <*> timeTypeRes
                                <*> durationRes
                                <*> statusRes
                                
                            | None, Some act -> 
                                let activityEntryRes = (ActivityInfo.fromDB activities machines) act
                                let attributionRes   = Result.map ActivityEntry activityEntryRes
                                validateTimeRecord
                                <!> siteRes
                                <*> shopFloorRes
                                <*> workCenterRes
                                <*> attributionRes
                                <*> timeTypeRes
                                <*> durationRes
                                <*> statusRes

                            | Some wo, Some ev -> Failure "Both Workorder and Event entry are set."
                            | None, None       -> Failure "Both Workorder and Event entry are missing."