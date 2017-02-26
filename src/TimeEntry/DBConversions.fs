namespace TimeEntry

    module DBConversions =
        open System
        open TimeEntry.Result
        open TimeEntry.ConstrainedTypes
        open TimeEntry.DomainTypes
        open TimeEntry.Constructors
        
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
                        let siteRes = Site.validate sites sfinfo.Site
                        let shopfloorRes = ShopFloor.validate shopfloors sfinfo.ShopFloor
                        ShopFloorInfo.create 
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
                        let workcenterRes = WorkCenter.validate workcenters wcInfo.WorkCenter
                        let starthourRes = Hour.validate wcInfo.StartHour
                        let endhourRes   = Hour.validate wcInfo.EndHour
                        WorkCenterInfo.create
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
                    let machineRes = Machine.validate machines machInfo.Machine
                    let shopfloorInfoRes = fromDBShopfloorInfo sites shopfloors machInfo.ShopFloorInfo 
                    MachineInfo.create
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
                            | RecordLevel.ShopFloor (ShopFloorAccess.All)    -> ("shopfloor", true, [])
                            | RecordLevel.ShopFloor (ShopFloorAccess.List sf) -> 
                                let sfcodes = sf |> List.map(fun (ShopFloor (String5 s)) -> s) 
                                ("shopfloor", false, sfcodes)
                            | RecordLevel.WorkCenter (WorkCenterAccess.All)  -> ("workcenter", true, [])
                            | RecordLevel.WorkCenter (WorkCenterAccess.List wc) -> 
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
                    let siteRes     = Site.validate sites dbActivity.Site
                    let codeRes     = ActivityCode.validate activities dbActivity.Code
                    let timetypeRes = TimeType.validate dbActivity.TimeType
                    let extraInfoRes = ExtraInfo.validate dbActivity.ExtraInfo

                    let levelRes = 
                        match dbActivity.RecordLevel with
                            | "shopfloor" -> 
                                let sfRes  = dbActivity.AccessList |> List.map (ShopFloor.validate shopfloors) |> sequence
                                ShopFloorAccess.validate 
                                <!> (Success dbActivity.AccessAll) 
                                <*> sfRes
                                |> flatten
                                |> ( Result.map RecordLevel.ShopFloor )
                            | "workcenter" -> 
                                let wcRes  = dbActivity.AccessList |> List.map (WorkCenter.validate workcenters) |> sequence
                                WorkCenterAccess.validate
                                <!> (Success dbActivity.AccessAll) 
                                <*> wcRes
                                |> flatten
                                |> ( Result.map RecordLevel.WorkCenter )
                            | lvl -> Failure <| sprintf "Unexpected level : %s" lvl

                    let activityLinkRes = ActivityLink.validate (ActivityCode.validate activities) dbActivity.isLinked dbActivity.LinkedActivity

                    Activity.validate
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
                    let activityRes = ActivityCode.validate activities activityInfo.Activity
    
                    let machineRes   = 
                            activityInfo.Machine
                            |> failIfMissing "Machine missing"
                            |> bind (Machine.validate machines)

                    let causeRes     = (failIfMissing "Cause missing"    >=> Cause.validate) activityInfo.Cause
                    let solutionRes  = (failIfMissing "Solution missing" >=> Solution.validate) activityInfo.Solution
                    let commentsRes  = (failIfMissing "Comments missing" >=> Comments.validate) activityInfo.Comments
                    
                    let activityDetailsResOpt = 
                        ActivityDetails.validate
                        <!> machineRes 
                        <*> causeRes 
                        <*> solutionRes 
                        <*> commentsRes
                        |> Result.map Some

                    ActivityInfo.validate <!> activityRes <*> activityDetailsResOpt
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
                        | Viewer    -> "viewer"
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
                        let loginRes  = Login.validate logins dbuser.Login
                        let nameRes   = UserName.create dbuser.Name
                        let passworRes = Password.create dbuser.Password
                        let sitesRes  = dbuser.SiteList |> List.map (Site.validate sites) |> sequence
                        let accessRes = 
                            SiteAccess.validate 
                            <!> (Success dbuser.AllSites) 
                            <*> sitesRes
                            |> flatten
                        let levelRes  = AuthLevel.validate dbuser.Level
                        UserInfo.validate  
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
                    let workOrderRes            = WorkOrder.validate workOrders wo.WorkOrder
                    let workCenterRes           = WorkCenter.validate workCenters wo.WorkCenter
                    let itemCodeRes             = ItemCode.validate itemCodes wo.ItemCode
                    let totalMachineTimeHrRes   = TimeHr.validate wo.TotalMachineTimeHr
                    let totalLabourTimeHrRes    = TimeHr.validate wo.TotalLabourTimeHr
                    let statusRes               = WorkOrderStatus.validate wo.WorkOrderStatus
                    WorkOrderInfo.validate 
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
                        WorkOrderEntry  : string option
                        ActivityEntry   : string option
                        Status          : string
                    }
                
                let private attributionToString =
                    function
                        | Attribution.WorkOrder _     -> "workorder"
                        | Attribution.Activity  _     -> "activity"

                let private updateAttribution attribution timeRecord = 
                    match attribution with
                        | Attribution.WorkOrder wo ->
                            { timeRecord with WorkOrderEntry = wo.ToString() |> Some }
                        | Attribution.Activity act ->
                            { timeRecord with ActivityEntry = act.ToString() |> Some }

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
                    let (NbPeople nb) = time.NbPeople

                    {   
                                    Site            = site
                                    ShopFloor       = shopfloor 
                                    WorkCenter      = workcenter 
                                    TimeType        = timetype
                                    StartTime       = time.Duration.StartTime
                                    EndTime         = time.Duration.EndTime
                                    TimeHr          = t
                                    NbPeople        = nb
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
                    (time: DBTimeRecord) =
                        let siteRes = Site.validate sites time.Site
                        let shopFloorRes = ShopFloor.validate shopfloors time.ShopFloor
                        
                        let workCenterRes = 
                            time.WorkCenter 
                            |> Option.map (WorkCenter.validate workcenters)
                            |> switchResOpt
                        
                        let timeTypeRes = TimeType.validate time.TimeType
                        let durationRes = Duration.validate time.StartTime time.EndTime
                        let nbPeopleRes = NbPeople.validate time.NbPeople
                        let statusRes   = RecordStatus.validate time.Status


                        match time.WorkOrderEntry, time.ActivityEntry with
                            | Some wo, None -> 
                                let workOrderEntryRes = WorkOrder.validate workorders wo
                                let attributionRes = Result.map (Attribution.WorkOrder) workOrderEntryRes
                                TimeRecord.create
                                <!> siteRes
                                <*> shopFloorRes
                                <*> workCenterRes
                                <*> attributionRes
                                <*> timeTypeRes
                                <*> durationRes
                                <*> nbPeopleRes
                                <*> statusRes
                                
                            | None, Some act -> 
                                let activityEntryRes = ActivityCode.validate activities act
                                let attributionRes   = Result.map (Attribution.Activity) activityEntryRes
                                TimeRecord.create
                                <!> siteRes
                                <*> shopFloorRes
                                <*> workCenterRes
                                <*> attributionRes
                                <*> timeTypeRes
                                <*> durationRes
                                <*> nbPeopleRes
                                <*> statusRes

                            | Some wo, Some ev -> Failure "Both Workorder and Event entry are set."
                            | None, None       -> Failure "Both Workorder and Event entry are missing."
