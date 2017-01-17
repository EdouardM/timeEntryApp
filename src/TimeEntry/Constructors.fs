namespace TimeEntry

module Constructors =
    open System
    open TimeEntry.Result
    open TimeEntry.ConstrainedTypes
    open TimeEntry.DomainTypes

    ///Helper function to create domain types values validated against a list of valid input
    let validate ctor name validList = 
        (fun id -> 
            match List.exists (fun s -> s = id ) validList with
                | true -> Success (id)
                | false -> Failure <| sprintf "Can't find %s: %s" name id)
        >=> ctor

    let create ctor name validList = 
        (fun id -> 
            match List.exists (fun s -> s = id ) validList with
                | false -> Success (id)
                | true  -> Failure <| sprintf "The value exists. Can't create %s: %s" name id)
        >=> ctor

    (* SITE CONSTRUCTORS / VALIDATION *)
    
    let createSite = create ( stringExact3      >>= DomainTypes.Site)  "site"
    let validateSite = validate ( stringExact3  >>= DomainTypes.Site)  "site"

    (* SHOPFLOOR CONSTRUCTORS *)
    let createShopFloor = create ( stringExact5 >>= ShopFloor)  "shopfloor"
    let validateShopFloor = validate (stringExact5 >>= ShopFloor) "shopfloor"


    let createShopFloorInfo site shopfloor = 
        { ShopFloorInfo.Site = site; ShopFloorInfo.ShopFloor = shopfloor}


    (* WORKCENTER CONSTRUCTORS *)
    let validateWorkCenter = validate (stringMax5 >>= WorkCenter) "workcenter"
    let validateHour = 
        function
            | h when h > 23u -> Failure "Hour can't be bigger than 23."
            | h -> Success (Hour h)
    
    let createWorkCenterInfo shopFloor workCenter startTime endTime = 
        { WorkCenter = workCenter; ShopFloorInfo = shopFloor; StartHour = startTime; EndHour = endTime }
    
    (* MACHINE CONSTRUCTORS *)
    let validateMachine = validate (stringMax10 >>= Machine) "machine"

    let createMachineInfo machine shopFloor =
        { Machine = machine; ShopFloorInfo = shopFloor}
    
    (* ACTIVITY CONSTRUCTORS *)
    let validateShopFloorAccess accessAll authorizedShopFloor =
        match accessAll, authorizedShopFloor with
            | true, []      -> Success (AllShopFloors)
            | false, []     -> Failure "Must have at least one authorized shopfloor when access is set to shopfloor list."
            | false, sf     -> Success (ShopFloorList sf)
            | true, sf      -> Failure "Should have empty list of shopfloors when access is set to all."
    

    let validateWorkCenterAccess accessAll authorizedWorkCenter =
        match accessAll, authorizedWorkCenter with
            | true, []      -> Success (AllWorkCenters)
            | false, []     -> Failure "Must have at least one authorized workcenter when access is set to workcenter list."
            | false, wc     -> Success (WorkCenterList wc)
            | true, wc      -> Failure "Should have empty list of workcenter when access is set to all."
    
    let validateTimeType = 
        function
            | "labour"      -> Success (LabourTime)
            | "machine"     -> Success (MachineTime)
            | ty            -> Failure <| sprintf "Invalid time type: %s" ty
    let validateExtraInfo = 
        function 
            | "withinfo"    -> Success (ExtraInfo.WithInfo)
            | "withoutinfo" -> Success (ExtraInfo.WithoutInfo)
            | extra         -> Failure <| sprintf "Invalid extra info: %s" extra

    let validateActivityCode = validate (stringMax4 >>= ActivityCode) "activity"

    let validateActivity site code level timetype link extrainfo =
        { Site = site; RecordLevel = level; Code = code; TimeType = timetype; ActivityLink = link; ExtraInfo = extrainfo }
    
    let validateActivityLink (validateActivityCode: string -> Result<ActivityCode>) islinked linkedActivity = 
        match islinked, linkedActivity with
            | true, Some act    -> (validateActivityCode act |> Result.map Linked )
            | false,    None    -> Success NotLinked
            | false, Some act   -> Failure "One activity marked as not linked cannot have a linked activity."
            | true,     None    -> Failure "One activity marked as linked must have a linked activity."


    let validateCause    = stringMax50
    let validateSolution = stringMax50
    let validateComments = stringMax200

    let validateActivityDetails machine cause solution comments = 
        { ActivityDetails.Machine = machine; Cause = cause; Solution = solution; Comments = comments}

    let validateActivityEntry activity details =
        match details  with
            | Some d      -> Success (Detailed (activity,d))
            | None        -> Success (Normal activity)
            
    (* USER CONSTRUCTORS *)
    let validateUserName = validate (stringMax50 >>= UserName) "user name"

    let validateLogin = validate (stringMax8 >>= Login) "user login"

    let validatePassword = (stringMax50 >>= Password)


    let validateSiteAccess accessAll authorizedSites = 
        match accessAll, authorizedSites with
            | true, []      -> Success (AllSites)
            | false, []     -> Failure "User must have at least one authorized site when access is set to Site List."
            | false, sites  -> Success (SiteList sites)
            | true, l       -> Failure "Should have empty list of sites when access is set to AllSites."
    
    let validateAuthLevel = 
        function
            | "user"    -> Success User
            | "keyuser" -> Success KeyUser
            | "admin"   -> Success Admin
            | level     -> Failure <| sprintf "Unexpected value for authorization level: %s" level

    let validateUser login name password access level = 
        { Login = login ; Name = name; Password = password; SiteAccess = access; Level = level}

    (* WORKORDER CONSTRUCTORS *)
    let validateWorkOrder = validate (stringExact10 >>= WorkOrder) "work order number"

    let validateItemCode = validate (stringMax6 >>= ItemCode) "item code"

    let validateWorkOrderStatus =
        function
            | "open"    ->  Success Open
            | "closed"  ->  Success Closed
            | _         ->  Failure "Invalid workorder status."

    let validateTimeHr = 
        function
            | h when h < 0.f        -> Failure "Total hour can't be negative."
            | h when h > 999.9999f  -> Failure "Total hour can't be bigger than 999.9999 hr."
            | h -> Success (TimeHr h)

    let validateWorkOrderInfo workOrder workCenter itemCode totalMachine totalLabour status = 
        { 
            WorkOrder = workOrder; 
            WorkCenter = workCenter; 
            ItemCode = itemCode; 
            TotalMachineTimeHr = totalMachine; 
            TotalLabourTimeHr = totalLabour; 
            Status = status
        }    

    (* TIME RECORD CONSTRUCTOS*)

    let validateDuration (startTime: DateTime) (endTime: DateTime) =
        if     startTime.Year >= 2010 
            && startTime.Year <= 2100 
            && endTime.Year >= 2010
            && endTime.Year <= 2100
        then
            if startTime <= endTime 
            then 
                Success { StartTime = startTime; EndTime = endTime } 
            else 
                Failure <| sprintf "Start time must be before End time.\nStart time: %A\tEnd time: %A" startTime endTime
        else
            Failure <| sprintf "Date year must be within the range from 2010 to 2100.\nStart Year: %d\End Year: %d" startTime.Year endTime.Year


    ///Validate a number of people:
    ///Number of people can only be integer or half of integer and positive
    let validateNbPeople nb = 
        if nb >= 0.f then
            let r = floor nb
            let d = nb - r
            if d >= 0.5f then Success (NbPeople (r + 0.5f))
            else Success(NbPeople r)
        else Failure <| sprintf "Number of people can't be negative.\nNb people: %.2f" nb

    let validateRecordStatus = 
        function
            | "entered"   -> Success Entered
            | "validated" -> Success Validated
            | status      -> Failure <| sprintf "Invalid Record Status: %s" status 

    let validateTimeRecord  site shopfloor workcenter attribution timetype duration status =
        { Site = site; ShopFloor = shopfloor; WorkCenter = workcenter; TimeType = timetype;Duration = duration; Attribution = attribution; Status = status}
