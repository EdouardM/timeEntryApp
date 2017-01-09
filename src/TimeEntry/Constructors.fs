namespace TimeEntry

module Constructors =
    open System
    open TimeEntry.Result
    open TimeEntry.DomainTypes

    ///Helper function to create domain types values validated against a list of valid input
    let create ty name validList id = 
        match List.exists (fun s -> s = id ) validList with
            | true -> Success (ty id)
            | false -> Failure <| sprintf "Can't find %s: %s" name id 

    (* SITE CONSTRUCTORS / VALIDATION *)
    let createSite = create DomainTypes.Site "site"

    (* SHOPFLOOR CONSTRUCTORS *)
    let createShopFloor = create ShopFloor "shopfloor"


    let createShopFloorInfo site shopfloor = 
        { ShopFloorInfo.Site = site; ShopFloorInfo.ShopFloor = shopfloor}


    (* WORKCENTER CONSTRUCTORS *)
    let createWorkCenter = create WorkCenter "workcenter"
    let createHour = 
        function
            | h when h > 23u -> Failure "Hour can't be bigger than 23."
            | h -> Success (Hour h)
    
    let createWorkCenterInfo shopFloor workCenter startTime endTime = 
        { WorkCenter = workCenter; ShopFloorInfo = shopFloor; StartHour = startTime; EndHour = endTime }
    
    (* MACHINE CONSTRUCTORS *)
    let createMachine = create Machine "machine"

    let createMachineInfo machine shopFloor =
        { Machine = machine; ShopFloorInfo = shopFloor}
    
    (* ACTIVITY CONSTRUCTORS *)
    let createShopFloorAccess accessAll authorizedShopFloor =
        match accessAll, authorizedShopFloor with
            | true, []      -> Success (AllShopFloors)
            | false, []     -> Failure "Must have at least one authorized shopfloor when access is set to shopfloor list."
            | false, sf     -> Success (ShopFloorList sf)
            | true, sf      -> Failure "Should have empty list of shopfloors when access is set to all."
    

    let createWorkCenterAccess accessAll authorizedWorkCenter =
        match accessAll, authorizedWorkCenter with
            | true, []      -> Success (AllWorkCenters)
            | false, []     -> Failure "Must have at least one authorized workcenter when access is set to workcenter list."
            | false, wc     -> Success (WorkCenterList wc)
            | true, wc      -> Failure "Should have empty list of workcenter when access is set to all."
    
    let createTimeType = 
        function
            | "labour"      -> Success (LabourTime)
            | "machine"     -> Success (MachineTime)
            | ty            -> Failure <| sprintf "Invalid time type: %s" ty

    let createActivityCode = create ActivityCode "activity"

    let createActivity site code level timetype link =
        { Site = site; RecordLevel = level; Code = code; TimeType = timetype; ActivityLink = link }
    
    let createActivityLink (createActivityCode: string -> Result<ActivityCode>) islinked linkedActivity = 
        match islinked, linkedActivity with
            | true, Some act    -> (createActivityCode act |> Result.map Linked )
            | false,    None    -> Success NotLinked
            | false, Some act   -> Failure "One activity marked as not linked cannot have a linked activity."
            | true,     None    -> Failure "One activity marked as linked must have a linked activity."


    let createActivityDetails machine cause solution comments = 
        { Machine = machine; Cause = cause; Solution = solution; Comments = comments}

    let createActivityInfo activity hasInfo details =
        match hasInfo, details  with
            | true, Some d      -> Success (Detailed (activity,d))
            | false,None        -> Success (Normal activity)
            | false,Some d      -> Failure "Activity details are not expected."
            | true, None        -> Failure "Activity details are expected."

    (* USER CONSTRUCTORS *)
    let createUserName = create UserName "user name"

    let createLogin = create Login "user login"

    let createSiteAccess accessAll authorizedSites = 
        match accessAll, authorizedSites with
            | true, []      -> Success (AllSites)
            | false, []     -> Failure "User must have at least one authorized site when access is set to Site List."
            | false, sites  -> Success (SiteList sites)
            | true, l       -> Failure "Should have empty list of sites when access is set to AllSites."
    
    let createLevel = 
        function
            | "user"    -> Success User
            | "keyuser" -> Success KeyUser
            | "admin"   -> Success Admin
            | level     -> Failure <| sprintf "Unexpected value for authorization level: %s" level

    let createUser login name access level = 
        { Login = login ; Name = name; SiteAccess = access; Level = level}

    (* WORKORDER CONSTRUCTORS *)
    let createWorkOrder = create WorkOrder "work order number"

    let createItemCode = create ItemCode "item code"

    let createWorkOrderStatus =
        function
            | "open"    ->  Success Open
            | "closed"  ->  Success Closed
            | _         ->  Failure "Invalid workorder status."

    let createTimeHr = 
        function
            | h when h < 0.f        -> Failure "Total hour can't be negative."
            | h when h > 999.9999f  -> Failure "Total hour can't be bigger than 999.9999 hr."
            | h -> Success (TimeHr h)

    let createWorkOrderInfo workOrder workCenter itemCode totalMachine totalLabour status = 
        { 
            WorkOrder = workOrder; 
            WorkCenter = workCenter; 
            ItemCode = itemCode; 
            TotalMachineTimeHr = totalMachine; 
            TotalLabourTimeHr = totalLabour; 
            Status = status
        }    

    (* TIME RECORD CONSTRUCTOS*)

    let createDuration (startTime: DateTime) (endTime: DateTime) =
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


    ///Create a valid number of people:
    ///Number of people can only be integer or half of integer and positive
    let createNbPeople nb = 
        if nb >= 0.f then
            let r = floor nb
            let d = nb - r
            if d >= 0.5f then Success (NbPeople (r + 0.5f))
            else Success(NbPeople r)
        else Failure <| sprintf "Number of people can't be negative.\nNb people: %.2f" nb

    let createRecordStatus = 
        function
            | "entered"   -> Success Entered
            | "validated" -> Success Validated
            | status      -> Failure <| sprintf "Invalid Record Status: %s" status 

    let createTimeRecord  site shopfloor workcenter attribution timeEntry status =
        { Site = site; ShopFloor = shopfloor; WorkCenter = workcenter; TimeEntryMode = timeEntry; Attribution = attribution; Status = status}


    (* EVENT CONSTRUCTORS*)
    let createEventInfo machine cause solution comments = 
        { Machine = machine; Cause = cause; Solution = solution; Comments = comments}

    let createEvent event hasInfo allowZeropeople =
        match hasInfo, allowZeropeople with
            | false, true       -> Success (ZeroPerson event)
            | true, false       -> Success (WithInfo event)
            | false, false      -> Success (WithoutInfo event) 
            | true, true        -> Failure "Event with zero person can't carry information."

    let extractEvent = function 
            | WithInfo ev -> ev
            | ZeroPerson ev -> ev
            | WithoutInfo ev -> ev

    let extractEventfromEntry = function
            | EventWithInfo (ev, info) -> ev
            | EventWithoutInfo ev -> ev
            | EventZeroPerson ev -> ev

    let createEventEntry (event:Event) (eventInfo: EventInfo option) =
        match event, eventInfo with
            | WithoutInfo ev, None        -> Success <| EventWithoutInfo event
            | WithInfo ev, Some info      -> Success <| EventWithInfo (event, info)
            | ZeroPerson ev, None         -> Success <| EventZeroPerson event
            | ZeroPerson ev, Some info    -> Failure "info not expected"
            | WithoutInfo ev, Some info   -> Failure "info not expected"
            | WithInfo ev, None           -> Failure "expecting information"