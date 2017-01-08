namespace TimeEntry

module Constructors =
    open System
    open TimeEntry.Result
    open TimeEntry.DomainTypes
    open TimeEntry.ConstrainedTypes

    ///Helper function to create domain types values validated against a list of valid input
    let create ctor name validList = 
        (fun id -> 
            match List.exists (fun s -> s = id ) validList with
            | true -> Success (id)
            | false -> Failure <| sprintf "Can't find %s: %s" name id)
        >> bind ctor

    ///Create a valid number of people:
    ///Number of people can only be integer or half of integer and positive
    let createNbPeople nb = 
        if nb >= 0.f then
            let r = floor nb
            let d = nb - r
            if d >= 0.5f then Success (NbPeople (r + 0.5f))
            else Success(NbPeople r)
        else Failure <| sprintf "Number of people can't be negative.\nNb people: %.2f" nb

    let createSite = create (stringMax4 >> Result.map DomainTypes.Site) "site"

    let createShopfloor = create (string5 >> Result.map ShopFloor) "shopfloor"

    let createShopfloorInfo site shopfloor = 
        { ShopFloorInfo.Site = site; ShopFloorInfo.ShopFloor = shopfloor}

    let createWorkCenter = create (string4 >> Result.map WorkCenter) "workcenter"

    let createHour = 
        function
            | h when h > 23u -> Failure "Hour can't be bigger than 23."
            | h -> Success (Hour h)

    let createTimeHr = 
        function
            | h when h < 0.f        -> Failure "Total hour can't be negative."
            | h when h > 999.9999f  -> Failure "Total hour can't be bigger than 999.9999 hr."
            | h -> Success (TimeHr h)

    let createWorkCenterInfo shopFloor workCenter startTime endTime = 
        { WorkCenter = workCenter; ShopFloorInfo = shopFloor; StartHour = startTime; EndHour = endTime }
    
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

    let createTimeEntry timeType nbPeople duration =
        match timeType, nbPeople with
            | MachineTime, (NbPeople 0.f)  -> MachineOnly duration
            | LabourTime,  nbPeople       -> LabourOnly (duration, nbPeople)
            | MachineTime, nbPeople       -> MachineAndLabour (duration, nbPeople) 

    let createWorkOrder = create (string10 >> Result.map WorkOrder) "work order number"

    let createItemCode = create (string6 >> Result.map ItemCode) "item code"
    let createItemType = create (string5 >> Result.map ItemType) "item type"

    let createWorkOrderStatus =
        function
            | "open"    ->  Success Open
            | "closed"  ->  Success Closed
            | _         ->  Failure "Invalid workorder status."

    let createWorkOrderEntry workOrder workCenter itemCode totalMachine totalLabour status = 
        { 
            WorkOrder = workOrder; 
            WorkCenter = workCenter; 
            ItemCode = itemCode; 
            TotalMachineTimeHr = totalMachine; 
            TotalLabourTimeHr = totalLabour; 
            Status = status
        }

    let createMachine = create (string10 >> Result.map Machine) "machine"

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


    let createTimeRecord  site shopfloor workcenter allocation timeEntry =
        { Site = site; ShopFloor = shopfloor; WorkCenter = workcenter; TimeEntry = timeEntry; Allocation = allocation; Status = Entered}


    let createTimeType = 
        function
            | "machine" -> Success MachineTime
            | "labour"  -> Success LabourTime
            | _ -> Failure "Invalid Time Type"
