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


    ///Create a valid number of people:
    ///Number of people can only be integer or half of integer and positive
    let createNbPeople nb = 
        if nb >= 0. then
            let r = floor nb
            let d = nb - r
            if d >= 0.5 then Success (NbPeople (r + 0.5))
            else Success(NbPeople r)
        else Failure <| sprintf "Number of people can't be negative.\nNb people: %.2f" nb

    let createSite = create DomainTypes.Site "site"

    let createShopfloor = create ShopFloor "shopfloor"

    
    let createWorkCenter = create WorkCenter "workcenter"

    let createHour = 
        function
            | h when h < 0  -> Failure "Hour must be positive."
            | h when h > 23 -> Failure "Hour can't superior to 23"
            | h -> Success (Hour h)

    let createWorkCenterInfo site shopFloor workCenter startTime endTime = 
        { Site = site; WorkCenter = workCenter; ShopFloor = shopFloor; StartHour = startTime; EndHour = endTime }

    
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
            | MachineTime, (NbPeople 0.)  -> MachineOnly duration
            | LabourTime,  nbPeople       -> LabourOnly (duration, nbPeople)
            | MachineTime, nbPeople       -> MachineAndLabour (duration, nbPeople) 

    let createWorkOrder = create WorkOrder "work order number"

    let createItemCode = create ItemCode "item code"
    let createItemType = create ItemCode "item type"

    let createWeight =
            function
            | w when w < 0.  -> Failure "Weight must be positive."
            | w -> Success (Weight w)

    let createWeightWithUnit weight = 
        function 
            | "KG" -> Kg <!> createWeight weight 
            | "GR" -> Gr <!> createWeight weight
            | _  -> Failure "Undefined unit of measure for weight."

    let createWorkOrderStatus =
        function
            | "open"    ->  Success Open
            | "closed"  ->  Success Closed
            | _         ->  Failure "Invalid workorder status."


    let createWorkOrderEntry workOrder itemCode weight status = 
        { WorkOrder = workOrder; ItemCode = itemCode; Weight = weight; Status = status}

    let createMachine = create Machine "machine"

    let createEventInfo machine cause solution comments = 
        { Machine = machine; Cause = cause; Solution = solution; Comments = comments}
    let createEvent event hasInfo allowZeropeople =
        match hasInfo, allowZeropeople with
            | false, true       -> Success (ZeroPerson event)
            | true, false       -> Success (WithInfo event)
            | false, false      -> Success (WithoutInfo event) 
            | true, true        -> Failure "Event with zero person can't carry information."

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
