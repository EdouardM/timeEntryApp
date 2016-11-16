namespace TimeEntry
module DomainTypes =
    open System
    open Helpers
    
    //Domain Model:
    type TimeType = 
            | MachineTime
            | LabourTime

    type RecordStatus = | Entered | Validated

    type NbPeople = NbPeople of float
    

    ///Create a valid number of people:
    ///Number of people can only be integer or half of integer and positive
    let createNbPeople nb = 
        if nb >= 0. then
            let r = floor nb
            let d = nb - r
            if d >= 0.5 then Success (NbPeople (r + 0.5))
            else Success(NbPeople r)
        else Failure <| sprintf "Number of people can't be negative.\nNb people: %.2f" nb

    type Site  = Site of string
    
    ///Helper function to create domain types values validated against a list of valid input
    let create ty name validList id = 
        match List.exists (fun s -> s = id ) validList with
            | true -> Success (ty id)
            | false -> Failure <| sprintf "Can't find %s: %s" name id 

    let createSite = create Site "site"

    type ShopFloor  = ShopFloor of string

    let createShopfloor = create ShopFloor "shopfloor"

    type WorkCenter  = WorkCenter of string

    let createWorkCenter = create WorkCenter "workcenter"

    type Hour = Hour of int

    let createHour = 
        function
            | h when h < 0  -> Failure "Hour must be positive."
            | h when h > 23 -> Failure "Hour can't superior to 23"
            | h -> Success (Hour h)

    type WorkCenterInfo = 
        {
            Site        : Site
            WorkCenter  : WorkCenter
            ShopFloor   : ShopFloor
            StartHour   : Hour
            EndHour     : Hour
        }
    let createWorkCenterInfo site shopFloor workCenter startTime endTime = 
        { Site = site; WorkCenter = workCenter; ShopFloor = shopFloor; StartHour = startTime; EndHour = endTime }

    type Duration = 
        {
            StartTime   : DateTime
            EndTime     : DateTime
        }
        with
            member x.Duration = x.EndTime - x.StartTime 

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

    type TimeEntry = 
        | MachineOnly of Duration
        | MachineAndLabour of Duration * NbPeople
        | LabourOnly of Duration * NbPeople

    let createTimeEntry timeType nbPeople duration =
        match timeType, nbPeople with
            | MachineTime, (NbPeople 0.)  -> MachineOnly duration
            | LabourTime,  nbPeople       -> LabourOnly (duration, nbPeople)
            | MachineTime, nbPeople       -> MachineAndLabour (duration, nbPeople) 

    type WorkOrder = WorkOrder of string
    let createWorkOrder = create WorkOrder "work order number"

    type ItemCode  = ItemCode of string
    let createItemCode = create ItemCode "item code"

    type ItemType  = ItemType of string
    
    let createItemType = create ItemCode "item type"

    type Weight = Weight of float

    let createWeight =
            function
            | w when w < 0.  -> Failure "Weight must be positive."
            | w -> Success (Weight w)

    type WorkOrderEntry =
        {
            WorkOrder       : WorkOrder
            ItemCode        : ItemCode
            Weight          : Weight
        }
    let createWorkOrderEntry workOrder itemCode weight = 
        { WorkOrder = workOrder; ItemCode = itemCode; Weight = weight}


    type Machine = Machine of string

    let createMachine = create Machine "machine"

    //In case of BreakDown we record addiional information
    type EventInfo = 
        { 
            Machine     : Machine 
            Cause       : string
            Solution    : string
            Comments    : string 
        }   
    let createEventInfo machine cause solution comments = 
        { Machine = machine; Cause = cause; Solution = solution; Comments = comments}
     
    type Event = 
        | ZeroPerson    of string
        | WithInfo      of string
        | WithoutInfo   of string

    let createEvent event hasInfo allowZeropeople =
        match hasInfo, allowZeropeople with
            | false, true       -> Success (ZeroPerson event)
            | true, false       -> Success (WithInfo event)
            | false, false      -> Success (WithoutInfo event) 
            | true, true        -> Failure "Event with zero person can't carry information."
    
    type EventEntry = 
        | EventWithInfo         of Event * EventInfo 
        | EventWithoutInfo      of Event
        | EventZeroPerson       of Event 
    let createEventEntry (event:Event) (eventInfo: EventInfo) =
        match event with
            | WithoutInfo ev        -> Success <| EventWithoutInfo event
            | WithInfo ev           -> Success <| EventWithInfo (event, eventInfo)
            | ZeroPerson ev         -> Success <| EventZeroPerson event
            
    type TimeAllocation = 
        //productive time record against work Order
        | WorkOrderEntry of WorkOrderEntry
        //improductive time recorded against event
        | EventEntry of EventEntry

    let createTimeAllocation allocation = 
        match allocation with
            | WorkOrderEntry w -> Success (WorkOrderEntry w)
            | EventEntry e -> Success (EventEntry e)


    //Domain model (pure)
    type TimeRecord =
        {
            Site            : Site
            ShopFloor       : ShopFloor
            WorkCenter      : WorkCenter
            TimeEntry       : TimeEntry
            Allocation      : TimeAllocation
            Status          : RecordStatus
        }
    
    let createTimeRecord  site shopfloor workcenter allocation timeEntry =
        { Site = site; ShopFloor = shopfloor; WorkCenter = workcenter; TimeEntry = timeEntry; Allocation = allocation; Status = Entered}

    (* 
        Types for User information
    *)

    type UserName = Login of string

    type AuthLevel = | Entry | Maintenance | Admin

    type SiteAccess = | All | Site of Site


    type User =
        {
            Site        : SiteAccess
            Name        : UserName
            Level       : AuthLevel
        }

    // Use Types
    type EntryRequest = { User: User; Entry : TimeRecord }

    type EntryResponse = { Id: int; Request: EntryRequest}

    //Use Case 1: Entry of time
    // Db failure or JSon failure
    type AddEntryData = EntryRequest -> Result<EntryResponse>


    module DataBase =
        //DataBase model (pure)
        type DBTimeRecord =
            {
                Site        : Site
                ShopFloor   : ShopFloor
                WorkCenter  : WorkCenter
                TimeType    : TimeType
                Duration    : Duration
                NbPeople    : NbPeople
            }

        ///Function to convert one Domain Record into records to insert into Database
        let toDB  (time : TimeRecord) = 
            match time.TimeEntry with
                //List of one record: Machine time
                | MachineOnly duration -> 
                    [ {   
                        Site        = time.Site 
                        ShopFloor   = time.ShopFloor 
                        WorkCenter  = time.WorkCenter 
                        TimeType    = MachineTime
                        Duration    = duration
                        NbPeople    = NbPeople 0. } ]
                //List of two records: Machine & Labour time
                | MachineAndLabour (duration, nb) -> 
                    [ { 
                        Site        = time.Site
                        ShopFloor   = time.ShopFloor 
                        WorkCenter  = time.WorkCenter
                        TimeType    = MachineTime 
                        Duration    = duration
                        NbPeople    = NbPeople 0. } ;

                        { 
                        Site        = time.Site
                        ShopFloor   = time.ShopFloor 
                        WorkCenter  = time.WorkCenter
                        TimeType    = MachineTime 
                        Duration    = duration
                        NbPeople    = nb }]
                //List of one record: Labour time
                | LabourOnly (duration, nb) -> 
                    [ {   
                        Site        = time.Site 
                        ShopFloor   = time.ShopFloor
                        WorkCenter  = time.WorkCenter
                        TimeType    = LabourTime
                        Duration    = duration 
                        NbPeople    = nb } ]

    module DTO =
        ///Domain to store data deserialized from JSON format
        type WorkCenterInfoDTO =
            {
                Site        : string
                ShopFloor   : string
                WorkCenter  : string
                StartHour   : int
                EndHour     : int

            }
        
        let workCenterInfoFromDTO 
            validSites
            validShopFloors
            validWorkCenters
            (dto: WorkCenterInfoDTO) =
                let siteResult = createSite validSites dto.Site
                let shopfloorResult  = createShopfloor validShopFloors dto.ShopFloor
                let workcenterResult = createWorkCenter validWorkCenters dto.WorkCenter
                let startHourResult  = createHour dto.StartHour
                let endHourResult    = createHour dto.EndHour
                createWorkCenterInfo 
                <!> siteResult 
                <*> shopfloorResult 
                <*> workcenterResult 
                <*> startHourResult 
                <*> endHourResult
        
       
        type WorkOrderDTO =
            {
                WorkOrder       : string
                ItemCode        : string
                Weight          : float
            }
        let workOrderFromDTO 
            validWorkOrders
            validItemCodes
            (dto: WorkOrderDTO) =
                let workorderResult  = createWorkOrder validWorkOrders dto.WorkOrder
                let itemCodeResult   = createItemCode validItemCodes dto.ItemCode
                let weightResult     = createWeight dto.Weight
                createWorkOrderEntry
                <!> workorderResult
                <*> itemCodeResult 
                <*> weightResult 

        type TimeRecordDTO =
            {
                Site        : string
                ShopFloor   : string
                WorkCenter  : string
                TimeType    : string
                StartTime   : DateTime
                EndTime     : DateTime
                NbPeople    : float
            }
        
        let timeRecordFromDTO 
            validSites
            validShopFloors
            validWorkCenters
            (dto: TimeRecordDTO) =
                let siteResult = createSite validSites dto.Site
                let shopfloorResult  = createShopfloor validShopFloors dto.ShopFloor
                let workcenterResult = createWorkCenter validWorkCenters dto.WorkCenter
                let workcenterInfoResult = createWorkCenterInfo <!> siteResult <*> shopfloorResult <*> workcenterResult
                let timetypeResult = 
                    match dto.TimeType with
                        | "Machine" -> Success MachineTime
                        | "Labour"  -> Success LabourTime
                        | ty         -> Failure <| sprintf "Wrong time type: %s" ty

                let durationResult = createDuration dto.StartTime dto.EndTime
                let nbPeopleResult = createNbPeople dto.NbPeople

                let timeEntryResult = createTimeEntry <!> timetypeResult <*> nbPeopleResult <*> durationResult
                let timeRecordResult = createTimeRecord <!> siteResult <*> shopfloorResult <*> workcenterResult <*> timeEntryResult
                timeRecordResult