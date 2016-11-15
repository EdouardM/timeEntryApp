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

    [<Measure>] type kg

    type WeightProduced = WeightProduced of float<kg>

    let createMeasure ty name (x:float<_>) =
        function
            | x when x < 0.<_> -> Failure <| sprintf "%s can't be negative. Input: %.2f" name x
            | x -> Success (ty x)

    let createWeightProduced = createMeasure WeightProduced "Weight" 

    //Batch for Semi finished products
    [<Measure>] type batch

    //Consumer Units for finished products
    [<Measure>] type CU

    type QuantityProduced = 
        | QuantitySF of float<batch> 
        | QuantityPF of int<CU> 

    type WorkOrderEntry =
        {
            WorkOrder           : WorkOrder
            ItemCode            : ItemCode
            WeightProduced      : WeightProduced
            //QuantityProduced    : QuantityProduced
        }
    let newWorkOrderEntry workOrder itemCode weight quantity = 
        { WorkOrder = workOrder; ItemCode = itemCode; WeightProduced = weight}


    type Machine = Machine of string

    let createMachine = create Machine "machine"

    //In case of BreakDown we record addiional information
    type BreakDownInfo = 
        { 
            Machine     : Machine 
            Cause       : string
            Solution    : string
            Comments    : string 
        }   
    let createBreakDownInfo machine cause solution comments = 
        { Machine = machine; Cause = cause; Solution = solution; Comments = comments}

    //Only some event allow Zero People => MachineOnly TimeType
    type AllowZeroPeople = Yes | No
    //Only some event allow Additional info: breakdowns
    type HasEventInfo    = Yes | No 
    
    type Event = 
        { 
            Event           : string
            AllowZeroPeople : AllowZeroPeople
            HasEventInfo    : HasEventInfo 
        }

    type EventEntry = { Event: Event; EventInfo: BreakDownInfo option}

    type TimeAllocation = 
        //productive time record against work Order
        | WorkOrderEntry of WorkOrderEntry
        //improductive time recorded against event
        | EventEntry of EventEntry

    //Domain model (pure)
    type TimeRecord =
        {
            Site        : Site
            ShopFloor   : ShopFloor
            WorkCenter  : WorkCenter
            TimeEntry   : TimeEntry
            Allocation  : TimeAllocation
            Status      : RecordStatus
        }
    
    let createTimeRecord site shopfloor workcenter allocation timeEntry =
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
                        Site = time.Site; 
                        ShopFloor = time.ShopFloor; 
                        WorkCenter = time.WorkCenter; 
                        TimeType = MachineTime; Duration = duration; 
                        NbPeople = NbPeople 0. } ]
                //List of two records: Machine & Labour time
                | MachineAndLabour (duration, nb) -> 
                    [ { 
                        Site = time.Site; 
                        ShopFloor = time.ShopFloor; 
                        WorkCenter = time.WorkCenter;
                        TimeType = MachineTime; 
                        Duration = duration; 
                        NbPeople = NbPeople 0. } ;

                    {   Site = time.Site; 
                        ShopFloor = time.ShopFloor;
                        WorkCenter = time.WorkCenter; 
                        TimeType = LabourTime ; 
                        Duration = duration;
                        NbPeople = nb } ]
                //List of one record: Labour time
                | LabourOnly (duration, nb) -> 
                    [ {   
                        Site = time.Site; 
                        ShopFloor = time.ShopFloor;
                        WorkCenter = time.WorkCenter; 
                        TimeType = LabourTime; 
                        Duration = duration; 
                        NbPeople = nb } ]

    module DTO =
        ///Domain to store data deserialized from JSON format
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
        
        let fromDTO 
            validSites
            validShopFloors
            validWorkCenters
            (dto: TimeRecordDTO) =
                let siteResult = createSite validSites dto.Site
                let shopfloorResult  = createShopfloor validShopFloors dto.ShopFloor
                let workcenterResult = createWorkCenter validWorkCenters dto.WorkCenter
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